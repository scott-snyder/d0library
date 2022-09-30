      SUBROUTINE CLUDIG
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS :    Build the trd detected clusters.
C-                            energy  ----->digital
C-                            Store dE/dT as ZEBRA bank TLYR after
C-                              normalisation and packing
C-                            Compute deposited energy on hit wires
C-   INPUTS  : -TRD clusters (energies and drift time)
C-             -Cluster shapes
C-             -CSWEIGHT.DAT: part of the cluster energy on each of
C-                            the cathode strips hit as a function of
C-                            the cluster position.
C-   OUTPUTS : -TRD hits=dE/dT distribution
C-
C-   CREATED                A. ZYLBERSTEJN
C-   UPDATED  20-OCT-1989   J. SCHWINDLING  :Use of CSWEIGHT.DAT for the
C-                                           energy on the cathodes.
C-   Updated  15-DEC-1989   A. Zylberstejn  Modify call to BKTLYR
C-   Updated   6-MAY-1993   J.P. Cussonneau   
C-                                           Take into account 128 fadc bins 
C-                                           (NFADC -> NMFADC)
C-                                           Add hitos 
C-                                           New cluster shape, one per layer
C----------------------------------------------------------------------
C
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CLUSM.INC/LIST'
      INCLUDE 'D0$INC:CLUSTR.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:ENETRD.INC/LIST'
      INCLUDE 'D0$INC:FADDIG.INC/LIST'
      INCLUDE 'D0$INC:FADCCN.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GEOMTC.INC/LIST'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:NORTRD.INC'
      INCLUDE 'D0$INC:POSIT.INC/LIST'
      INCLUDE 'D0$INC:PRNEVT.INC/LIST'
C      INCLUDE 'D0$INC:TRDLNK.INC/LIST'
      INCLUDE 'D0$INC:TRINFO.INC'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INTEGER IUNIT,IER
C
      LOGICAL FIRST,NEWWIR,NEWCAT(3)
C
      INTEGER ICAT,IDH,ICSWE,IPA,LTLYR,N
      REAL Y1(3),Y2(3),ENEW(3),SIGN,WEIGHT(1:3,1:3,0:49),DS,TEMP
      INTEGER IBINMX,IERR,SHIFT(4)
      REAL YSHAP(NMFADC),VMAX,WGN,PENSEC(3)     
      INTEGER NMWH
      PARAMETER(NMWH =10 )
      INTEGER HITW(NMWH),NWH,ID,IUCOMP
      REAL GAINAN(NMWH)
C   NMWH=MAXIMUM NUMBER OF HIT WIRES,HITW=ARRAY OF HIT WIRES
C   NWH         =NUMBER OF HIT WIRES
      REAL YAUX(NMFADC,NMWH),YMAX(NMWH)
      EQUIVALENCE(WS(1),YAUX(1,1))
C
      INTEGER NMCSH
      PARAMETER (NMCSH=50)
      INTEGER HITCS(NMCSH),NCSH
      REAL CAUX(NMFADC,NMCSH),CMAX(NMCSH)
C
      INTEGER KSTRIP(3),IDC(3)
      INTEGER ISTACK
      REAL DRACSQ,DSC(3),WC(3),WCTOT,WCN(3),WPW
      INTEGER IS,KS
C
      INTEGER I,IAMP,IC,IFOIS,J,JX,K,KM,KX,JMIN
      INTEGER IBIN,IW,IZSTAR,MINFZE,NDD
C      INTEGER NHITS
      REAL XINT,ENG,RN,CC,WMAX
      REAL S0,S1,S2,S3,S4,FACTM
C
      DATA FIRST/.TRUE./
      DATA IFOIS/0/,JMIN/3/
C
C       part of cluster energy seen on cathode strips in each chamber
C
      DATA PENSEC/0.39,0.45,0.37/
C
      IFOIS=IFOIS+1
      IF(FIRST)THEN
        FIRST=.FALSE.
C
        CALL GTUNIT(70,IUNIT,IER)       ! Reserve unit number
C&IF VAXVMS,VAXELN,UFFARM,SIUNIX,ULTRIX,SUNOS,ALFOSF
        OPEN(FILE='TRD_STRIPS',UNIT=IUNIT,STATUS='OLD',READONLY)
C&ENDIF
C&IF ETA10,IBMAIX
C&        OPEN(FILE='TRD_STRIPS',UNIT=IUNIT,STATUS='OLD')
C&ENDIF
        DO 1 ISTACK=1,3
          READ(IUNIT,*)
          DO 2 IS=0,49
            READ(IUNIT,*) (WEIGHT(ISTACK,KS,IS),KS=1,3)
    2     CONTINUE
    1   CONTINUE
        CLOSE(IUNIT)
        CALL RLUNIT(70,IUNIT,IER)       ! RELEASE IT
C        GOTO 901
C
C  900   CONTINUE
C        WRITE(LOUT,*)' PROBLEM_TRD IN CLUDIG: FILE CSWEIGHT.DAT NOT',
C     &  ' FOUND'
C        WRITE(LOUT,*)' CATHODE STRIPS WON''T BE ANALYSED'
C        STRD(2)=1
C
C  901   CONTINUE
        WMAX=2**NBITF-1-ORIG
        FACTM = 80000.
        IBINMX = 2**NBITF - 1
        DO 11 I =  1,  NWORDF
          SHIFT(I)=2**((I-1)*NBITF)
   11   CONTINUE
      ENDIF
      IF(IPRNT.GT.0 .AND. PTRD.GE.9)WRITE(LOUT,*)' ENTRY CLUDIG'
C
C   BUILD THE CLUSTERS
C
      ISTACK = TSTACK
C
      CALL VZERO_i(IHITWN(1),NMWH)
      CALL VZERO(ETOTWI(1),NMWH)
      CALL VZERO(YAUX(1,1),NMFADC)
      HITW(1)=IWIRE(1)
      NWH=0
      YMAX(1)=0.
      CALL VZERO_i(IHITSN(1),NMCSH)
      CALL VZERO(ETOTSN(1),NMCSH)
      CALL VZERO(CAUX(1,1),NMFADC)
      HITCS(1) = ISTRIP(1)
      NCSH = 1
      IHITSN(1) = HITCS(1)
      DRACSQ = DRGRAN(ISTACK)**2
      WPW = 1./DRACSQ
C  LOOP ON THE CLUSTERS
      S0=0.
      DO 300 I=1,NSMEAR
        ID=0
        IF(NWH.NE.0)ID=IUCOMP(IWIRE(I),HITW,NWH)
        NEWWIR=.FALSE.
        IF(ID.LE.0)THEN !DEFINE A NEW HIT WIRE
          IF(NWH.GE.NMWH)GO TO 300
          NWH=NWH+1
          HITW(NWH)=IWIRE(I)
          NEWWIR=.TRUE.
          ID=NWH
          IHITWN(ID)=HITW(NWH)
          IW=IWIRE(I)
          IF(TSTACK.EQ.3)IW=(IW-1)/2+1
          CALL TRGGN('BID',IW,TSTACK,WGN,IERR)!GET ANODE WIRE GAIN
          GAINAN(NWH)=WGN
        ENDIF
C  COMPUTE TIME OF ARRIVAL AND CLUSTER SHAPE
C
        XINT=XCLES(I)
        ENG=ECLES(I)
        S0=S0+ENG
        ETOTWI(ID)=ETOTWI(ID)+ENG
        IC=TIMEC(I)+OFSFAD
        CALL TRDSHP(ISTACK,IC,YSHAP)
C        IF(ENG.GT.5.)THEN
        IF (IPRNT.GT.0 .AND. PTRD.GE.9) THEN
          WRITE(LOUT,*) ' IN CLUDIG.  STACK',ISTACK,' TRACK',ITRA
          WRITE(LOUT,*) ' I',I,'IWIRE ',IWIRE(I),' ID',ID
          WRITE(LOUT,*) '  NWH',NWH,'HITW ',HITW(NWH),' ENG',ENG,' ETOT'
     +           ,ETOTWI(ID)
          WRITE(LOUT,*) ' ISTRIP ',ISTRIP(I),' KSTRIP',KSTRIP
          WRITE(LOUT,*) ' DSTRIP ',DSTRIP(I),'    WCN',WCN
          WRITE(LOUT,*) '  NCSH',NCSH,'HITCS ',HITCS(NCSH)
          WRITE(LOUT,*) ' TIMEC',TIMEC(I),' OFSFAD',OFSFAD
C          WRITE(LOUT,*) ' YSHAP'
C          WRITE(LOUT,'(10G10.4)')YSHAP
        ENDIF
        RN=ENG
        IC=(TIMEC(I)+OFSFAD)/TCLOCK+1. !CONVERT TIME INTO FADC UNIT
        IF(IC.GT.NMFADC)THEN
          WRITE(LOUT,*)' PROBLEM_TRD IN CLUDIG'
          WRITE(LOUT,*)' XCL,YCL,ZCL',XCLES(I),YCLES(I),ZCLES(I)
          WRITE(LOUT,*)'IC',IC,'I,TIMEC(I)',I, TIMEC(I)
          WRITE(LOUT,*)' OFSFAD,TCLOCK',OFSFAD,TCLOCK
          GO TO 300
        ENDIF
        IF(NEWWIR)THEN
          CALL VSCALE(YSHAP,RN,YAUX(1,ID),NMFADC)
        ELSE
          CALL VLINE(YSHAP,RN,YAUX(1,ID),1.,YAUX(1,ID),NMFADC)
        ENDIF
        YMAX(ID)=VMAX(YAUX(1,ID),NMFADC)
C --
        IF(ENG.GT.15. AND.PTRD.GE.8)THEN
          WRITE(LOUT,*)' NEWWIR',NEWWIR,' ENG',ENG,' RN',RN,'  DQ/DT'
          WRITE(LOUT,'(10G10.4)')(YAUX(KX,ID)*RNOR(ISTACK),KX=1,NFADC)
        ENDIF
        IF(YMAX(ID).LE.0.) THEN
          WRITE(LOUT,*)' PROBLEM_TRD IN CLUDIG FOR WIRES'
          WRITE(LOUT,*)' YMAX =',YMAX(ID),' ID',ID,' IC',IC
          WRITE(LOUT,*)' TIMEC(I),OFSFAD',TIMEC(I),OFSFAD
        ENDIF
C
C   FIND CATHODE STRIPS FOR THIS CLUSTER
C
        IF (STRD(2).NE.1.) THEN
          KSTRIP(1) = ISTRIP(I) - 1
          IF (KSTRIP(1).LE.0) KSTRIP(1) = NSTRIP(ISTACK)
          KSTRIP(2) = ISTRIP(I)
          KSTRIP(3) = ISTRIP(I) + 1
          IF (KSTRIP(3).GT.NSTRIP(ISTACK)) KSTRIP(3) = 1
C
          DO 10 KS=1,3
            NEWCAT(KS)=.FALSE.
            IS = IUCOMP(KSTRIP(KS),HITCS,NCSH)
            IF (IS.LE.0) THEN  !NEW CATHODE
              IF (NCSH.GE.NMCSH) THEN
                IF (PTRD.GT.10)WRITE (LOUT,*)
     +           'TOO MANY CATHODE STRIPS HIT'
                IDC(KS)=0
                GO TO 10
              ENDIF
              NCSH = NCSH + 1
              HITCS(NCSH) = KSTRIP(KS)
              CMAX(NCSH)=0.
              IS = NCSH
              IHITSN(IS) = HITCS(NCSH)
              NEWCAT(KS)=.TRUE.
            ENDIF
            IDC(KS) = IS
   10     CONTINUE
C
C   CATHODE SHARING OVER 3 STRIPS
C
          DSC(2) = DSTRIP(I)*WIDCAT(ISTACK)
          DSC(1) = DSC(2) + WIDCAT(ISTACK)
          DSC(3) = DSC(2) - WIDCAT(ISTACK)
C --
          IF (TRHIST) THEN 
           CALL HFILL (6998,DSTRIP(I)*100.,0.,1.)
           CALL HFILL (6999,ENG,0.,1.)
          ENDIF
          DO 18 KS=1,3
            ICSWE=MIN(INT(ABS(DSTRIP(I)*100.)),49)
            ENEW(KS)=WEIGHT(ISTACK,KS,ICSWE)
     >       *ENG*GAINAN(ID)
C --
            IF (TRHIST) THEN 
              CALL HFILL (5900+10*ISTACK+KS,WEIGHT(ISTACK,KS,ICSWE),0.
     &             ,1.)
            ENDIF     
   18     CONTINUE
          IF(DSTRIP(I).LT.0.) THEN
            TEMP=ENEW(3)
            ENEW(3)=ENEW(1)
            ENEW(1)=TEMP
          ENDIF
C
C          DO 20 KS=1,3
C            WC(KS) = 1./(DRACSQ+DSC(KS)**2)
C   20     CONTINUE
C          WCTOT = 2*(WC(1)+WC(2)+WC(3)+WPW)
          DO 30 KS=1,3
C            WCN(KS) = WC(KS)/WCTOT
C  NORMALIZE ENERGY DEPOSIT AND TAKE INTO ACCOUNT THE GAIN OF THE CELL
             IF(IDC(KS).GT.0)ETOTSN(IDC(KS))=ETOTSN(IDC(KS))+ENEW(KS)
   30     CONTINUE
          DO 40 KS=1,3
            IS = IDC(KS)
            IF(IS.LE.0)GO TO 40
C            RN=ENG*WCN(KS)
            IF(GAINAN(ID).NE.0.) RN=ENEW(KS)/GAINAN(ID)
            IF(TRHIST) CALL HFILL(7400+10*ISTACK+KS,RN,0.,1.)
            IF(NEWCAT(KS))THEN
              CALL VSCALE(YSHAP,RN,CAUX(1,IS),NMFADC)
            ELSE
              CALL VLINE(YSHAP,RN,CAUX(1,IS),1.,CAUX(1,IS),NMFADC)
            ENDIF
            CMAX(IS) = VMAX(CAUX(1,IS),NMFADC)
            IF(ENG.GT.15. AND. PTRD.GE.8)THEN
              WRITE(LOUT,*)' NEWCAT',NEWCAT(IS),' IS',IS,'CMAX',CMAX(IS)
              WRITE(LOUT,*)'  DQ/DT FOR  CATHODE STRIPS ISTACK ',ISTACK
              WRITE(LOUT,'(10G10.4)')
     &         (CAUX(KX,IS)*RNOR(ISTACK),KX=1,NMFADC)
            ENDIF
   40     CONTINUE
        ENDIF
  300 CONTINUE
C
      IF(TRHIST) THEN
        IPA=0
        IF(AMASS.LT..1)IPA=1
        IPA=ISTACK+IPA*10
        CALL HF1(7100+IPA,S0,1.)
        IF(STRD(2).NE.1)THEN
          DO 305 ICAT=1,NCSH
            CALL HFILL(7400+ISTACK,ETOTSN(ICAT),0.,1.)
  305     CONTINUE
        ENDIF
      ENDIF
C
C
C  PACK THE INFORMATION AND FILL THE ZEBRA BANKS "TLYR"
C
      IF(IPRNT.GT.0 .AND. PTRD.GE.9 )
     +WRITE(LOUT,*)'  NB. OF HIT WIRES',NWH,' CLUSTER NB ',
     +NSMEAR
      IF(TRHIST)CALL HF1(7006,FLOAT(NWH),1.)
      S1=0.
      S2=0.
      DO 340 IW=1,NWH
        IF(YMAX(IW).LE.0.) THEN
          WRITE(LOUT,*)
     +  ' PROBLEM_TRD IN CLUDIG: YMAX(',IW,')='
     +   ,YMAX(IW),' FOR ITRA =',ITRA
          GO TO 340
        ENDIF
        K=0
        CALL BKTLYR(LTLYR,TSTACK)
        IZSTAR=LTLYR
        IF (IZSTAR.LE.0) THEN
          WRITE (LOUT,*) 'PROBLEM_TRD IN CLUDIG.  LTLYR',LTLYR
          GO TO 340
        ENDIF
        IF(IPRNT.GT.0 .AND. PTRD.GE.9)THEN
          WRITE(LOUT,*) ' CLUDIG ANODES STACK',TSTACK,' IZSTAR',IZSTAR
          WRITE(LOUT,*) ' LTLYR ',LTLYR
        ENDIF
        IF (YMAX(IW).GT.WMAX) THEN
          WRITE(LOUT,*) ' PROBLEM_TRD IN CLUDIG YMAX > WMAX'
          WRITE(LOUT,*) ' YMAX(',IW,')=',YMAX(IW)
        ENDIF
        FACT=WMAX/YMAX(IW)
        FACT=AINT(FACT)
        FACT=AMIN1(FACT,FACTM)
        IQ(IZSTAR+1)=INT(FACT*100.)*256+INT(ORIG)
        FACT = FLOAT(INT(FACT*100.))/100.
        NDD=IQ(IZSTAR-1)
        IQ(IZSTAR+NDD)=HITW(IW)
        IQ(IZSTAR+NDD-1)=ITRA+1000*ISTAK
        K=K+1
        DO 320 I=1,NMFADC,NWORDF 
          K=K+1
          IQ(IZSTAR+K)=0
          DO 316 J=1,NWORDF
            IBIN=YAUX(I+J-1,IW)*FACT+ORIG
            IBIN=MIN(IBIN,IBINMX)
            IBIN = MAX(IBIN,0)
            S1=S1+FLOAT(IBIN)
            IF (FACT.GT.0.) S2=S2+(FLOAT(IBIN)-ORIG)/FACT
            IQ(IZSTAR+K)=IQ(IZSTAR+K)+IBIN*SHIFT(J)
  316     CONTINUE
  320   CONTINUE
  340  CONTINUE
C
C  ZEBRA BANK "TLYR" FOR CATHODES
C
      IF (STRD(2).NE.1.) THEN
        IF(IPRNT.GT.0 .AND. PTRD.GE.4 )
     +    WRITE(LOUT,*)'  NB. OF HIT CATHODE STRIPS',NCSH,
     +    ' CLUSTER NB ',NSMEAR
        IF(TRHIST)CALL HF1(7206,FLOAT(NCSH),1.)
        DO 345 IS=1,NCSH
          IF (CMAX(IS).LE.0.) THEN
            WRITE(LOUT,*)
     +       '   PROBLEM_TRD IN CLUDIG: CMAX(',IS,')='
     +       ,CMAX(IS),' FOR ITRA =',ITRA
            GO TO 345
          ENDIF
          K = 0
          CALL BKTLYR(LTLYR,TSTACK)
          IZSTAR = LTLYR
          IF (IZSTAR.LE.0) THEN
            WRITE (LOUT,*) 'ERROR FROM S/R BKTLYR.  LTLYR',LTLYR
            GO TO 345
          ENDIF
          IF (IPRNT.GT.0 .AND. PTRD.GE.9) THEN
            WRITE(LOUT,*) ' CLUDIG CATHOD STACK',TSTACK,' IZSTAR',
     &          IZSTAR
            WRITE(LOUT,*) ' LTLYR ',LTLYR,' ITRA',ITRA,' ISTAK',ISTAK
          ENDIF
          S3 = 0.
          S4 = 0.
          S0=0.
          FACT = WMAX/CMAX(IS)
          FACT=AMIN1(FACT,FACTM)
          FACT=AINT(FACT)
          IQ(IZSTAR+1)=INT(FACT*100.)*256+INT(ORIG)
          FACT = FLOAT(INT(FACT*100.))/100.
          NDD = IQ(IZSTAR-1)
          IQ(IZSTAR+NDD) = HITCS(IS)+1000  !ADD 1000 TO CATHODE STRIP #
          IQ(IZSTAR+NDD-1) = ITRA+1000*ISTAK
          K = K+1
          DO 343 I=1,NMFADC,NWORDF
            K = K+1
            IQ(IZSTAR+K) = 0
            DO 341 J=1,NWORDF
              IBIN = CAUX(I+J-1,IS)*FACT + ORIG
              IBIN = MIN(IBIN,IBINMX)
              IBIN = MAX(IBIN,0)
C              S3 = S3 + CAUX(I+J-1,IS)
C              S4 = S4 + (FLOAT(IBIN)+.5-ORIG)/FACT
              IQ(IZSTAR+K) = IQ(IZSTAR+K) +IBIN*SHIFT(J)
  341       CONTINUE
  343     CONTINUE
C         IF (IPRNT.GT.0 .AND. PTRD.GE.5) THEN
C            WRITE (LOUT,*) ' CATHODE DQ/DT : CAUX '
C            WRITE (LOUT,*) (CAUX(I,IS),I=1,NMFADC)
C            WRITE(LOUT,*) ' HITCS',HITCS(IS),
C     +                    ' SUM OF CAUX',S3,' SUM OF IBIN',S4
C          ENDIF
  345   CONTINUE
      ENDIF  ! CATHODES
C
C  KEEP THE 4 HOTTEST WIRES FOR FURTHER NEEDS
C
  350 IF(NWH.GT.4)THEN
        IW= MINFZE(ETOTWI,NWH)  !CERN ROUTINE E102
        K=0
        S4=0.
        DO 362 I=1,NWH
          IF(I.EQ.IW)GO TO 362
          K=K+1
          ETOTWI(K)=ETOTWI(I)
          IHITWN(K)=IHITWN(I)
  362   CONTINUE
        NWH=NWH-1
        GO TO 350
      ENDIF
      DO 363I=1,4
        S4=S4+ETOTWI(I)
  363   CONTINUE
C  KEEP THE 6 HOTTEST STIPS FOR FURTHER NEEDS
  370   IF(NCSH.GT.6)THEN
          IS= MINFZE(ETOTSN,NCSH)  !CERN ROUTINE E102
          K=0
          DO 372 I=1,NCSH
            IF(I.EQ.IS)GO TO 372
            K=K+1
            ETOTSN(K)=ETOTSN(I)
            IHITSN(K)=IHITSN(I)
  372     CONTINUE
          NCSH=NCSH-1
          GO TO 370
        ENDIF
 4545   FORMAT(12F6.1)
        IF(IPRNT.GT.0 .AND. PTRD.GE.9)WRITE(LOUT,*) ' EXIT CLUDIG'
        RETURN
        END
