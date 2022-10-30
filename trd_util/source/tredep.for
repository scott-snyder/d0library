      SUBROUTINE TREDEP (WIRE,ICH,YAUX,NDAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyse energy deposit for all the coded
C-                         TRD wires. Start to fill bank THIT
C-
C-   Inputs  :  Wire=wire number  (1 to 256)
C-              ICH =layer number (1 to 3)
C-              Yaux=FADC array Pedestal substracted
C-              NDAT= Number of FADC bins
C-   Outputs :
C-   Controls:
C-
C-   Created  29-MAY-1991   A. Zylberstejn
C-   Updated  26-JUL-1991   A. Zylberstejn  : Create and fill bank THIT
C-   Updated  20-SEP-1993   A. Zylberstejn  update for 512 channels in layer 3
C-   Updated  20-MAR-1994   A. Zylberstejn  go to version 3
C-   Updated  13-APR-1994   A. Zylberstejn  Add TMIN in bits 18 of IQ(lthit+1)
C-   Updated  22-APR-1994   NORMAN A. GRAF  Check bank size before
C-                                          adding TROP quantities in
C-                                          REDUCE_THIT
C-   Updated   3-FEB-1995   A. Zylberstejn  : Add URANIUM info
C-   Updated  24-MAR-1995   A. Zylberstejn  Multiply by 1000 the THIT banks
C-   containing the HV's
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUREC.INC'
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:quest.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:THIT_INFO.INC'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:VARTRD.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:zebstp.INC'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INTEGER CHA1,ICH,ISUPFLAT,I,IERR,IWI,WIRE,UBIT,NEVOLD,NM,TMIN
      REAL TRDTMIN
      INTEGER NDEV_GAS,NDEV_HV,NWANT,VERSION
      PARAMETER (NDEV_GAS = 15)
      PARAMETER (NDEV_HV = 48)
      INTEGER NDEV_URAN
      PARAMETER (NDEV_URAN = 14)
      INTEGER ICL,II,IR,LOUT,ND,TRUNIT,NDAT,JBYT,JJ,NDRAW,IAUX(4)
      INTEGER I0,I1,JR,LT,SOM,IMS,GZTRDH,TMINC,TCHNB
      INTEGER GZTHIT,LSH,LTHIT,NHIT,NTOT,IENERG,EXTD
      INTEGER FFADCB,LFADCB,NBFADC,NBIN_FADC,LTROP
      INTEGER NEED,N_IN_THIT,NWDS_USED, NWDS_MAX
      REAL ENTOT,YAUX(NDAT),VSUM,VI,XCC,YCC,XUN(256)
      CHARACTER*(32) INTOCH
      CHARACTER*3 C3
      EQUIVALENCE (VI , II )
      LOGICAL FIRST,DOPRINT,DO_DRAW,DO_HISTO,DOCLUS,EXTENDED_HITS
      LOGICAL RUN1A,TRD_DO_PRINT
      DATA FIRST/.TRUE./
      DATA VERSION/4/
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        NBIN_FADC=128
        CALL EZGET('NBINS_PHYSICS',I,IERR)
        IF(IERR.EQ.0)NBIN_FADC=I
        DO_HISTO=.FALSE.
        CALL EZGET_iarr('HSTBOK',IAUX,IERR)
        IF(IERR.EQ.0)THEN
          CALL UHTOC(IAUX(1),3,C3,3)
          DO_HISTO=C3.EQ.'Y' .OR. C3.EQ.'y' .OR. C3.EQ.'YES'
        END IF
        SWTDBG=0
        CALL EZGET('SWTDBG',SWTDBG,IERR)
        CALL EZGET_l('EXTENDED_HITS',EXTENDED_HITS,IERR)
        DOCLUS=.FALSE.
        CALL EZGET('CLUSTER_RECONS',I,IERR)
        IF(IERR.EQ.0)THEN
          CALL UHTOC(I,3,C3,3)
          DOCLUS=C3.EQ.'Y' .OR. C3.EQ.'y' .OR. C3.EQ.'YES'
        END IF
        FFADCB=1
        LFADCB=NBIN_FADC
        CALL EZGET('FIRST_FADC_BIN',I,IERR)
        IF(IERR.EQ.0)        FFADCB=I
        IF(.NOT.RUN1A())THEN
          CALL EZGET('LAST_FADC_BIN',I,IERR)
          IF(IERR.EQ.0)        LFADCB=I
        END IF
        NBFADC=LFADCB-FFADCB+1
        CALL EZRSET
        DOPRINT=TRD_DO_PRINT()
        LOUT=TRUNIT()
        IF(DOPRINT)WRITE(LOUT,*)
     &    ' In TREDEP, first bin of FADC to be considered',FFADCB,
     &    ' last bin',LFADCB,' nb. of bins',NBFADC
C        lout=6
        NDRAW=0
        DO I = 1 ,  256
          XUN(I)=I
        END DO
        EXTD=1
        IF(EXTENDED_HITS)EXTD=2
      END IF
      DOPRINT=TRD_DO_PRINT()
      IF(IQ(LHEAD+9).NE.NEVOLD)THEN
        NEVOLD=IQ(LHEAD+9)
        IF(DOPRINT)THEN
          WRITE(LOUT,*)' new evt in TREDEP',IQ(LHEAD+9)
          WRITE(LOUT,*)' ----------------'
        END IF
      END IF
C      IF(DOPRINT)WRITE(LOUT,*)' wire,ich',WIRE,ICH
      IF(DO_HISTO)CALL HCDIR('//PAWC/TRD',' ')  ! go to TRD directory
      SOM=0
      IWI=WIRE
      IF(WIRE.LE.0. OR. WIRE.GT.NWIRE_PER_LAYER(ICH))THEN
        CALL ERRMSG('wrong wire number ','TREDEP',INTOCH(WIRE) ,'W')
        GO TO 999
      END IF
      NEED = 1
      CALL MZNEED(IXMAIN, NEED, 'G')
      NWDS_USED = IQUEST(12)
      NWDS_MAX = IQUEST(13)
      IF ((NWDS_MAX - NWDS_USED) .LT. 10 0000) THEN
        CALL ERRMSG('Not enough room for bank THIT','TREDEP',
     &        ' ' ,'W')
        LTHIT=0
        NTOT=0
        GOTO 999
      END IF
      LTHIT=GZTHIT()
      IF(LTHIT.EQ.0)THEN
        NHIT=0
        CALL BKTHIT(LTHIT)   !Book bank LTHIT
        N_IN_THIT=IQ(LTHIT-1)
C        IF(DOPRINT)
C     +      WRITE(LOUT,*)' LTHIT in TREDEP',LTHIT
        IF(LTHIT.LE.0)THEN
          CALL ERRMSG('Cant book bank THIT ','TREDEP',' ','W')
          GO TO 999
        END IF
        IQ(LTHIT+1)=VERSION ! After uranium has been introduced
C        IF(NWIRE_PER_LAYER(3).GT.256) IQ(LTHIT+1) = 2! Bank version
        NTOT=2
      ELSE
      END IF
      NHIT=NHIT+1
      ENTOT=VSUM(YAUX(FFADCB),NBFADC)  ! Total energy on the wire
      TMIN=TRDTMIN(YAUX(FFADCB))+FFADCB-1
C      IF(DOPRINT .AND. ENTOT.LE.0.)THEN
C        WRITE(LOUT,*) ' In tredep,wire,ich',WIRE,ICH,
C     &      ' energy tot',ENTOT,' ntot a l entree',NTOT,' lsh',LSH,
C     &      ' lthit',LTHIT
C      END IF
      NCREC=0
      IF(DOCLUS)THEN
        CALL CLUSTF(YAUX,1000*ICH+NBIN_FADC)!Reconstruct clusters
        IF(DO_HISTO)THEN
          DO IR =  1, NCREC
            CALL HF1(670+ICH+FIRSHT,FLOAT(IBCEN(IR)),1.)
          END DO
        END IF
      END IF
C      IF(DOPRINT)
C     +    WRITE(LOUT,*) ' nhit',NHIT,
C     &    'wire',WIRE,' layer',ICH,' nb of clust',NCREC
      IF (NTOT+2*NCREC+10.GT.IQ(LTHIT-1))THEN
C        IF(DOPRINT)WRITE(LOUT,*)' First push of THIT,ntot,nd',
C     &      NTOT,IQ(LTHIT-1)
        CALL MZPUSH(IXCOM,LTHIT,0,N_IN_THIT,'I')
      END IF
      IQ(LTHIT+2)=NHIT
      LSH=LTHIT+NTOT
      IQ(LSH+1)=0
      CHA1=ICH
      IF(ICH.GT.3)THEN
        CHA1=ICH-3
        IQ(LSH+1)=1         !Encode anode/cathode
      END IF
      NCREC=MIN0(NCREC,15)
      IF(.NOT.EXTENDED_HITS)THEN
        NCREC=MIN0(NCREC,4)
      ELSE
        CALL SBYT(1,IQ(LSH+1),17,1) ! set bit 17 to 1
      END IF
      CALL SBYT(WIRE-1,IQ(LSH+1), 2,9) !Encode wire number (-1)
      CALL SBYT(CHA1-1,IQ(LSH+1),11,2)!Encode layer number (-1)
      CALL SBYT(NCREC ,IQ(LSH+1),13,4)  !Encode number of clusters
      CALL SBYT(TMIN,IQ(LSH+1),18,7)! Encode tmin on the wire
      IQ(LSH+2)=AMAX1(ENTOT*10.,0.)    ! total energy*10.
      II=IQ(LSH+1)
C   first info for wire iwire layer ich in THIT bank
      FIRST_INFO(TCHNB(WIRE,ICH))=NTOT
      NTOT=NTOT+2
C      IF(DOPRINT) THEN
C        WRITE(LOUT,*)' in tredep, wire',WIRE,' layer',CHA1,
C     &    ' nb. of clust.',NCREC,' tmin',TMIN
C        WRITE(LOUT,*)' In TREDEP after decoding hit: wire',
C     &    JBYT(II,2,9)+1,' layer',JBYT(II,11,2)+1,' type',
C     &    JBYT(II,1,1),' nb. of cluster',JBYT(II,13,4),' total energy',
C     &    FLOAT(IQ(LSH+2))*.1,' ntot',NTOT,' ncrec',NCREC,' tmin',
C     &    JBYT(II,18,8)
C      END IF
      IF(NCREC.NE.0)THEN
C        JJ=3
C        JR=-1
C        NTOT=NTOT+2
        JJ=2
        NM=0
        DO IR =  1,NCREC
          NM=NM+1
          JJ=JJ+1
          IENERG=INT(ECLR(IR)+.5)
          IQ(LSH+JJ)=IENERG
          CALL SBYT(IBCEN(IR),IQ(LSH+JJ),14,7)!Encode clus. position
          IF(EXTENDED_HITS)THEN
            CALL SBYT(IBLFT(IR),IQ(LSH+JJ+1),1,8)! left position
            CALL SBYT(IBRGHT(IR),IQ(LSH+JJ+1),9,8)! right position
            CALL SBYT(INT(YSUP(IR)+.5),IQ(LSH+JJ+1),17,8)!clus. peak
            JJ=JJ+1
          END IF
        END DO
C        IF(DOPRINT .AND. NCREC.NE.NM)WRITE(LOUT,*)' ncrec',NCREC,' nm',
C     &    NM
        IF(NCREC.NE.NM)WRITE(LOUT,*)' ncrec',NCREC,' nm',
     &    NM
        NCREC=NM
        IF(NCREC.NE.0) NTOT=NTOT+NCREC*EXTD
      END IF
C***
   10 CONTINUE
      GO TO 999
      ENTRY REDUCE_THIT
      LTHIT=GZTHIT()
      IF(LTHIT.LE.0)GO TO 999
C
      IQ(LTHIT+1)=(NTOT+1)*10+VERSION
      LTROP=LC(LTGEN-IZTROP)
C      IF(DOPRINT)
C     +  WRITE(LOUT,*)'in tredep,ntot',NTOT,' lthit',LTHIT,' ltrop',
C     +  LTROP,
C     &  ' mot 1',NTOT+1
      IF (LTROP.GT.0) THEN
        NWANT=8+NDEV_HV+NDEV_GAS+NTOT+2*NDEV_URAN
C
        IF (NWANT.GT.IQ(LTHIT-1))THEN
C          IF(DOPRINT)WRITE(LOUT,*)' Next push of THIT,ntot,nd',
C     &        NTOT,IQ(LTHIT-1)
          CALL MZPUSH(IXCOM,LTHIT,0,NWANT-IQ(LTHIT-1),'I')
          LTHIT=GZTHIT()
        END IF
C
        LSH=LTHIT+NTOT
        IQ(LSH+1)=IC(LTROP+1)! = DATF
        IQ(LSH+2)=IC(LTROP+2)! = TIMF
        IQ(LSH+3)=C(LTROP+3) != VAL_CANARY(1) ! gain with 50% half max (ADC counts/10)
        IQ(LSH+4)=C(LTROP+4) != VAL_CANARY(3) ! slope (%)
        IQ(LSH+5)=C(LTROP+5) != VAL_CANARY(5) ! gain with 80% half max (ADC counts)
        DO  I = 1,NDEV_HV
          IQ(LSH+5+I)=  C(LTROP+5+I)*1000.! = VAL_HV(I)
        END DO
        DO  I = 1,NDEV_GAS
          IQ(LSH+5+NDEV_HV+I)=C(LTROP+5+NDEV_HV+I)*10.! = VAL_GAS(I)
        END DO
C    to keep compatibility with previous LTROP structure (JFG)
        IQ(LSH+6+NDEV_HV+NDEV_GAS)=C(LTROP+6+NDEV_HV+NDEV_GAS)! = VAL_HV(NDEV_HV+1)
        IQ(LSH+7+NDEV_HV+NDEV_GAS)=C(LTROP+7+NDEV_HV+NDEV_GAS)! = VAL_HV(NDEV_HV+2)
        IQ(LSH+8+NDEV_HV+NDEV_GAS)=C(LTROP+8+NDEV_HV+NDEV_GAS)!
C  put uranium values in bank(3/feb/1995)
        IQ(LSH+9+NDEV_HV+NDEV_GAS)=C(LTROP+NDEV_HV+NDEV_GAS+ 9)! VAL_URAN1(1)
        IQ(LSH+10+NDEV_HV+NDEV_GAS)=1000.*C(LTROP+NDEV_HV+NDEV_GAS+10)! VAL_URAN1(3)
        DO I = 1,NDEV_URAN-3
          IQ(LSH+10+I+NDEV_HV+NDEV_GAS) =
     &                            10.*C(LTROP+NDEV_HV+NDEV_GAS+10+I)
        ENDDO
        IQ(LSH+22+NDEV_HV+NDEV_GAS)=C(LTROP+NDEV_HV+NDEV_GAS+22)! VAL_URAN2(1)
        IQ(LSH+23+NDEV_HV+NDEV_GAS)=1000.*C(LTROP+NDEV_HV+NDEV_GAS+23) ! VAL_URAN2(3)
        DO I = 1,NDEV_URAN-3
          IQ(LSH+23+I+NDEV_HV+NDEV_GAS)=10.
     &      *C(LTROP+NDEV_HV+NDEV_GAS+23+I)
        ENDDO
        NTOT = NWANT
      END IF
C      IF(DOPRINT)
C     +WRITE(LOUT,*)' in tredep, ntot final',NTOT,' iq(lthit-1)',
C     &  IQ(LTHIT-1),'gain',Q(LSH+5),' val_hv',Q(LSH+8+NDEV_HV+NDEV_GAS)
      IF (NTOT.LT.IQ(LTHIT-1))THEN
C        IF(DOPRINT)
C     +      WRITE(LOUT,*)' In TREDEP before final push ,ntot',
C     &      NTOT,'nd',IQ(LTHIT-1)
        CALL MZPUSH(IXCOM,LTHIT,0,NTOT-IQ(LTHIT-1),'I')
      END IF
      NTOT=0
  999 RETURN
      END
