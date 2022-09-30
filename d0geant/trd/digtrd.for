      LOGICAL FUNCTION DIGTRD
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : TRD digitization.goes from dE/dT to dQ/dT
C-                         taking into account the wire gain
C-             The bank CDD4 (raw data)is filled after having added
C-             pedestals and electronic noise
C-
C-             STRD(3)=0 Banks "CDD4" and "GTLY" kept and written on output
C-                      file
C-                    =1  Bank "GTLY" dropped only "CDD4" written
C-             STRD(4)=0  bank "TRDH" dropped not written
C-                    =1  "       "   kept and written on output
C-
C-   INPUTS  :dE/dT
C-            ENORM=WIRE GAINS(SIMILAR FOR ALL THE WIRES FOR THE TIME B
C-   OUTPUTS :FADC INFORMATION IN PACKED FORM
C-
C-   CREATED :19-JUN-1986   S.L.L.
C-   Updated  26-OCT-1989   Jeffrey Bantly  add Crate Header and Trailer words
C-   Updated   2-NOV-1989   Qizhong Li-Demarteau  change data format to Ver 2
C-   Updated  28-DEC-1989   A. Zylberstejn  Call to a simplified TRDCAL
C-   Updated   6-MAY-1993   J.P. Cussonneau  Remove call to trdcal
C-                                           Add call to trgpmc to simulate
C-                                           real pedestal events and Uranium
C-                                           background
C-----------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:NORTRD.INC'
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:GCFLAG.INC'
      INCLUDE 'D0$INC:GEOMTC.INC'
      INCLUDE 'D0$INC:GEOMTR.INC'
      INCLUDE 'D0$INC:TRINFO.INC'
      INCLUDE 'D0$INC:TECDSC.INC'
      INCLUDE 'D0$INC:GTRHLN.INC'
C      INCLUDE 'D0$INC:TRDLNK.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PEDTRD.INC'
C
      INCLUDE 'D0$LINKS:IZCDD4.LINK/LIST'
      INCLUDE 'D0$LINKS:IZGEAN.LINK/LIST'
C
      INTEGER JST,JW,ISTART,NINF,NWORDS,N5,NBID
      INTEGER I,IERR,IFA,IFOIS,IST,IJ,IW,IWIRE,J,LL,NBANK
      INTEGER NHITPW(256),ICR
      INTEGER NHITPC(256), JS, KS, KSM
      INTEGER ND,NZBANK,IWM,VOIE
      REAL DY,DEDT(NMFADC),V,QTOT,QTOT_MIN,WGN,Q1,SFADC,SDED
      INTEGER KW,K,IVER,ISKP,IFL
      INTEGER LCDD4
      REAL DEDTW(NMFADC),DEDTC(NMFADC)
      REAL CGAIN,PEDS,RN,PEDMC(NMFADC)
      REAL RNDM
      LOGICAL FIRST
      INTEGER GZTRDH,LTRDH
      INTEGER LENBNK,MXCDD4,MXSTRT
      INTEGER IPED,ITRG
      LOGICAL UR
C
      DATA LENBNK,MXCDD4 /34,50000/
      DATA CGAIN /3.0/    ! Temporarily here.  Cathode/anode gain ratio
      DATA IFOIS/0/
      DATA FIRST/.TRUE./
C
C-----------------------------------------------------------------------------
C
      DIGTRD = .TRUE.
      IF ( DTRD .LT. 3 ) GOTO 999
      IF(FIRST)THEN
        FIRST =.FALSE.
        DO IST = 1,3
          ECAL(IST) = EREF/SURCAL(IST)
          RNOR(IST) = SURCAL(IST)/EREF
          IF(PTRD.GE.4) THEN
            WRITE(LOUT,*)' IN DIGTRD, GAIN OF LAYER NUMBER:',IST
            WRITE(LOUT,*) EREF,'kev  ->',SURCAL(IST),'FADC'
          ENDIF
        ENDDO
      ENDIF
C

      IFOIS=IFOIS+1
C
C  Fill the ZEBRA Bank "CDD4" :TRD Flash Adc
C  First create stand alone Bank where all the information on one
C  wire are summed bin per bin
C
C      CALL DZVERI(' ',IXCOM,'CFLSU')
      LTRDH=GZTRDH()
      IF (LTRDH.LE.0)THEN
        IF ( PTRD.NE.0 )
     &    WRITE(LOUT,*)' PROBLEM_TRD IN DIGTRD: TRDH BANK NOT BOOKED'
        RETURN
      ENDIF
      CALL BKCDD4(LCDD4)
C
C ****  Call GZTRDH again, in case BKCDD4 caused the bank to move.
      LTRDH=GZTRDH()
C
      IVER = 2                        ! Version 2
      ISKP = 0                        ! Header is desired, offset =0
      IFL  = 1                        ! Header is desired
      CALL CDD4FL(IVER,ISKP,IFL)      ! Add in Crate Header
      NINF=6                          ! Crate Header puts 6 words in CDD4
      ISTART=LCDD4+NINF
      MXSTRT=LCDD4+MXCDD4-LENBNK
C -- First call to trgpmc with (ITRG = 0)
      ITRG = 0
      CALL TRGPMC(ITRG,1,1,PEDMC,UR)
C --
      DO 300 JST=1,3         ! Loop on the 3 TRD stacks
        IST=4-JST
        CALL VZERO_i(NHITPW,256)
        CALL VZERO_i(NHITPC,256)
        LL=LQ(LTRDH-IST)
        IF(LL.EQ.0)GO TO 250
        NBANK=NZBANK(IXCOM,LL)
        IF (NBANK.LE.0) GO TO 300
        IWM = NWIRE(IST)
        KSM = NSTRIP(IST)
C
C  PREPARE BANK CDD4 FOR ANODES
C
        CALL VZERO_i(IWS,256)
C  PUT IWS(I) =1 IF WIRE I IS HIT
        DO 30 I=1,NBANK
          ND=IQ(LL-1)
          IWIRE=IQ(LL+ND) !WIRE NUMBER
          IW=IWIRE
          IF (IW.GE.1000) GO TO 28      ! SKIP CATHODE STRIP
          IF (IST.EQ.3) IW=(IWIRE-1)/2+1
          IWS(IW)=1
   28     LL=LQ(LL)
   30   CONTINUE
C --
        DO 100 JW=1,256 !LOOP ON THE ANODE WIRES
          KW = 257-JW
C  CRATE NUMBER                                  0 TO  3
          ICR=INT((KW-1)/16)/4
C
C -- Return pedestal or Uranium event for anodes
C
          ITRG = 1
          CALL TRGPMC(ITRG,IST,KW,PEDMC,UR)
C --
          CALL VZERO(DEDTW,NMFADC)
          CALL TRGPED('BID',KW,IST,PEDS,IERR)!get pedestal mean value
          CALL VBIAS(PEDMC,PEDS,PEDMC,NMFADC)
C --
          IF(IWS(KW).LE.0)GO TO 60
          LL = LQ(LTRDH-IST)
          DO 50 I=1,NBANK
            ND=IQ(LL-1)
            IWIRE=IQ(LL+ND) !WIRE NUMBER CCC PB LINE 307
            IW=IWIRE
            IF (IW.GE.1000) GO TO 40      !SKIP  CATHODE STRIP
            IF (IST.EQ.3) IW=(IWIRE-1)/2+1
            IF (IW.NE.KW) GO TO 40
            IF (ISTART.GE.MXSTRT) GO TO 250
            CALL TRDUNP(LL,DEDT)
            IF (IEVENT.EQ.1.AND.PTRD.GE.4) THEN
              WRITE(LOUT,*)' IEVENT=',IEVENT,' LL=',LL,' IW',IW
     &          ,' IST=',IST
              WRITE(LOUT,'(16(/,2X,8F6.3))') (DEDT(J),J=1,NMFADC)
            ENDIF
            CALL VADD(DEDT,DEDTW,DEDTW,NMFADC)
            NHITPW(KW)=NHITPW(KW)+1
   40       LL=LQ(LL)
   50     CONTINUE
   60     CONTINUE
C    -
C
          QTOT = 0.
          CALL TRGGN('BID',KW,IST,WGN,IERR)!GET WIRE GAIN
          IF(IERR.NE.0)THEN
            WRITE(LOUT,*)' PROBLEM_TRD IN DIGTRD:Error getting TRD',
     &    ' GAINS'
            WGN=1.
          ENDIF
          IF(RNOR(IST).LE.0.) RNOR(IST)=1.
          SDED = 0.
          SFADC = 0.
          DO 80 I=1,NMFADC
            SDED=SDED+DEDTW(I)
            DEDTW(I)=DEDTW(I)*RNOR(IST)*WGN+PEDMC(I)+0.5*RNDM()
            IFA=INT(DEDTW(I))
            IFA=MAX0(0,IFA)
            IFA=MIN0(IFA,255)
            QTOT = QTOT + FLOAT(IFA)
            IF (TRHIST) THEN
              V=FLOAT(IFA)
              CALL HFILL(7054+IST,FLOAT(I)+0.01,0.,V)
              SFADC=SFADC+V
            ENDIF
   80     CONTINUE
C -- Zero substraction for anodes
          QTOT_MIN = FLOAT(NMFADC-1)*(PEDS+0.75*SIGPED(IST))
          IF (QTOT.GT.QTOT_MIN) THEN
            IF(TRHIST)THEN
              CALL HFILL(9000+IST,FLOAT(KW)+0.1,0.,1.)
              IF(UR) THEN
                CALL HFILL(9100+IST,FLOAT(KW)+0.1,0.,1.)
              ENDIF
              CALL HF1(7053,SDED,1.)
              CALL HF1(7058,SFADC,1.)
              IF(RNOR(IST).GT.0.)
     &          CALL HFILL(7060+IST,SFADC/RNOR(IST),0.,1.)
            ENDIF
            K=0
            DO I = 1,NMFADC
              K=K+1
              IF(K.GE.5)K=1
C   Pack and fill ZEBRA bank ICDD4
              IFA=INT(DEDTW(I))
              V=DEDTW(I)
              IFA=MAX0(0,IFA)
              IFA=MIN0(IFA,255)
              IJ=(I-1)/NDATW+1+ISTART
              CALL MVBITS(IFA,0,8,IQ(IJ),8*(4-K))
              IF(TRHIST.AND.UR) THEN
                CALL HFILL(9200+IST,FLOAT(I)+0.01,0.,V)
              ENDIF
            ENDDO
            NINF=NINF+NMFADC/4
            CALL MVBITS(NMFADC+4,0,8,IQ(IJ+1),0)  ! Version 2 is +4 here
            CALL MVBITS(NMFADC,0,8,IQ(IJ+1),16)
            CALL TCODER(VOIE,IST-1,KW-1,0,2)
            CALL MVBITS(NMFADC+8,0,8,IQ(IJ+2),0)  ! Version 2 is +8 here
            CALL MVBITS(VOIE,0,16,IQ(IJ+2),16)
            ISTART=IJ+2
            NINF=NINF+2
          ENDIF
  100   CONTINUE
C
C  PREPARE BANK CDD4 FOR CATHODES
C
        IF (STRD(2).NE.1.) THEN
          CALL VZERO_i(IWS,256)
C  PUT IWS(I) =1 IF WIRE I IS HIT
          LL = LQ(LTRDH-IST)
          DO 120 I=1,NBANK
            ND=IQ(LL-1)
            IWIRE=IQ(LL+ND) !WIRE NUMBER
            IW=IWIRE
            IF (IW.LT.1000) GO TO 118
            IWS(IW-1000)=1
  118       LL=LQ(LL)
  120     CONTINUE
          DO 200 JS=1,256
            KS = 257-JS
C  CRATE NUMBER                                  4 TO  7
            ICR=INT((KS-1)/16)/4 +4
C --
            ITRG = 2
            CALL TRGPMC(ITRG,IST+3,KS,PEDMC,UR)
C --
            CALL VZERO(DEDTC,NMFADC)
            CALL TRGPED('BID',KS,IST+3,PEDS,IERR)!get pedestal mean value
            CALL VBIAS(PEDMC,PEDS,PEDMC,NMFADC)
C --
            IF(IWS(KS).LE.0)GO TO 160
            LL = LQ(LTRDH-IST)
            DO 150 I=1,NBANK
              ND=IQ(LL-1)
              IW=IQ(LL+ND) !WIRE NUMBER
              IF (IW-1000.NE.KS) GO TO 140
              IF (ISTART.GE.MXSTRT) GO TO 250
              IF(TRHIST) CALL HF1(7250+IST,FLOAT(KS),1.)
              CALL TRDUNP(LL,DEDT)
              CALL VADD(DEDTC,DEDT,DEDTC,NMFADC)
              NHITPC(KS)=NHITPC(KS)+1
  140         LL=LQ(LL)
  150       CONTINUE
  160       CONTINUE
C    -Transform to FADC counts,add the pedestals,add the electronic
C       noise,truncate to 255.

            QTOT = 0.
            DO 180 I=1,NMFADC
              DEDTC(I)=DEDTC(I)*RNOR(IST)*CGAIN+PEDMC(I)+0.5*RNDM()
              IFA=INT(DEDTC(I))
              IFA=MAX0(0,IFA)
              IFA=MIN0(IFA,255)
              QTOT = QTOT + FLOAT(IFA)
              IF(TRHIST) THEN
                V = FLOAT(IFA)
                CALL HFILL(7254+IST,FLOAT(I)+0.01,0.,V)
              ENDIF
  180       CONTINUE
C -- Zero substraction for cathodes
            QTOT_MIN = FLOAT(NMFADC-1)*(PEDS+0.75*SIGPED(IST+3))
            IF (QTOT.GT.QTOT_MIN) THEN
              IF (TRHIST) THEN
                CALL HFILL(9003+IST,FLOAT(KS)+0.1,0.,1.)
                IF (UR) THEN
                  CALL HFILL(9103+IST,FLOAT(KS)+0.1,0.,1.)
                ENDIF
              ENDIF
              K = 0
              DO I = 1,NMFADC
                K=K+1
                IF(K.GE.5)K=1
                IFA = INT(DEDTC(I))
                V = DEDTC(I)
                IFA=MAX0(0,IFA)
                IFA=MIN0(IFA,255)
C   Pack and fill ZEBRA bank ICDD4
                IJ=(I-1)/NDATW+1+ISTART
                CALL MVBITS(IFA,0,8,IQ(IJ),8*(4-K))
                IF (TRHIST) THEN
                  IF (UR) CALL HFILL(9203+IST,FLOAT(I)+0.01,0.,V)
                ENDIF
              ENDDO
              NINF=NINF+NMFADC/4
              CALL MVBITS(NMFADC+4,0,8,IQ(IJ+1),0)   ! Version 2 is +4 here
              CALL MVBITS(NMFADC,0,8,IQ(IJ+1),16)
C  cathode channel number (crates 4-7)
              CALL TCODER(VOIE,IST+2,KS-1,0,2) !get address
C             IQ(IJ+2)=NMFADC+6+VOIE*65536
              CALL MVBITS(NMFADC+8,0,8,IQ(IJ+2),0)    ! Version 2 is +8 here
              CALL MVBITS(VOIE,0,16,IQ(IJ+2),16)
              IF (IDEBUG.NE.0 .AND. PTRD.GE.6) THEN
                WRITE (LOUT,*) ' CATHODE FADC OUTPUT.   IFA :'
                WRITE (LOUT,*) (DEDT(J),J=1,NMFADC)
                WRITE (LOUT,*) ' PACKED DATA BANK ICDD4.  LCDD4 ',LCDD4
                WRITE (LOUT,*) (IQ(J),J=ISTART+1,IJ+2)
              ENDIF
              ISTART=IJ+2
              NINF=NINF+2
            ENDIF
  200     CONTINUE
        ENDIF  ! CATHODES
C
        GO TO 300
C
  250   CONTINUE
        WRITE (LOUT,*) ' PROBLEM IN DIGTRD.  LL',LL,
     +  ' LCDD4',LCDD4,' ISTART',ISTART
C
  300 CONTINUE
C
C ****  Add in Crate Trailer words to CDD4
C
      IVER  = 2                     ! Version 2 trailer
      ISKP  = NINF                  ! Offset to end of CDD4 for Trailer
      IFL   = 2                     ! Fill trailer
      CALL CDD4FL(IVER,ISKP,IFL)
      NINF  = NINF + 4              ! Ver 2 Trailer has 4 words
C
      IF(LCDD4.GT.0)THEN
        NWORDS=-IQ(LCDD4-1)+NINF
        CALL MZPUSH(IXCOM,LCDD4,0,NWORDS,'R')
      ELSE
        WRITE(LOUT,*)' Problem_TRD: bank LCDD4 not booked'
      ENDIF
C  DROP THE BANKS ACCORDING TO THE VALUES OF STRD(3) AND STRD(4)
      IF(STRD(3).EQ.1. .AND. LGTRH.NE.0)CALL MZDROP(IXCOM,LGTRH,' ')
      IF(STRD(4).NE.1. .AND. LTRDH.NE.0)CALL MZDROP(IXCOM,LTRDH,' ')
  999 RETURN
      END
