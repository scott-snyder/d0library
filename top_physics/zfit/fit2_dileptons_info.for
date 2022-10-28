      SUBROUTINE FIT2_DILEPTONS_INFO(RUN,EVENT,LEP,LEP_ERR,MET,MET_ERR,
     &  OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To find 2 highest Et leptons (ee, eg, gg, mumu)
C-                         and fill LEP(2,14)
C-                         Also return run, event #s and Miss_ET components
C-                         and errors
C-
C-   Inputs  : PELC, PPHO, CACL, PMUON and PNUT banks
C-   Outputs :
C-   Controls: RCP
C-
C-   Updated  23-JUN-1993   Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C----------------------------------------------------------------------
      INTEGER STATUS
      INTEGER IER,RUN,EVENT,JBIT,TRK
      INTEGER RUNNO,EVONUM
      INTEGER LVERT
      INTEGER LPELC,LPPHO,LPNUT,LCACL,LCASH,LHMTR,LPARH,LPMUO
      INTEGER GZPELC,GZPPHO,GZPNUT,GZPARH,GZPMUO,GZVERT,GZHEAD
      INTEGER I,J,K,NELEC,NPHOT,NELEC_TIGHT,NELEC_LOOSE
      INTEGER IETA_HOT(5),IPHI_HOT(5),LAYER_HOT(5)
      INTEGER NCDMATCH
      INTEGER NPMUO,NMUON,IFW4,IFW2,NMUON_TIGHT,NMUON_LOOSE
      INTEGER ICLEAN,ISTAT,MASK
      INTEGER BAD_RUNS(200),NBAD_RUNS
      INTEGER FIRST_RUN,LAST_RUN
      INTEGER ITEMP
      INTEGER IRET,NO_PNUT2
C
      REAL    LEP_ET_CUT,MET_CUT,ETA_MAX
      REAL    EM_ET
      REAL    EM(20),LEP(2,16),LEP_ERR(2,3,3)
      REAL    MET(2),MET_ERR(2,2),DMET,DPXZ,DPYZ
      REAL    ET_MISS,ET_MISS_SCALAR,PHI_MISS,PNUT(2),NUTR(4,3)
      REAL    ET_MISS_ERR,PHI_MISS_ERR
      REAL    ZVTX,THETA,DETA
      REAL    PHI,PHIMOD
      REAL    ISOL,CHISQ,CORE_ET,DIST
      REAL    EDPTH(5),PDPTH(5),ENERGY_HOT(5)
      REAL    ETAMU,PTMU,ECAL_2NN,CALISO_2NN,RZ_IMPACT
C
      LOGICAL FIRST
      LOGICAL GOOD_EVT,WRITE_DST,OK
      LOGICAL FUNCTION XXX,GOOD_ELECTRON,GOOD_PHOTON
      LOGICAL DO_ELECTRONS,DO_MUONS
      LOGICAL DO_ONE_LOOSE_LEPTON,DO_TIGHT_LEPTONS
      LOGICAL DO_HV_CORRECTION,MB_MET_ERROR,OLD_VERSION
      LOGICAL ELECTRON_CRACK_CUT
      LOGICAL DO_JET_CORRECTION,DO_EM_CORRECTION,DO_MUON_CORRECTION
      LOGICAL DO_SOFT_CORRECTION,DO_ENERGY_CORRECTIONS
      LOGICAL TIGHT_MUON
      LOGICAL DO_CLEAN_UP,EZERROR,MRBS_LOSS
C----------------------------------------------------------------------
C
      EXTERNAL GZPMUO,GZPARH
C
      DATA FIRST /.TRUE./
      DATA ETA_MAX/4.5/
      DATA FIRST_RUN/50226/
      DATA LAST_RUN/66000/
C      PARAMETER (MASK = '0004188A'X)
C      COMMON /TOTALS/ LUN, NEV_IN, NEV_OUT
C----------------------------------------------------------------------
C
C
      OK=.FALSE.
      ELECTRON_CRACK_CUT=.FALSE.
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FIT_TWO_RCP')
        CALL EZGET('LEP_ET_CUT',LEP_ET_CUT,IER)
        IF(IER.EQ.0)CALL EZGET('MET_CUT',MET_CUT,IER)
        IF(IER.EQ.0)CALL EZGET_l('DO_HV_CORRECTION',DO_HV_CORRECTION,
     &    IER)
        CALL EZGET_l('DO_ELECTRONS',DO_ELECTRONS,IER)
        IF(DO_ELECTRONS)THEN
          CALL EZGET_l('ELECTRON_CRACK_CUT',ELECTRON_CRACK_CUT,IER)
        ENDIF
        CALL EZGET_l('DO_ELECTRONS',DO_ELECTRONS,IER)
        CALL EZGET_l('DO_MUONS',DO_MUONS,IER)
        IF(IER.EQ.0)CALL EZGET_l('DO_ONE_LOOSE_LEPTON',
     &    DO_ONE_LOOSE_LEPTON,IER)
        IF(IER.EQ.0)CALL EZGET_l('DO_TIGHT_LEPTONS',
     &    DO_TIGHT_LEPTONS,IER)
        IF(IER.EQ.0)CALL EZGET_l('MB_MET_ERROR',
     &    MB_MET_ERROR,IER)
        IF(IER.EQ.0)CALL EZGET_l('OLD_VERSION',
     &    OLD_VERSION,IER)
        IF(IER.EQ.0)CALL EZGET_l('DO_JET_CORRECTION',
     &    DO_JET_CORRECTION,IER)
        IF(IER.EQ.0)CALL EZGET_l('DO_EM_CORRECTION',
     &    DO_EM_CORRECTION,IER)
        IF(IER.EQ.0)CALL EZGET_l('DO_MUON_CORRECTION',
     &    DO_MUON_CORRECTION,IER)
        IF(IER.EQ.0)CALL EZGET_l('DO_SOFT_CORRECTION',
     &    DO_SOFT_CORRECTION,IER)
        IF(IER.EQ.0)CALL EZGET_l('DO_ENERGY_CORRECTIONS',
     &    DO_ENERGY_CORRECTIONS,IER)
        CALL EZGET_l('DO_CLEAN_UP',DO_CLEAN_UP,IER)
        IF(IER.NE.0)CALL ERRMSG('RCP','FIT2_DILEPTONS_INFO',
     &    'error getting RCP parameters','W')
        CALL EZRSET
C
        IF(DO_CLEAN_UP)THEN
          CALL INRCP('BAD_RUN_RCP',IER)
          IF(IER.NE.0)
     &      CALL ERRMSG('FIT_TWO','FIT_TWO_EVENT',
     &      'BAD_RUN_RCP not found','W')
          CALL EZPICK('BAD_RUN_RCP')
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('FIT_TWO','FIT_TWO_EVENT',
     &        'Cannot find bank BAD_RUN_RCP','W')
            GOTO 999
          ENDIF
          CALL EZGETA('BAD_RUN_NOS',0,0,0,NBAD_RUNS,IER)
          IF(NBAD_RUNS.LE.200) THEN
            IF(IER.EQ.0) CALL EZGET_i('BAD_RUN_NOS',BAD_RUNS,IER)
          ELSE
            CALL ERRMSG('FIT_TWO_EVENT','FIT2_DILEPTONS_INFO',
     1        'Too Many Bad Runs To Store ','F')
          ENDIF
        ENDIF
        CALL EZRSET
      ENDIF
C
C ****  INITIALISATION
C
      DO I=1,2
        DO J=1,16
          LEP(I,J)=0.0
        ENDDO
      ENDDO
      DO I=1,2
        DO J=1,3
          DO K=1,3
            LEP_ERR(I,J,K)=0.
          ENDDO
        ENDDO
      ENDDO
      DO I=1,2
        MET(I)=0.
        DO J=1,2
          MET_ERR(I,J)=0.
        ENDDO
      ENDDO
C
C ****  GET RUN AND EVENT NUMBERS
C
      RUN=RUNNO()
      EVENT=EVONUM()
C
C ****  Reject bad run and bad events
C 
      IF(.NOT.DO_CLEAN_UP)GOTO 50
      IF(RUN.LT.FIRST_RUN.OR.RUN.GT.LAST_RUN) GOTO 999
      IF(NBAD_RUNS.LT.1) GO TO 999
      DO I=1,NBAD_RUNS
        IF(RUN.EQ.BAD_RUNS(I)) GOTO 999
      ENDDO
      IF(MRBS_LOSS(IRET)) GOTO 999
C
      LHEAD=GZHEAD()
      IF(LHEAD.NE.0) THEN
C
C *** Test word 30 for micro-blank flag
C
        ITEMP=JBIT(IQ(LHEAD+30),1)
        IF(ITEMP.GT.0) GOTO 999
      ENDIF
C      
   50 CONTINUE
      LVERT=GZVERT(1)
      IF(LVERT.GT.0) ZVTX=Q(LVERT+5)
      IF(ZVTX.EQ.0) ZVTX=-1000.
C
C ****  MISSING ET...
C
      IF(DO_HV_CORRECTION.AND..NOT.DO_ENERGY_CORRECTIONS)THEN
C
C ****  Right now no HV correction available for PNUT(4) and PNUT(5)
C
C        LPNUT=GZPNUT(4)
        LPNUT=GZPNUT(2)
        IF(LPNUT.LE.0)LPNUT=GZPNUT(1)
C        IF(LPNUT.LE.0)THEN
C          LPNUT = GZPNUT(2)
C        ENDIF
        IF(LPNUT.LE.0)GOTO 999
        CALL HV_COR_PNUT(RUN,NUTR)
        ET_MISS=NUTR(3,2)
        PHI_MISS=NUTR(4,2)
        ET_MISS_SCALAR = Q(LPNUT+14)
        MET_ERR(1,1)=Q(LPNUT+11)
        MET_ERR(2,2)=Q(LPNUT+12)
        MET_ERR(1,2)=Q(LPNUT+16)
        MET_ERR(2,1)=-MET_ERR(1,2)
      ELSE
        IF(DO_HV_CORRECTION.AND.DO_ENERGY_CORRECTIONS)THEN
          CALL ERRMSG('FIT_TWO','FIT2_DILEPTONS_INFO',
     &      'HV corrections not avaialable with Energy corrections'
     &      ,'W')
        ENDIF
C        LPNUT=GZPNUT(4)
C        IF(LPNUT.LE.0)THEN
C          LPNUT = GZPNUT(2)
C        ENDIF
        LPNUT=GZPNUT(2)
        IF(LPNUT.LE.0)THEN
          NO_PNUT2=NO_PNUT2+1
          WRITE(6,*)' NO_PNUT2:', NO_PNUT2
          LPNUT=GZPNUT(1)
        ENDIF
        IF(LPNUT.LE.0)GOTO 999
        IF (DO_ENERGY_CORRECTIONS)THEN
          CALL MET_CORRECTION(DO_JET_CORRECTION,DO_EM_CORRECTION,
     &      DO_MUON_CORRECTION,DO_SOFT_CORRECTION,ET_MISS,ET_MISS_ERR,
     &      PHI_MISS,PHI_MISS_ERR,IER)
        ELSE
          ET_MISS = Q(LPNUT+7)
          PHI_MISS = Q(LPNUT+10)
        ENDIF
        ET_MISS_SCALAR = Q(LPNUT+14)
        MET_ERR(1,1)=Q(LPNUT+11)
        MET_ERR(2,2)=Q(LPNUT+12)
        MET_ERR(1,2)=Q(LPNUT+16)
        MET_ERR(2,1)=Q(LPNUT+16)
      ENDIF
      IF(ET_MISS.LT.MET_CUT)GOTO 999
      MET(1) = ET_MISS*COS(PHI_MISS)
      MET(2) = ET_MISS*SIN(PHI_MISS)
      IF(.NOT.MB_MET_ERROR)GOTO 220
      DMET=1.08+0.019*ET_MISS_SCALAR
      IF(ET_MISS.NE.0)THEN
        DMET=DMET/ET_MISS
      ELSE
        DMET=0.
      ENDIF
      DPXZ=DMET*MET(1)
      DPYZ=DMET*MET(2)
      MET_ERR(1,1)=DPXZ*DPXZ
      MET_ERR(2,2)=DPYZ*DPYZ
      MET_ERR(1,2)=0.
      MET_ERR(2,1)=0.
  220 CONTINUE
C
C ****  electrons...
C
      IF(.NOT.DO_ELECTRONS.AND..NOT.DO_MUONS)
     &  CALL ERRMSG('RCP',' FIT2_DILEPTONS_INFO',
     &' NO VALID SELECTION MADE','F')
      IF(.NOT.DO_ELECTRONS)GOTO 500
      NELEC = 0
      NPHOT = 0
      NELEC_TIGHT=0
      NELEC_LOOSE=0
C
C *** Look for electron candidates
C
      LPELC=GZPELC()
      IF(LPELC.NE.0) THEN
C
C *** Order Banks in Decreasing Pt
C
        CALL ZSORT(IXCOM,LPELC,7)
        LPELC=GZPELC()
        CALL ZTOPSY(IXCOM,LPELC)
        LPELC=GZPELC()
C
        DO WHILE(LPELC.GT.0)
          EM_ET = Q(LPELC+7)
          IF(EM_ET.LT.LEP_ET_CUT)GOTO 10
          IF(ABS(Q(LPELC+9)).GT.ETA_MAX)GOTO 10
C
C ****  Reject junk electrons
C
          THETA=Q(LPELC+8)
          PHI=Q(LPELC+10)
          CALL DET_ETA(ZVTX,THETA,DETA)
          IF(DO_CLEAN_UP)THEN
            IF(PHI.LT.5.346.AND.PHI.GT.5.342.AND.DETA.LT.-0.2
     &        .AND.DETA.GT.-0.8) GOTO 10
          ENDIF
C
C ****  Remove electrons in cracks
C
          IF(ELECTRON_CRACK_CUT)THEN
            PHI = Q(LPELC+10)
            PHIMOD=PHI*32./2./3.1415926
            PHIMOD=AMOD(PHIMOD,1.)
            IF((PHIMOD.GT.0.9 .OR. PHIMOD.LT.0.1).AND.
     &        (ABS(Q(LPELC+9)).LT.1.4)) THEN
              GOTO 10
            ENDIF
          ENDIF
C
C ****  If old version do these cuts
C
          IF(.NOT.OLD_VERSION)GOTO 230
          LHMTR=LQ(LPELC-1)
          IF(LHMTR.GT.0)THEN
            CHISQ=Q(LHMTR+7)
          ELSE
            GOTO 10
          ENDIF
          CORE_ET=Q(LPELC+17)*SIN(Q(LPELC+8))
          IF(Q(LPELC+17).GT.0.AND.CORE_ET.GT.0)THEN
            ISOL=(Q(LPELC+16)-Q(LPELC+17))/Q(LPELC+17)
          ELSE
            ISOL=.5
          ENDIF
          DIST=Q(LPELC+22)
          IF(CHISQ.GT.200.)GOTO 10
          IF(ISOL.GT..1)GOTO 10
C          IF(DIST.GT.10.)GOTO 10
          NELEC=NELEC+1
          GOTO 20
  230     CONTINUE
C
          IF(.NOT.GOOD_ELECTRON(LPELC,'LOOSE'))GOTO 10
C
          IF(DO_TIGHT_LEPTONS)THEN
            IF(.NOT.GOOD_ELECTRON(LPELC,'TIGHT'))GOTO 10
            NELEC_TIGHT=NELEC_TIGHT+1
            NELEC=NELEC+1
          ELSEIF(DO_ONE_LOOSE_LEPTON)THEN
            IF(NELEC.EQ.1.AND.NELEC_TIGHT.EQ.0)THEN
              IF(.NOT.GOOD_ELECTRON(LPELC,'TIGHT'))
     &          GOTO 10
            ENDIF
            IF(GOOD_ELECTRON(LPELC,'TIGHT'))THEN
              NELEC_TIGHT=NELEC_TIGHT+1
            ENDIF
            NELEC=NELEC+1
          ELSE
            NELEC=NELEC+1
          ENDIF
C
C          CALL CHECK_EM_QUALITY(LPELC,MASK,OK)

C          IF(.NOT.OK)GOTO 10
C          IF(BTEST(STATUS,11))WRITE(21,*)RUN,EVENT
C          IF(BTEST(STATUS,11))GOTO 10
   20     CONTINUE
          IF(DO_HV_CORRECTION)THEN
            CALL HV_COR_EM(RUN,LPELC,EM)
            DO I=1,16
              LEP(NELEC,I)=EM(I)
            ENDDO
          ELSE
            DO I=1,16
              LEP(NELEC,I)=Q(LPELC+2+I)
            ENDDO
          ENDIF
          LEP_ERR(NELEC,1,1)=Q(LPELC+11)
          LEP_ERR(NELEC,2,2)=Q(LPELC+12)
          LEP_ERR(NELEC,3,3)=Q(LPELC+26)
          LEP_ERR(NELEC,1,2)=Q(LPELC+27)
          LEP_ERR(NELEC,1,3)=Q(LPELC+28)
          LEP_ERR(NELEC,2,3)=Q(LPELC+29)
          LEP_ERR(NELEC,2,1)=LEP_ERR(NELEC,1,2)
          LEP_ERR(NELEC,3,1)=LEP_ERR(NELEC,1,3)
          LEP_ERR(NELEC,3,2)=LEP_ERR(NELEC,2,3)
   10     CONTINUE
          LPELC = LQ(LPELC)
          IF(NELEC.EQ.2)GOTO 900
        ENDDO
      ENDIF
C
C ****  NOW PHOTONS
C
C      LPPHO=GZPPHO()
C      IF(LPPHO.NE.0) THEN
C
C *** Order Banks in Decreasing Pt
C
C        CALL ZSORT(IXCOM,LPPHO,7)
C        LPPHO=GZPPHO()
C        CALL ZTOPSY(IXCOM,LPPHO)
C        LPPHO=GZPPHO()
C
C        DO WHILE(LPPHO.GT.0)
C          EM_ET = Q(LPPHO+7)
C          IF(EM_ET.LT.LEP_ET_CUT)GOTO 20
C          IF(ABS(Q(LPPHO+9)).GT.ETA_MAX)GOTO 20
C          IF(.NOT.GOOD_PHOTON(LPPHO,'LOOSE'))GOTO 20
C
C        CALL CHECK_EM_QUALITY(LPPHO,CLEANEM_MASK,OK)
C          NELEC=NELEC+1
C          IF(DO_HV_CORRECTION)THEN
C            CALL HV_COR_EM(RUN,LPPHO,EM)
C            DO I=1,16
C              LEP(NELEC,I)=EM(I)
C            ENDDO
C          ELSE
C            DO I=1,16
C              LEP(NELEC,I)=Q(LPPHO+2+I)
C            ENDDO
C          ENDIF
C   20   CONTINUE
C        LPPHO = LQ(LPPHO)
C        IF(NELEC.EQ.2)GOTO 900
C        ENDDO
C      ENDIF
  900 CONTINUE
      IF(NELEC.EQ.2)OK=.TRUE.
C      IF(NELEC.NE.2)WRITE(6,*)RUN,EVENT,'  NO TWO GOOD ELECTRONS'
      GOTO 910
C
C ****  PROCESS MUONS
C
  500 CONTINUE
C
C check pmuo bank
C
      LPARH=GZPARH()
      NPMUO = 0
      IF (LPARH.GT.0) THEN
        NPMUO=IQ(LPARH+2)
      ENDIF
C
C find number of good muons in the event
C
      IF(NPMUO.LT.2)GOTO 910
      NMUON=0
      NMUON_TIGHT=0
      NMUON_LOOSE=0
C
      LPMUO=GZPMUO(0)
C
      IF(LPMUO.NE.0) THEN
C get muon information from PMUO bank
C
C *** Order Banks in Decreasing Pt
C
        CALL ZSORT(IXCOM,LPMUO,14)
        LPMUO=GZPMUO(0)
        CALL ZTOPSY(IXCOM,LPMUO)
        LPMUO=GZPMUO(0)
C
        DO WHILE (LPMUO.GT.0)
          TIGHT_MUON=.FALSE.
          ETAMU = Q(LPMUO+16)
          IF(ABS(ETAMU).GT.1.7)GOTO 550
          PTMU = Q(LPMUO+14)
          IF(PTMU.LT.LEP_ET_CUT)GOTO 550
          IFW4 = IQ(LPMUO+9)
          IF(IFW4.GT.1) GOTO 550
          IFW2 = IQ(LPMUO+44)
          NCDMATCH = IQ(LPMUO+6)
          ECAL_2NN = Q(LPMUO+34)
          CALISO_2NN = Q(LPMUO+30)
          RZ_IMPACT = Q(LPMUO+41)
C
C ****  COSMIC RAY REJECTION
C
          IF(BTEST(IFW2,8))GOTO 550
          IF(BTEST(IFW2,6))GOTO 550
          IF(BTEST(IFW2,7))GOTO 550
C
C ****  SOME MORE MUON QUALITY CUTS
C
          NMUON_LOOSE=NMUON_LOOSE+1
C
          IF(RZ_IMPACT.LT.20.)THEN
            IF(NCDMATCH.GE.1.OR.ECAL_2NN.GT.1.OR.
     &           CALISO_2NN.GT.-2.)THEN
              TIGHT_MUON= .TRUE.
            ENDIF
          ENDIF
          IF(DO_TIGHT_LEPTONS)THEN
            IF(TIGHT_MUON)THEN
              NMUON_TIGHT=NMUON_TIGHT+1
              NMUON=NMUON+1
            ENDIF
          ELSEIF(DO_ONE_LOOSE_LEPTON)THEN
            IF(NMUON.EQ.1.AND.NMUON_TIGHT.EQ.0)THEN
              IF(.NOT.TIGHT_MUON)GOTO 550
              NMUON_TIGHT=NMUON_TIGHT+1
              NMUON=NMUON+1
            ENDIF
            IF(TIGHT_MUON)THEN
              NMUON_TIGHT=NMUON_TIGHT+1
            ENDIF
            NMUON=NMUON+1
          ELSE
            NMUON=NMUON+1
          ENDIF
C
          DO J=1,5
            LEP(NMUON,J)=Q(LPMUO+9+J)
          ENDDO
          LEP(NMUON,6)=1./Q(LPMUO+13)
          LEP(NMUON,7)=Q(LPMUO+15)
          LEP(NMUON,8)=Q(LPMUO+17)
  550     CONTINUE
          LPMUO=LQ(LPMUO)
          IF(NMUON.EQ.2)GOTO 901
        ENDDO
      ENDIF
C
  901 CONTINUE
      IF(NMUON.EQ.2)OK=.TRUE.
C      IF(NMUON.NE.2)WRITE(6,*)RUN,EVENT,'  NO TWO GOOD MUONS'
      GOTO 910
  910 CONTINUE
  999 RETURN
      END
