      FUNCTION NP_DI_LEPTON_LOOSE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select di-lepton candidates for SUSY
C-                         analysis (Wino-Wino).
C-
C-   Defines leptons as follows:
C-
C-   electrons: PELC, Pt .GE. E_PT_CUT .AND. ISOLATION .LE. EM_ISO_CUT,
C-              .AND. CHI-SQUARED .LE. EM_HCHSQ_CUT
C-
C-   muons    : PMUO, Pt .GE. MU_PT_CUT .AND.  ABS(ETA) .LE. MU_ETA_CUT
C-              .AND. No Octant Crossing .AND.
C-              (CD Track Match .OR.  (MTC FRACT .GE. MTC_FRACT_CUT
C-                                 .AND.MTC HFRACT .GE. MTC_HFRACT_CUT))
C-              .AND. IFW1 .LE. MU_IFW4_CUT .AND.
C-              Floating T0 .LE. FLOAT_T0_CUT,
C-              If two muons differ in angle by MU_COL_CUT rad.
C-              .AND. both have Floating T0 .GE. BTB_T0_CUT
C-              NGOODMUON = NGOODMUON - 1
C-
C-   SELECTION is: # Good Electrons .GE. 2 .AND. MU_CORR_MET .GE. EM_MET_CUT
C-            .OR. # Good Muons     .GE. 2 .AND. MU_CORR_MET .GE. MU_MET_CUT
C-            .OR. # Good Electrons .GE. 1 .AND. # Good Muons.GE.1
C-                                         .AND. MU_CORR_MET .GE. MU_MET_CUT
C-
C-   Inputs  :
C-   Outputs : Function is True if event passes selection.
C-   Controls:

C-   Created   5-OCT-1994   Lewis Taylor Goss
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER IER,IFW4,JBIT,IUPLIM,I,J,LMUOT
      INTEGER NMUON,LPMUO,GZPMUO,NELEC,LPELC,GZPELC
      INTEGER LPNUT2,GZPNUT,LHMTE,MU_IFW4_CUT,GZPPHO
      INTEGER NGOODMUON,NGOODELEC,OCTANT_CROSS,CD_TRACKS,NCOSMIC
      integer ngoodphot
C
      PARAMETER (IUPLIM = 20)
C
      LOGICAL FIRST,DO_OCTANT_CUT,PASS_OCT_CUT,NP_DI_LEPTON_LOOSE,EZERR
C
      REAL X_ISO,PTEM,ANG,EXMISS,EYMISS,MU_CORR_MET,DEL_PHI
      REAL E_PT_CUT,MU_PT_CUT,EM_ISO_CUT,EM_HCHSQ_CUT,HCHSQ
      REAL MU_COL_CUT,MU_ETA_CUT,MTC_FRACT_CUT,MTC_HFRACT_CUT
      REAL FLOAT_T0_CUT,BTB_T0_CUT,PTMU,ETAMU,MTC_FRACT,MTC_HFRACT
      REAL FLOAT_T0,GOODMU_P(4,IUPLIM),T0_GOODMU(IUPLIM),DEL_THETA
      REAL DEL_PHI_CUT,DEL_THETA_CUT,EM_MET_CUT,MU_MET_CUT,INTEG_BDL
      REAL INTEG_BDL_CUT,BEND_VIEW_CUT,BEND_IMP_PARAM
C      REAL NUMTRKS
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      NP_DI_LEPTON_LOOSE=.FALSE.
C
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        CALL INRCP ('NP_DI_LEPTON_LOOSE_RCP', IER)
        IF (IER.EQ.0)  CALL EZPICK('NP_DI_LEPTON_LOOSE_RCP')
        IF (.NOT.EZERR(IER)) THEN
          CALL EZGET ('E_PT_CUT',       E_PT_CUT,       IER)
          CALL EZGET ('EM_ISO_CUT',     EM_ISO_CUT,     IER)
          CALL EZGET ('EM_HCHSQ_CUT',   EM_HCHSQ_CUT,   IER)
          CALL EZGET ('DO_OCTANT_CUT',  DO_OCTANT_CUT,  IER)
          CALL EZGET ('MU_ETA_CUT',     MU_ETA_CUT,     IER)
          CALL EZGET ('MU_PT_CUT',      MU_PT_CUT,      IER)
          CALL EZGET ('MU_IFW4_CUT',    MU_IFW4_CUT,    IER)
          CALL EZGET ('MTC_FRACT_CUT',  MTC_FRACT_CUT,  IER)
          CALL EZGET ('MTC_HFRACT_CUT', MTC_HFRACT_CUT, IER)
          CALL EZGET ('DEL_PHI_CUT',    DEL_PHI_CUT,    IER)
          CALL EZGET ('DEL_THETA_CUT',  DEL_THETA_CUT,  IER)
          CALL EZGET ('FLOAT_T0_CUT',   FLOAT_T0_CUT,   IER)
          CALL EZGET ('INTEG_BDL_CUT',  INTEG_BDL_CUT,  IER)
          CALL EZGET ('BEND_VIEW_CUT',  BEND_VIEW_CUT,  IER)
          CALL EZGET ('MU_COL_CUT',     MU_COL_CUT,     IER)
          CALL EZGET ('BTB_T0_CUT',     BTB_T0_CUT,     IER)
          CALL EZGET ('EM_MET_CUT',     EM_MET_CUT,     IER)
          CALL EZGET ('MU_MET_CUT',     MU_MET_CUT,     IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG ('No NP_DI_LEPTON_LOOSE_RCP', 
     &'NP_DI_LEPTON_LOOSE',
     &        'Could not find NP_DI_LEPTON_LOOSE_RCP', 'F')
        ENDIF                          ! if ier .eq.
      ENDIF                            ! if first
C
C     GET MISSING ET
C
      LPNUT2 = GZPNUT(2)
      EXMISS = 0.
      EYMISS = 0.
      IF (LPNUT2.GT.0) THEN
        EXMISS = Q(LPNUT2+3)
        EYMISS = Q(LPNUT2+4)
      ENDIF
C
      NMUON     = 0
      NGOODMUON = 0
      LPMUO     = GZPMUO(0)
      DO WHILE(LPMUO.GT.0)
        LMUOT = LQ(LPMUO-2)
        NMUON = NMUON + 1            ! NUMBER OF MUONS
C
        PTMU           = Q(LPMUO+14)
        ETAMU          = Q(LPMUO+16)
        IFW4           = IQ(LPMUO+9)
        OCTANT_CROSS   = JBIT(IQ(LPMUO+44),9)
        MTC_FRACT      = Q(LPMUO+93)
        MTC_HFRACT     = Q(LPMUO+94)
        FLOAT_T0       = Q(LPMUO+24)
        CD_TRACKS      = IQ(LPMUO+6)
        DEL_PHI        = Q(LPMUO+38)
        DEL_THETA      = Q(LPMUO+39)
        BEND_IMP_PARAM = Q(LPMUO+56)
        INTEG_BDL      = 1.
        IF (LMUOT.GT.0) INTEG_BDL = Q(LMUOT+22)
C
        PASS_OCT_CUT = .TRUE.
        IF (DO_OCTANT_CUT.AND.OCTANT_CROSS.GT.0) PASS_OCT_CUT = .FALSE.
C
        IF(PASS_OCT_CUT.AND.(PTMU.GE. MU_PT_CUT)
     &    .AND.(ABS(ETAMU)       .LE. MU_ETA_CUT)
     &    .AND.(IFW4             .LE. MU_IFW4_CUT)
     &    .AND.(FLOAT_T0         .LE. FLOAT_T0_CUT)
     &    .AND.(INTEG_BDL        .GE. INTEG_BDL_CUT)
     &    .AND.(BEND_IMP_PARAM   .LE. BEND_VIEW_CUT)
     &    .AND.(((MTC_FRACT      .GE. MTC_FRACT_CUT)
     &           .AND.(MTC_HFRACT.GE. MTC_HFRACT_CUT))
     &       .OR.((DEL_THETA     .LE. DEL_THETA_CUT)
     &           .AND.(DEL_PHI   .LE. DEL_PHI_CUT)))) THEN
          NGOODMUON = NGOODMUON + 1
          IF (NGOODMUON.LE.IUPLIM) THEN
            GOODMU_P(1,NGOODMUON) = Q(LPMUO+10)
            GOODMU_P(2,NGOODMUON) = Q(LPMUO+11)
            GOODMU_P(3,NGOODMUON) = Q(LPMUO+12)
            GOODMU_P(4,NGOODMUON) = Q(LPMUO+13)
            T0_GOODMU(NGOODMUON)  = FLOAT_T0
          ENDIF
        ENDIF
C
        LPMUO = LQ(LPMUO)
C
      ENDDO
C
      NCOSMIC = 0
      IF(NGOODMUON.GT.1) THEN
        DO I=1,NGOODMUON-1
          DO J=I+1,NGOODMUON
            ANG=(GOODMU_P(1,I)*GOODMU_P(1,J)+GOODMU_P(2,I)
     &        *GOODMU_P(2,J)+GOODMU_P(3,I)*GOODMU_P(3,J))/(GOODMU_P(4,I)
     &        *GOODMU_P(4,J))
            IF (ANG.LT.-1.) ANG = -1.
            IF (ANG.GT. 1.) ANG =  1.
            IF((ACOS(ANG)   .GE .MU_COL_CUT).AND.
     &        (T0_GOODMU(I) .GE .BTB_T0_CUT).AND.
     &        (T0_GOODMU(J) .GE .BTB_T0_CUT)) NCOSMIC = NCOSMIC + 1
          ENDDO
        ENDDO
      ENDIF
C correct missing ET for good muons
      DO I = 1,NGOODMUON
        IF(T0_GOODMU(I).LE.BTB_T0_CUT) THEN
          EXMISS = EXMISS - GOODMU_P(1,I)
          EYMISS = EYMISS - GOODMU_P(2,I)
        ENDIF
      ENDDO
      MU_CORR_MET = SQRT(EXMISS**2 + EYMISS**2)
C
      NGOODMUON = NGOODMUON - NCOSMIC ! SUBTRACT THE COSMICS
C
C...  ELECTRONS
C
      NELEC     = 0
      NGOODELEC = 0
      NGOODPHOT = 0
      LPELC     = GZPELC()
      DO WHILE(LPELC.GT.0)
        NELEC = NELEC+1
C
        PTEM  = Q(LPELC+7)
C
        IF(Q(LPELC+17).NE.0) THEN
          X_ISO = (Q(LPELC+16)-Q(LPELC+17))/Q(LPELC+17)
        ELSE
          X_ISO = -1.
        ENDIF
C
        HCHSQ = 0.
        LHMTE = LQ(LPELC-1)
        IF (LHMTE.GE.0) HCHSQ=Q(LHMTE+7) ! H-MATRIX CHISQ
C
        IF((PTEM.GE.E_PT_CUT).AND.(X_ISO.LE.EM_ISO_CUT).AND.
     &    (HCHSQ.LE.EM_HCHSQ_CUT)) NGOODELEC=NGOODELEC+1
C
        LPELC = LQ(LPELC)
C
      ENDDO
c
c      include good photons with tracks, CC ONLY
      LPELC     = GZPPHO()
      DO WHILE(LPELC.GT.0)
        NELEC = NELEC+1
C
        PTEM  = Q(LPELC+7)
C
        IF(Q(LPELC+17).NE.0) THEN
          X_ISO = (Q(LPELC+16)-Q(LPELC+17))/Q(LPELC+17)
        ELSE
          X_ISO = -1.
        ENDIF
C
        HCHSQ = 0.
        LHMTE = LQ(LPELC-1)
        IF (LHMTE.GE.0) HCHSQ=Q(LHMTE+7) ! H-MATRIX CHISQ
cc         NUMTRKS = Q(LPELC+21) ! FIND # OF TRACKS FOR PHOTON ROAD
C
        IF((PTEM.GE.E_PT_CUT).AND.(X_ISO.LE.EM_ISO_CUT).AND.
     &    (HCHSQ.LE.EM_HCHSQ_CUT)) 
     &NGOODPHOT=NGOODPHOT+1
C
        LPELC = LQ(LPELC)
C
      ENDDO

C
      NP_DI_LEPTON_LOOSE = 
     &(((NGOODELEC.GE.2).AND.(MU_CORR_MET.GE.EM_MET_CUT))
     &            .OR.((NGOODMUON.GE.2).AND.(MU_CORR_MET.GE.MU_MET_CUT))
     &            .OR.((NGOODMUON.GE.1).AND.(NGOODELEC.GE.1.OR.
     &            NGOODPHOT.GE.1)
     &                                .AND.(MU_CORR_MET.GE.MU_MET_CUT))
     &            .OR.((NGOODELEC.GE.1).AND.(NGOODPHOT.GE.1).AND.
     &                                  (MU_CORR_MET.GE.EM_MET_CUT)))
C
  999 RETURN
      END
