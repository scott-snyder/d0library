      SUBROUTINE CJET_MULINKS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill pointers in PMUO to associated JETS 
C-                         bank.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-FEB-1993   Alex Smith
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER   NS,PT_JETS,IJT_CONE
      REAL      PTMUONS,DR,DPHI
      REAL      JET_ETA,JET_PHI,MU_ETA,MU_PHI
      INTEGER   GZJETS,LPMUO,GZPMUO,IER
      INTEGER   MUJETS_IFW4_MAX
      LOGICAL   FIRST,EZERR
      DATA FIRST/.TRUE./
      SAVE FIRST
      SAVE MUJETS_IFW4_MAX
C----------------------------------------------------------------------
      IF ( FIRST )THEN                  ! LOCAL INIT
        FIRST = .FALSE.
        CALL INRCP('CAJETS_RCP',IER)
        CALL EZPICK('CAJETS_RCP')       ! SELECT JETS RCP BANK
        IF (IER.EQ.0)
     &    CALL EZGET('MUJETS_IFW4_MAX',MUJETS_IFW4_MAX,IER)
        IF (EZERR(IER)) THEN
          CALL ERRMSG('NO_CAJETS_RCP','CJET_MUON',
     &      'CAJETS_RCP not found in CJET_MUON.','W')
        ENDIF
        CALL EZRSET
      ENDIF
C----------------------------------------------------------------------
      PTMUONS = 0.0
C
C *** Look for PMUO muons that fall within 0.7 cone of JET.
C
      LPMUO=GZPMUO(0)
      IF(LPMUO.LE.0) THEN        ! Abort if PMUO does not exist
        CALL ERRMSG('NO_PMUO','CJET_MULINKS',
     &    'SKIP CJET_MULINKS','W')
        GOTO 999
      ENDIF
      DO WHILE (LPMUO.GT.0)
        IF(IQ(LPMUO-3).LT.6 ) THEN        ! Abort IF old version of PMUO
          CALL ERRMSG('OLD PMUO VERSION, RERUN MURECO','CJET_MULINKS',
     &        'SKIP CJET_MULINKS','W')
          GOTO 999
        ENDIF
C
C --- Cut on muon quality: --------------------------------------------------
C
        IF ( IQ(LPMUO+9) .GT. MUJETS_IFW4_MAX )
     &     GOTO 999
C
C --- Loop over JETS banks, find associated ones: ---------------------------
C
        LJETS = GZJETS()
        IF (LJETS.LE.0) THEN
          CALL ERRMSG('NO_JETS_BANK','CJET_MULINKS',
     &      'CANT LOOK FOR ASSOC. JETS','W')
          GO TO 999
        ENDIF
        PT_JETS = 0
        IJT_CONE = 0
        DO WHILE (LJETS.GT.0) 
          JET_ETA = Q(LJETS+9)
          JET_PHI = Q(LJETS+8)
          JET_PHI = JET_PHI-INT(JET_PHI/6.2830)*6.2830
          IF (JET_PHI.LT.0.0) JET_PHI = JET_PHI + 6.2830
          Q(LJETS+8) = JET_PHI
          MU_ETA = Q(LPMUO+16)
          MU_PHI = Q(LPMUO+17)
          MU_PHI = MU_PHI-(INT(MU_PHI/6.2830)*6.2830)
          IF (MU_PHI.LT.0.0) MU_PHI = MU_PHI + 6.2830
          DPHI = ABS(MU_PHI - JET_PHI)
          IF (DPHI .GT. 3.1415) DPHI = 6.2830 - DPHI
          DR = SQRT ( (MU_ETA - JET_ETA)**2 +
     &      (DPHI)**2 )  
C
C *** Fill pointers to JETS bank in PMUO if jet in cone:
C
          IF (DR .LT. 0.7) THEN
            IJT_CONE = IJT_CONE + 1
            PT_JETS = LJETS
          END IF
          LJETS = LQ(LJETS) ! NEXT JETS BANK
        END DO  ! LOOP OVER JETS
C
C *** Fill links in PMUO to JETS banks:
C
        IF (IJT_CONE .LE. 1) THEN
          IF (IJT_CONE .EQ. 1) THEN
            NS = IQ(LPMUO-2)
            LQ(LPMUO-NS-5) = PT_JETS
          END IF
        ELSE
          CALL ERRMSG('MUON WITHIN 0.7 OF >1 JET',
     &      'CJET_MULINKS', 'LINKS NOT FILLED IN PMUO','W')
          GO TO 999
        END IF
        LPMUO = LQ(LPMUO)
      END DO    ! LOOP OVER MUONS
  999 RETURN
      END
