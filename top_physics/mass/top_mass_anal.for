      SUBROUTINE TOP_MASS_ANAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ANALYZE DILEPTON MASSES HERE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNS.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INCLUDE 'D0$INC:EVENT_QUAN1.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER LISV1,LISP1
      EQUIVALENCE (LISV1,CSTLNK(LNKMX)),(LISP1,CSTLNK(LNKMX-1))
      INTEGER GZISV1
C
      INTEGER IER
      LOGICAL FIRST_TIME
      DOUBLE PRECISION    L1,L2,F1,F2,DELF
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      INTEGER IS1,IS2,ICOMB,STATUS,NITER,ITER_MAX
      REAL    TMASS_LOS,TMASS_HIS,DELMASS_S,WMASS_S
      LOGICAL MORE
      INTEGER IERROR
C
      LOGICAL SOL_EV
C----------------------------------------------------------------------
C
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('WMASS',WMASS_S,IER)
        CALL EZGET('DO_EXACT',DO_EXACT,IER)
        CALL EZGET('TMASS_LO',TMASS_LOS,IER)
        CALL EZGET('TMASS_HI',TMASS_HIS,IER)
        CALL EZGET('DELMASS',DELMASS_S,IER)
C
        CALL UCOPYSD(TMASS_LOS,TMASS_LO,1)
        CALL UCOPYSD(TMASS_HIS,TMASS_HI,1)
        CALL UCOPYSD(DELMASS_S,DELMASS,1)
        CALL UCOPYSD(WMASS_S,WMASS,1)
C
        CALL EZGET('EXACT_CONFIGS',NCNFGE,IER)
        CALL DO_HBOOK_OPEN('HBOOK_OPEN',STATUS)
        CALL DHDIR('TOP_MASS_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('TOP_MASS','TOP_MASS_ANAL',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        CALL DO_HBOOK('CONFIGE_NTUPLE')
        CALL EZRSET
      ENDIF
C
      ITOT_EV = ITOT_EV + 1
      CALL DHDIR_DECLARE_FILE('DILEPTON')
C
      CALL DHDIR('TOP_MASS_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('TOP_MASS','TOP_MASS_ANAL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      MORE =  .TRUE.
      COMBNUM = 0.0   !1ST JET COMBINATION
C
      CALL CALCULATE_CROSS_SECTION
C
      DO WHILE (MORE)
        CALL TOP_MASS_GET_EVENT(MORE,IER)
C
        IF(IER.NE.0)GO TO 888   !NOT A GOOD EVENT
C
        IF ( COMBNUM.EQ.1.0 ) THEN
          IACC_EV = IACC_EV + 1
        ENDIF
C
        CALL MAKE_DEBUG_PLOTS  !look at smearing
C
        CALL TAG_B_JETS        !USE ISAJET to tag b jets
C
        CALL MAKE_MONTE_PLOTS  !make plots from MC data
C
        CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        SOL_EV = .FALSE.
        IF ( DO_EXACT ) THEN
C
C ****  IF HERE SOLVE FOR TOP SOLUTION EXACTLY FOR A GIVEN MASS
C
          CALL RESET_LIKELY  !Reset likelihoods
C
          TMASS_ON(1) = 0.0
          TMASS_ON(2) = 0.0
          DO ICONFG = 1 , NCNFGE
            DO ICMB = 1 , 2
              NTOPS = 0
              TMASSE = TMASS_LO
              DO WHILE (TMASSE.GE.TMASS_LO.AND.TMASSE.LE.TMASS_HI)
                NTOPS = NTOPS + 1
                IF ( NTOPS.GT.MAXT ) THEN
                  CALL ERRMSG('TOP_MASS','TOP_MASS_ANAL',
     &              'TOO MANY TOP SOLUTIONS REQUESTED ','W')
                  NTOPS = MAXT
                ENDIF
                TOP_MASS(NTOPS)=TMASSE
                CALL SETUP_ELLIPSES(IERROR)
                IF(IERROR.NE.0)THEN
                  CALL ERRMSG('TOP_MASS_ANAL','TOP_MASS',
     &              'REJECTING EVENT DUE TO OVERFLOWS ','W')
                  GO TO 999  !REJECT THIS EVENT
                ENDIF
C
                CALL CALCULATE_WEIGHTS
                CALL SAVE_TOP_SOLUTIONE
C
                IF ( ICONFG.EQ.1.AND.NSOLS(NTOPS).GT.0
     &            .AND.TMASS_ON(ICMB).EQ.0.0) THEN
                  TMASS_ON(ICMB) = TMASSE   !TURN ON TOP MASS
                ENDIF
C
                IF ( NSOLS(NTOPS).GT.0 ) THEN
                  SOL_EV = .TRUE.  !there is a solution for a combination.
                ENDIF
C
                TMASSE = TMASSE + DELMASS
              ENDDO
  123         CALL SWAP_LEPTON12   !SWAP THE TWO LEPTONS
            ENDDO
            CALL GENERATE_CONFIG   !GENERATE NEXT CONFIGURATION
          ENDDO
          CALL SAVE_RESULTS           !Put out results on to banks
        ELSE
          CALL DO_NEUTRINO_SEARCH    !THIS IS THE RR METHOD
        ENDIF
C
        IF (NJETS.EQ.2.AND.COMBNUM.EQ.1.0) THEN
          MORE = .TRUE. !MORE COMBINATIONS
          COMBNUM=6.0  !FORCE COMBNUM=7
        ENDIF
C
      ENDDO
C
  888 CONTINUE
      IF(IER.NE.0.AND.IER.NE.4)GO TO 999
C PERMIT EVENTS HERE FROM NON TTBAR SAMPLES
      IF ( SOL_EV ) THEN
        ISOL_EV = ISOL_EV + 1  !this event has solutions
      ENDIF
C
      LISV1 = GZISV1()
      DO WHILE (LISV1.NE.0)
        CALL MZDROP(IXMAIN,LISV1,'V')   !DROP STRUCTUREs BELOW
        LISV1 = LQ(LISV1)
      ENDDO
C
      RETURN
  999 CALL FLGSET('WRITE_THIS_EVENT',.FALSE.)
      RETURN
      END
