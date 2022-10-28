      SUBROUTINE ANALYZE_DILEPTON_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyze the emu event here.
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
      INTEGER IER
      LOGICAL FIRST_TIME
      REAL    L1,L2,F1,F2,DELF
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      INTEGER IS1,IS2,ICOMB,STATUS,NITER,ITER_MAX
C----------------------------------------------------------------------
C
      IF( first ) THEN
        first = .false.
        CALL EZPICK('LJTOP_HMATRIX_RCP')
        CALL EZGET_i('RUN',RUNC,IER)
        CALL EZGET_i('EVENT',EVENTC,IER)
        CALL EZGET_d('LEPTON1',LEPTON1(1),IER)
        CALL EZGET_d('LEPTON2',LEPTON2(1),IER)
        CALL EZGET_d('JET1',JET1,IER)
        CALL EZGET_d('JET2',JET2,IER)
        CALL EZGET_d('JET3',JET3,IER)
        CALL EZGET_d('PNUT',PNUT,IER)
        CALL EZGET_rarr('WMASS',WMASS,IER)
        CALL EZGET_l('DO_EXACT',DO_EXACT,IER)
        CALL EZGET_d('TMASS_LO',TMASS_LO,IER)
        CALL EZGET_d('TMASS_HI',TMASS_HI,IER)
        CALL EZGET('DELMASS',DELMASS,IER)
        CALL EZGET_i('EXACT_CONFIGS',NCNFGE,IER)
        CALL DO_HBOOK_OPEN('HBOOK_OPEN',STATUS)
        CALL EZRSET
      ENDIF
C
      CALL DHDIR_DECLARE_FILE('DILEPTON')
C
      CALL DHDIR('LJTOP_HMATRIX_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CPHANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF ( DO_EXACT ) THEN
C
        CALL CALCULATE_CROSS_SECTION
C
C ****  IF HERE SOLVE FOR TOP SOLUTION EXACTLY FOR A GIVEN MASS
C
        DO ICONFG = 1 , NCNFGE
          DO ICMB = 1 , 2
            NTOPS = 0
            TMASSE = TMASS_LO
            DO WHILE (TMASSE.GE.TMASS_LO.AND.TMASSE.LE.TMASS_HI)
              NTOPS = NTOPS + 1
              IF ( NTOPS.GT.MAXT ) THEN
                CALL ERRMSG('TOP_DILEPTON','ANALYZE_DILEPTON_EVENT',
     &            'TOO MANY TOP SOLUTIONS REQUESTED ','W')
                NTOPS = MAXT
              ENDIF
              TOP_MASS(NTOPS)=TMASSE
              CALL SETUP_ELLIPSES
              CALL SAVE_TOP_SOLUTIONE
              TMASSE = TMASSE + DELMASS
            ENDDO
            CALL SWAP_LEPTON12   !SWAP THE TWO LEPTONS
          ENDDO
          CALL GENERATE_CONFIG   !GENERATE NEXT CONFIGURATION
        ENDDO
      ELSE
        CALL DO_NEUTRINO_SEARCH    !THIS IS THE RR METHOD
      ENDIF
      CALL NTUPLE_CLOSE('DILEPTON',STATUS)
  999 RETURN
      END
