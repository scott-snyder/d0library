      LOGICAL FUNCTION NP_LSS_SELECT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :    FILTERS FOR THE WINO/ZINO ANALYSIS
C-                            FOR MAKING DODAD FILES
C-
C-   Returned value  :        TRUE IF FILTER IS PASSED
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: 
C-
C-   Created  13-OCT-1994   DOUGLAS M. NORMAN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NP_LSS_TRILEP_LOOSE
      LOGICAL NP_DI_LEPTON_LOOSE
      LOGICAL PASS_TRILEP
      LOGICAL PASS_DILEP
C----------------------------------------------------------------------
      PASS_TRILEP=NP_LSS_TRILEP_LOOSE()
      PASS_DILEP=NP_DI_LEPTON_LOOSE()
      NP_LSS_SELECT=PASS_TRILEP.OR.PASS_DILEP
  999 RETURN
      END
