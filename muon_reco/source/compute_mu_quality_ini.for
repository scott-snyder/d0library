      LOGICAL FUNCTION COMPUTE_MU_QUALITY_INI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine of the muon
C-                                 selection pacakge
C-
C-   Returned value  : 
C-                      always returns TRUE - Fatal error if not successful
C-   Inputs  :
C-                       None
C-   Outputs :
C-
C-   Controls: 
C-                       Uses CLEANMU_RCP
C-
C-   Created  29-Jun-1993   Stephen J. Wimpenny
C-   Modified 17-Jan-1994  Remove initialization of CLEANMU_RCP since
C-                         this is done in the first call to MUON_SELECT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C
      COMPUTE_MU_QUALITY_INI = .TRUE.
C
  999 RETURN
      END
