      FUNCTION LUTRA1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize things at the beginning of track
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  19-MAR-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LUTRA1
C----------------------------------------------------------------------
      CALL SET_ELECTRON_PARS            ! CHANGE parametrization constants
      LUTRA1 = .TRUE.
  999 RETURN
      END
