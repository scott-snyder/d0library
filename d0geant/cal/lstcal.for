      LOGICAL FUNCTION LSTCAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calorimeter wrap up 
C-                                 Called from UGLAST
C-
C-   Inputs  : NONE
C-   Outputs : NONE as of now
C-
C-   Created   9-JUL-1987   A.M.Jonckkhere
C-   Updated   9-JUN-1989   John Womersley  to close shower library 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:d0log.INC'
      LSTCAL=.TRUE.
C
C *** CALL END ROUTINE TO CLOSE SHOWER LIBRARY
C
C      IF (SHWG.GE.2) CALL SHLEND       only activate this after going
C      to HBOOK4
  999 RETURN
      END
