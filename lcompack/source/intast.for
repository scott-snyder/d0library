      LOGICAL FUNCTION INTAST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the value of the ASTFLG telling whether
C-                         an interrupt menu is active or not.  LCOMPACK
C-                         version always returns false.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  26-SEP-1988   Jan S. Hoftun
C-   Updated  29-Jul-1993   Herbert Greenlee
C-      Moved from COMPACK.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTAST=.FALSE.
      RETURN
      END
