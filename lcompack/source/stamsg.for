      SUBROUTINE STAMSG(STRING,CENTER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output string to STATUS part of screen if
C-                         possible. VAX-specific.
C-
C-   Inputs  : STRING: Characters to be output
C-             CENTER: Flag indicating whether to center text or not.
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-OCT-1988   Jan S. Hoftun
C-   Updated  19-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      LOGICAL CENTER
C----------------------------------------------------------------------
      CALL INTMSG(STRING)
      RETURN
      END
