      LOGICAL FUNCTION INTAST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the value of the ASTFLG telling whether
C-                         an interrupt menu is active or not.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
C----------------------------------------------------------------------
      INTAST=ASTFLG
      RETURN
      END
