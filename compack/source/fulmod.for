      LOGICAL FUNCTION FULMOD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return current value of FULSCR, the full screen
C-                         indication flag.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
C----------------------------------------------------------------------
      FULMOD=FULSCR
      RETURN
      END
