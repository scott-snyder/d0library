      LOGICAL FUNCTION GOTOP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the current value of TOPGO, indicating
C-                         whether to go directly back to the top menu
C-                         or not.
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
      GOTOP=TOPGO
      RETURN
      END
