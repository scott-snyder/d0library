      LOGICAL FUNCTION SETMOD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the value of SETUP, flag indicating 
C-                         whether in setup command file mode or not.
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
      SETMOD=SETUP
      RETURN
      END
