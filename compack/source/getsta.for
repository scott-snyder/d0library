      LOGICAL FUNCTION GETSTA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the current value of STAFLG, indicating
C-                         whether staus screen is available or not
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created   8-MAY-1989   Jan S. Hoftun   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      GETSTA=STAFLG
      RETURN
      END
