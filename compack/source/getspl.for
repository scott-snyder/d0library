      LOGICAL FUNCTION GETSPL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the current value of SPLFLG, indicating
C-                         whether in split screen mode or not
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created   9-OCT-1987   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      GETSPL=SPLFLG
      RETURN
      END
