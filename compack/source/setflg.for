      SUBROUTINE SETFLG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the flag NODISP to true to indicate that
C-                         the menu shouldn't be redisplayed after an
C-                         operation.
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
      NODISP=.TRUE.
      RETURN
      END
