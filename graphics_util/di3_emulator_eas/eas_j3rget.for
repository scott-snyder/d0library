      SUBROUTINE J3RGET(CODE, VAL1, VAL2, VAL3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This module returns three (3) values based on the item requested.
CD   The caller requests information by passing a value of 1..11 as an
CD   integer CODE and this routine return reals for val1, val2, and 
CD   val3.
C-
C-   Inputs  : CODE
C-   Outputs : VAL1, VAL2, VAL3
C-   Controls: 
C-
C-   Created  20-OCT-1988   A. Virgo
C-   UPDATED  01-JUL-1990   S. ABACHI
C-   Updated  02-SEP-1992   Nobuaki Oshima - Take care of UP VECTTOR, too.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER CODE
      REAL VAL1, VAL2, VAL3
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
C
      IF (CODE .LT. 1 .OR. CODE .GT. 11) THEN
         CALL ERROR('J3RGET: CODE OUT OF RANGE (1..11)')
      ENDIF
      VAL1 = 0.0
      VAL2 = 0.0
      VAL3 = 0.0
      GOTO (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), CODE
    1 CONTINUE
      GOTO 9900
    2 CONTINUE
      GOTO 9900
    3 CONTINUE
      GOTO 9900
    4 CONTINUE
      GOTO 9900
    5 CONTINUE
      GOTO 9900
    6 CONTINUE
      GOTO 9900
C-
    7 CONTINUE
      VAL1 = VUPNT(1)
      VAL2 = VUPNT(2)
      VAL3 = VUPNT(3)
      GOTO 9900
C-
    8 CONTINUE
      VAL1 = NORML(1)
      VAL2 = NORML(2)
      VAL3 = NORML(3)
      GOTO 9900
C-
    9 CONTINUE
      VAL1 = UPVEC(1)
      VAL2 = UPVEC(2)
      VAL3 = UPVEC(3)
      GOTO 9900
C-
   10 CONTINUE
      GOTO 9900
   11 CONTINUE
      GOTO 9900
C
C  Exit.
C
 9900 CONTINUE
      RETURN
      END
