      SUBROUTINE TK_CLEAR
C  Clear the screen of a Tektronix terminal
      COMMON/TBUF/TK_BUF(1000),ITPTR
      LOGICAL*1 TK_BUF
      INTEGER*4 VD
      TK_BUF(1)='1B'X                   ! ESC
      TK_BUF(2)=12                      ! FF, CLEAR SCREEN
      ITPTR=2
      CALL TK_OUT(TK_BUF,ITPTR)
      ITPTR=0
      END
