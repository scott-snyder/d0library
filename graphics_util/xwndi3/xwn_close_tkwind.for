      SUBROUTINE CLOSE_TKWIND
C  End the graphics display on a Tektronix terminal
      COMMON/TBUF/TK_BUF(1000),ITPTR
      LOGICAL*1 TK_BUF
      INTEGER*4 VD
C  Exit graphics (go to alpha) = CTRL X
      TK_BUF(1)=24
      CALL TK_OUT(TK_BUF,1)
      ITPTR=0
      END
