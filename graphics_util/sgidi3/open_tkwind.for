      SUBROUTINE OPEN_TKWIND(TITLE)
C  Switch terminal to Tektronix emulation
      CHARACTER*(*) TITLE
      COMMON/J_TBUF/TK_BUF(1000),ITPTR
      LOGICAL*1 TK_BUF
      TK_BUF(1)='1B'X                   ! ESC
      TK_BUF(2)='2A'X                   ! '*', SWITCH TO TEK MODE
      TK_BUF(3)='1B'X                   ! ESC
      TK_BUF(4)='-'                     ! SWITCH TO OVERWRITE MODE
      TK_BUF(5)='1B'X                   ! ESC
      TK_BUF(6)=12                      ! FF TO CLEAR SCREEN
      ITPTR=6
      CALL TK_OUT(TK_BUF,ITPTR)
      ITPTR=0
      RETURN
      END
