      SUBROUTINE TK_LINE2(X1,Y1,X2,Y2)
      COMMON/TBUF/TK_BUF(1000),ITPTR
      LOGICAL*1 TK_BUF
      IF(ITPTR.GT.990) THEN
        CALL TK_OUT(TK_BUF,ITPTR)
        ITPTR=0
      ENDIF
      TK_BUF(ITPTR+1)=29                      ! GS, VECTOR MODE
      I=Y1
      IF(I.LT.0) I=0
      IF(I.GT.779) I=779
      TK_BUF(ITPTR+2)='20'X+IAND(ISHFT(I,-5),31)        ! HI Y
      TK_BUF(ITPTR+3)='60'X+IAND(I,31)                  ! LO Y
      I=X1
      IF(I.LT.0) I=0
      IF(I.GT.1023) I=1023
      TK_BUF(ITPTR+4)='20'X+IAND(ISHFT(I,-5),31)        ! HI X
      TK_BUF(ITPTR+5)='40'X+IAND(I,31)                  ! LO X
      I=Y2
      IF(I.LT.0) I=0
      IF(I.GT.779) I=779
      TK_BUF(ITPTR+6)='20'X+IAND(ISHFT(I,-5),31)        ! HI Y
      TK_BUF(ITPTR+7)='60'X+IAND(I,31)                  ! LO Y
      I=X2
      IF(I.LT.0) I=0
      IF(I.GT.1023) I=1023
      TK_BUF(ITPTR+8)='20'X+IAND(ISHFT(I,-5),31)        ! HI X
      TK_BUF(ITPTR+9)='40'X+IAND(I,31)                  ! LO X
      ITPTR=ITPTR+9
      END
