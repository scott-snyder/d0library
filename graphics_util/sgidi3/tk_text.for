      SUBROUTINE TK_TEXT(STRING,X,Y)
      CHARACTER*(*) STRING
      COMMON/J_TFONTS/ITINDX,ITNUM(8)
      COMMON/J_TBUF/TK_BUF(1000),ITPTR
      LOGICAL*1 TK_BUF,BCHR
      CHARACTER*1 CHR
      EQUIVALENCE (CHR,BCHR)
      DATA ITINDX,ITOLD/1,1/
C  FONT SIZES
      DATA ITNUM/0,11,10,9,8,1,2,3/
C  ITINDX      SIZE
C     1        ALTERNATE (ESC 0)
C     2        SMALLEST (ESC ;)
C     3        NOT AS SMALL (ESC :)
C     4        SMALL    (ESC 9)
C     5        NORMAL   (ESC 8)
C     6        TIMES 2  (ESC 1)
C     7        TIMES 3  (ESC 2)
C     8        TIMES 4  (ESC 3)
C
      LENGTH=LEN(STRING)
      IF(ITPTR+LENGTH.GT.990) CALL TK_FORCE
      IF(LENGTH.GT.80)LENGTH=80
      TK_BUF(ITPTR+1)=29                     ! GS, VECTOR MODE
      I=Y                                    ! NOW POSITION XY
      IF(I.LT.0) I=0
      IF(I.GT.779) I=779
      TK_BUF(ITPTR+2)='20'X+IAND(ISHFT(I,-5),31)        ! HI Y
      TK_BUF(ITPTR+3)='60'X+IAND(I,31)                  ! LO Y
      I=X
      IF(I.LT.0) I=0
      IF(I.GT.1023) I=1023
      TK_BUF(ITPTR+4)='20'X+IAND(ISHFT(I,-5),31)        ! HI X
      TK_BUF(ITPTR+5)='40'X+IAND(I,31)                  ! LO X
      TK_BUF(ITPTR+6)=31         ! US, ALPHA MODE, NO MOVEMENT
      ITPTR=ITPTR+6
      CALL TK_FORCE
      IF(ITINDX.EQ.ITOLD) GO TO 5
C  RESIZING SECTION (NOT WORKING YET!!!)
      IF(ITINDX.LT.1.OR.ITINDX.GT.8) ITINDX=1
      ITOLD=ITINDX
      ITPTR=4
      TK_BUF(1)='1B'X             ! ESC
      TK_BUF(2)=0                 ! 0 TO RESET TO NORMAL
      TK_BUF(3)='1B'X             ! ESC
      TK_BUF(4)=ITNUM(ITINDX)     ! SELECT THE FONT SIZE
      CALL TK_FORCE                  ! DO IT
    5 DO 10 I=1,LENGTH
        CHR=STRING(I:I)
        TK_BUF(ITPTR+I)=BCHR
   10 CONTINUE
      ITPTR=LENGTH
      CALL TK_FORCE
      END
