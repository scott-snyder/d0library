      SUBROUTINE TK_FORCE
C  Flush anything remaining in the Tektronix output buffer.
      COMMON/TBUF/TK_BUF(1000),ITPTR
      LOGICAL*1 TK_BUF
      IF(ITPTR.GT.0) CALL TK_OUT(TK_BUF,ITPTR)
      ITPTR=0
      END
