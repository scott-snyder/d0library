      SUBROUTINE ERRINS(KEY, POS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Insert KEY into the position number POS.
C-
C-   Inputs  : KEY         A character string to be inserted
C-            
C-   Outputs : POS         The position where KEY is to be inserted
C- 
C-   Controls: NENTRY      Current number of data in storage
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*32 KEY
      INTEGER POS, J, K, N, P
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C
      N = NENTRY + 1
      P = POS + 1
      IF ( P .LE. N ) THEN
         J = N
         DO 100 K = P,N
           ID(J) = ID(J-1)
           COUNT(J) = COUNT(J-1)
           MAXLOG(J) = MAXLOG(J-1)
           MAXWRN(J) = MAXWRN(J-1)
           J = J - 1
  100    CONTINUE
      ENDIF
C
      NENTRY = NENTRY + 1
      ID(POS) = KEY
      MAXLOG(POS) = LOGDFL
      MAXWRN(POS) = WRNDFL
      COUNT(POS)  = 0
  999 RETURN
      END
