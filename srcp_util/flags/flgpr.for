      SUBROUTINE FLGPR (LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print flags to unit number LUN.
C-
C-   Inputs  : LUN         Logical Unit Number of output stream.
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created   9-JUL-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       LUN,I,J
      CHARACTER*1   TRUTH
      INCLUDE 'D0$INC:FLAGS.INC'
      INCLUDE 'D0$INC:FLAGNM.INC'
      CHARACTER*20  NAME(MAXFLG)
C----------------------------------------------------------------------
C
      DO 100 I =  1,NUMFLG
        WRITE(UNIT=TRUTH,FMT='(L1)') BOOLE(I)
        NAME(I) = NAMFLG(I)//'('//TRUTH//')'
  100 CONTINUE
      WRITE(UNIT=LUN,FMT='(1X,3A20,A19)') (NAME(I),I=1,NUMFLG)
C
  999 RETURN
      END
