      PROGRAM DBOFF_SERVER_STOP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main program to stop the dbl3 server
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-JUN-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INTEGER LU,IERR,I,NW
      CHARACTER*80 PAR(4)
C
      NW = 0
      LU = 20
   20 OPEN (UNIT=LU,FILE='DBPARAM',ACCESS='SEQUENTIAL'
     +               ,FORM='FORMATTED',STATUS='OLD',ERR=100)
      DO I=1,4
        READ(LU,10) PAR(I)
      ENDDO
      PAR(3)(1:2) = '1 '
      REWIND(UNIT=LU)
      DO I=1,4
        WRITE(LU,10) PAR(I)
      ENDDO
      CLOSE(UNIT=LU)
   10 FORMAT(A)
      GOTO 200
C----------------------------------------------------------------------
C
  100 CONTINUE
      NW = NW + 1
      IF(NW .LT. 4) THEN
        CALL LIB$WAIT(10.0)
        GOTO 20
      ENDIF
C
  200 CONTINUE
      END
