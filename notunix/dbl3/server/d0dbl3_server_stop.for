      PROGRAM D0DBL3_SERVER_STOP
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
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INTEGER LU,IERR,I,NW,ND
      PARAMETER (ND=10)
      CHARACTER*80 PAR(ND)
C
      NW = 0
      LU = 20
   20 OPEN (UNIT=LU,FILE='DBPARAM',ACCESS='SEQUENTIAL'
     +               ,FORM='FORMATTED',STATUS='OLD',ERR=100)
      DO I=1,ND
        READ(LU,10) PAR(I)
      ENDDO
      IF(PAR(1)(1:2) .EQ. '-1' .OR. PAR(1)(2:3) .EQ. '-1') GOTO 200
      PAR(1)(1:2) = '1 '
      REWIND(UNIT=LU)
      DO I=1,ND
        WRITE(LU,10) PAR(I)
      ENDDO
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
      CLOSE(UNIT=LU)
      END
