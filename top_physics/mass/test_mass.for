      SUBROUTINE TEST_MASS(STRING,P,MASS,TOL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TESTS IF MASS OF A 4 VECTOR 
C-   IS EQUAL TO MASS QUOTED WITHIN
C-   TOLERANCE
C-
C-   Inputs  : STRING= CHARACTER STRING IDENTIFYING
C-   ERROR MESSAGE.
C-   P= 4 VECTOR, MASS=CANONICAL MASS, TOL=TOLERANCE
C-   Outputs : ERROR MESSAGE
C-   IER=NON ZERO UPON ERROR
C-   Controls: 
C-
C-   Created  28-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION    P(*),MASS
      REAL TOL
      DOUBLE PRECISION    MASS4
      CHARACTER*(*) STRING
      INTEGER TRULEN,LEN,LEN3,LEN4
      CHARACTER*32 STRING1
      REAL    TEST
      CHARACTER*15 STRING2
      INTEGER IER
C----------------------------------------------------------------------
      IER =  0
      MASS4 = (P(4)**2-P(3)**2-P(2)**2-P(1)**2)
      IF ( MASS4.GT.0.0 ) THEN
        MASS4=SQRT(MASS4)
      ELSE
        MASS4=0.0
      ENDIF
      TEST = MASS4-MASS
      IF ( ABS(TEST).GT.TOL ) THEN
        LEN = TRULEN(STRING)
        WRITE(STRING2,1)TEST
    1   FORMAT(1X,F14.3)
        CALL ADDSTR(STRING(1:LEN),' TEST_MASS',STRING1,LEN3)
        CALL ADDSTR(STRING1(1:LEN3),STRING2,STRING1,LEN4)
        CALL ERRMSG('CALORIMETER',STRING1(1:LEN4),
     &    ' MASS NOT WITHIN TOLERANCE','W')
        IER = 1
      ENDIF
  999 RETURN
      END
