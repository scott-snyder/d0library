      INTEGER FUNCTION GZCCPH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CCPH bank
C-
C-   Returned value  : Link to CCPH Bank
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-JUL-1990 Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCCPH.LINK/LIST'
      INTEGER GZSCAL
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCCPH=0
C
C--   GET LINK TO SUPPORTING SCAL BANK
      LSCAL=GZSCAL('STPC')
C
C--   CHECK LSCAL
      IF(LSCAL.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCCPH',
     &    'SCAL BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CCPH
      GZCCPH=LC(LSCAL-IZCCPH)

C
  999 RETURN
      END
