      INTEGER FUNCTION GZCNSH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CNSH bank
C-
C-   Returned value  : Link to 1st element of CNSH linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  31-JUL-1991  Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCNSH.LINK/LIST'
      INTEGER GZSCAL
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCNSH=0
C
C--   GET LINK TO SUPPORTING SCAL BANK
      LSCAL=GZSCAL('STPC')
C
C--   CHECK LSCAL
      IF(LSCAL.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCNSH',
     &    'SCAL BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CNSH
      GZCNSH=LC(LSCAL-IZCNSH)

C
  999 RETURN
      END
