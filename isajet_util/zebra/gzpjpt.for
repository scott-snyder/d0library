      INTEGER FUNCTION GZPJPT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to PJPT bank
C-
C-   Returned value  : Link to 1st element of PJPT linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   18-DEC-1989 18:10:09.84  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPJPT.LINK/LIST'
      INTEGER LPJET,GZPJET
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZPJPT=0
C
C--   GET LINK TO SUPPORTING PJHD BANK
      LPJET=GZPJET()
C
C--   CHECK LPJHD
      IF(LPJET.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZPJPT',
     &    'PJET BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO PJPT
      GZPJPT=LQ(LPJET-IZPJPT)
C
  999 RETURN
      END
