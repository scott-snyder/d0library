      FUNCTION GZPJET()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to PJET bank
C-
C-   Returned value  : Link to 1st element of PJET linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-NOV-1989 18:10:09.84  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZPJET
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPJET.LINK/LIST'
      INTEGER LPJHD,GZPJHD
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZPJET=0
C
C--   GET LINK TO SUPPORTING PJHD BANK
      LPJHD=GZPJHD()
C
C--   CHECK LPJHD
      IF(LPJHD.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZPJET',
     &    'PJHD BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO PJET
      GZPJET=LQ(LPJHD-IZPJET)

C
  999 RETURN
      END
