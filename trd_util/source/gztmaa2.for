      INTEGER FUNCTION GZTMAA2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TMAA2 bank
C-
C-   Returned value  : Link to 1st element of TMAA2 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-MAR-1990   J.Fr. Glicenstein   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTMAE.LINK/LIST'
      INTEGER LTMAE,GZTCA2,LTCA2
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTMAA2=0
C
C--   GET LINK TO SUPPORTING TMAE BANK
      LTCA2=GZTCA2()
      LTMAE=LC(LTCA2-IZTMAE)
C
C--   CHECK LTMAE
      IF(LTMAE.LE.0)THEN
        CALL ERRMSG('GZTMAA2',
     &    'TMAE BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TMAA2
      GZTMAA2=LC(LTMAE-1)
C
  999 RETURN
      END

