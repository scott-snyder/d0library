      INTEGER FUNCTION GZTMAB1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TMAB1 bank
C-
C-   Returned value  : Link to 1st element of TMAB1 
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
      INTEGER LTMAE,GZTCA1,LTCA1
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTMAB1=0
C
C--   GET LINK TO SUPPORTING TMAE BANK
      LTCA1=GZTCA1()
      LTMAE=LC(LTCA1-IZTMAE)
C
C--   CHECK LTMAE
      IF(LTMAE.LE.0)THEN
        CALL ERRMSG('GZTMAB1',
     &    'TMAE BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TMAB1
      GZTMAB1=LC(LTMAE-2)
C
  999 RETURN
      END

