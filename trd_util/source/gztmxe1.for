      INTEGER FUNCTION GZTMXE1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TMXE1 bank
C-
C-   Returned value  : Link to 1st element of TMXE1 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTMXE.LINK/LIST'
      INTEGER LTCA1,GZTCA1
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTMXE1=0
C
C--   GET LINK TO SUPPORTING TCA1 BANK
      LTCA1=GZTCA1()
C
C--   CHECK LTCA1
      IF(LTCA1.LE.0)THEN
        CALL ERRMSG('GZTMXE1',
     &    'TCA1 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TMXE1
      GZTMXE1=LC(LTCA1-IZTMXE)
C
  999 RETURN
      END

