      INTEGER FUNCTION GZTMXE2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TMXE2 bank
C-
C-   Returned value  : Link to 1st element of TMXE2 
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
      INTEGER LTCA2,GZTCA2
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTMXE2=0
C
C--   GET LINK TO SUPPORTING TCA2 BANK
      LTCA2=GZTCA2()
C
C--   CHECK LTCA2
      IF(LTCA2.LE.0)THEN
        CALL ERRMSG('GZTMXE2',
     &    'TCA2 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TMXE2
      GZTMXE2=LC(LTCA2-IZTMXE)
C
  999 RETURN
      END

