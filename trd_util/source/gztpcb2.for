      INTEGER FUNCTION GZTPCB2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPCB2 bank
C-
C-   Returned value  : Link to 1st element of TPCB2 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPCB.LINK/LIST'
      INTEGER LTPEC2,GZTPEC2
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPCB2=0
C
C--   GET LINK TO SUPPORTING TPEC2 BANK
      LTPEC2=GZTPEC2()
C
C--   CHECK LTPEC2
      IF(LTPEC2.LE.0)THEN
        CALL ERRMSG('GZTPCB2:TPEC2 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPCB2
      GZTPCB2=LC(LTPEC2-IZTPCB)
C
  999 RETURN
      END

