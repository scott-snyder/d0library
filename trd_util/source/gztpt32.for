      INTEGER FUNCTION GZTPT32()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPT32 bank
C-
C-   Returned value  : Link to 1st element of TPT32 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPTR.LINK/LIST'
      INTEGER LTPR2,GZTPR2
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPT32=0
C
C--   GET LINK TO SUPPORTING TPR2 BANK
      LTPR2=GZTPR2()
C
C--   CHECK LTPR2
      IF(LTPR2.LE.0)THEN
        CALL ERRMSG('GZTPT32:TPR2 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPT32
      GZTPT32=LC(LTPR2-IZTPTR)
C
  999 RETURN
      END

