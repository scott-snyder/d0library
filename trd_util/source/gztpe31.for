      INTEGER FUNCTION GZTPE31()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPE31 bank
C-
C-   Returned value  : Link to 1st element of TPE31 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPE3.LINK/LIST'
      INTEGER LTPR1,GZTPR1
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPE31=0
C
C--   GET LINK TO SUPPORTING TPR1 BANK
      LTPR1=GZTPR1()
C
C--   CHECK LTPR1
      IF(LTPR1.LE.0)THEN
        CALL ERRMSG('GZTPE31:TPR1 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPE31
      GZTPE31=LC(LTPR1-IZTPE3)
C
  999 RETURN
      END

