      FUNCTION GZCAGS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CAGS bank
C-
C-   Returned value  : Link to CAGS 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created 25-APR-1991   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCAGS.LINK/LIST'
      INTEGER GZCAGS,GZCGEH
C----------------------------------------------------------------------
C
      GZCAGS=0
C
C-- Get link to supporting CGEH bank
      LCGEH=GZCGEH()
      IF(LCGEH.GT.0)THEN
C
C-- Find link to CAGS
        GZCAGS=LC(LCGEH-IZCAGS)
      ELSE
        CALL ERRMSG('CALORIMETER','GZCAGS',
     &    'CGEH BANK DOES NOT EXIST ' ,'W')
      ENDIF
C
  999 RETURN
      END
