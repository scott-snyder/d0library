      INTEGER FUNCTION GZCACL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CACL bank
C-
C-   Returned value  : Link to 1st element of CACL linear structure
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-APR-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCACL.LINK/LIST'
      INTEGER LCAPH,GZCAPH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCACL=0
C
C--   GET LINK TO SUPPORTING CAPH BANK
      LCAPH=GZCAPH()
C
C--   CHECK LCAPH
      IF(LCAPH.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCACL','CAPH BANK DOES NOT EXIST '
     &    ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CACL
      GZCACL=LQ(LCAPH-IZCACL)
      
C
  999 RETURN
      END
