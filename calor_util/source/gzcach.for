      INTEGER FUNCTION GZCACH(LCACL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CACH bank
C-
C-   Returned value  : Link to CACH bank hanging off the CACL bank
C-   Inputs  : LCACL link to CACL bank in question
C-   Outputs : GZCACH
C-   Controls: 
C-
C-   Created  29-APR-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCACH.LINK/LIST'
      INTEGER LCACL
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCACH=0
C
C--   CHECK LCACL
      IF(LCACL.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCACH','CACL BANK DOES NOT EXIST '
     &    ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CACH
      GZCACH=LQ(LCACL-IZCACH)
C
  999 RETURN
      END
