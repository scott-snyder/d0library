C DEC/CMS REPLACEMENT HISTORY, Element GZCASH.FOR
C *1     6-DEC-1989 00:05:29 HARRISON "Rajendran Raja: zbank utility"
C DEC/CMS REPLACEMENT HISTORY, Element GZCASH.FOR
      INTEGER FUNCTION GZCASH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CASH bank
C-
C-   Returned value  : Link to 1st element of CASH linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-FEB-1992 15:09:00.17  Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCASH.LINK/LIST'
      INTEGER LCACL,GZCACL
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCASH=0
C
C--   GET LINK TO SUPPORTING CACL BANK
      LCACL=GZCACL()
C
C--   CHECK LCACL
      IF(LCACL.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCASH',
     &    'CACL BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CASH
      GZCASH=LQ(LCACL-IZCASH)

C
  999 RETURN
      END
