      INTEGER FUNCTION GZTPRL(LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPRL bank
C-
C-   Returned value  : Link to 1st element of TPRL linear structure
C-   Inputs  :TRD layer number
C-   Outputs :
C-   Controls:
C-
C-   Created  30-OCT-1989 17:56:44.87  A. Zylberstejn
C-   Updated  27-FEB-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTPRL.LINK/LIST'
      INTEGER LAYER,LTRDT,GZTRDT
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPRL=0
C
C--   GET LINK TO SUPPORTING TRDT BANK
      LTRDT=GZTRDT()
C
C--   CHECK LTRDT
      IF(LTRDT.LE.0)THEN
        CALL ERRMSG('TRD','GZTPRL',
     &    'TRDT BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPRL
      GZTPRL=LQ(LTRDT-LAYER)
  999 RETURN
      END
