C DEC/CMS REPLACEMENT HISTORY, Element GZHMTP.FOR
C *1     6-DEC-1989 00:05:29 HARRISON "Rajendran Raja: zbank utility"
C DEC/CMS REPLACEMENT HISTORY, Element GZHMTP.FOR
      INTEGER FUNCTION GZHMTP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to HMTP bank
C-
C-   Returned value  : Link to 1st element of HMTP linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-SEP-1990 12:23:38.33  Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHMTP.LINK/LIST'
      INTEGER LPPHO,GZPPHO
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZHMTP=0
C
C--   GET LINK TO SUPPORTING PPHO BANK
      LPPHO=GZPPHO()
C
C--   CHECK LPPHO
      IF(LPPHO.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZHMTP',
     &    'PPHO BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO HMTP
      GZHMTP=LQ(LPPHO-IZHMTP)

C
  999 RETURN
      END
