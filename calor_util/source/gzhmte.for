C DEC/CMS REPLACEMENT HISTORY, Element GZHMTE.FOR
C *1     6-DEC-1989 00:05:29 HARRISON "Rajendran Raja: zbank utility"
C DEC/CMS REPLACEMENT HISTORY, Element GZHMTE.FOR
      INTEGER FUNCTION GZHMTE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to HMTE bank
C-
C-   Returned value  : Link to 1st element of HMTE linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-SEP-1990 12:17:52.73  Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHMTE.LINK/LIST'
      INTEGER LPELC,GZPELC
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZHMTE=0
C
C--   GET LINK TO SUPPORTING PELC BANK
      LPELC=GZPELC()
C
C--   CHECK LPELC
      IF(LPELC.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZHMTE',
     &    'PELC BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO HMTE
      GZHMTE=LQ(LPELC-IZHMTE)

C
  999 RETURN
      END
