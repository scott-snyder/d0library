      INTEGER FUNCTION GZISRC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to ISRC bank
C-
C-   Returned value  : Link to 1st element of ISRC linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-JAN-1990 16:49:35.86  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISRC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISAB.LINK/LIST'
      INTEGER LISAB,GZISAB
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZISRC=0
C
C--   GET LINK TO SUPPORTING ISAB BANK
C
      LISAB=LQ(LHEADR-IZISAB)
C
C--   CHECK LISAB
      IF(LISAB.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZISRC',
     &    'ISAB BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO ISRC
      GZISRC=LQ(LISAB-IZISRC)

C
  999 RETURN
      END
