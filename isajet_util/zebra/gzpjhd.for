      FUNCTION GZPJHD()
C------------------------------------------------------------------- ---
C-
C-   Purpose and Methods : Returns the Link to PJHD bank
C-
C-   Returned value  : Link to 1st element of PJHD linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-NOV-1989 17:57:58.00  Chip Stewart
C-   Updated  17-JAN-1990   Boaz Klima
C-      CHECK NUMBER OF STRUCTURAL LINKS
C-
C------------------------------------------------------------------- ---
      IMPLICIT NONE
      INTEGER GZPJHD
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPJHD.LINK'
      INTEGER LISAE,GZISAE,NS
C------------------------------------------------------------------- ---
C
C--   INITIALIZE
      GZPJHD=0
C
C--   GET LINK TO SUPPORTING ISAE BANK
      LISAE=GZISAE()
C
C--   CHECK LISAE
      IF(LISAE.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZPJHD',
     &    'ISAE BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C ****  Get number of structural links
C
      NS = IQ(LISAE-2)
      IF ( NS .GT. IZPJHD ) THEN
C--   FIND LINK TO PJHD
        GZPJHD=LQ(LISAE-IZPJHD)
      ENDIF
C
  999 RETURN
      END
