      FUNCTION GZTRPD(COUCHE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Returns link to current TRPD (pedestal) 
C-                        file.
C-   Returned value  : Link to TRPD(layer = couche) bank
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-OCT-1990   JFG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER COUCHE
      INCLUDE 'D0$LINKS:IZTPDH.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZTPDH,GZTRPD
      GZTRPD = 0
        LTPDH = GZTPDH()
      IF ( LTPDH .GT. 0 ) THEN
        GZTRPD = LC(LTPDH - COUCHE)
      ENDIF
  999 RETURN
      END
