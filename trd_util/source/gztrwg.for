      FUNCTION GZTRWG(COUCHE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns current link to TRWG(couche)
C-
C-   Returned value  : 
C-   Inputs  : layer couche (1-3)
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-OCT-1990   JFG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER COUCHE
      INCLUDE 'D0$LINKS:IZTWGH.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZTWGH,GZTRWG,LTWGH
      GZTRWG = 0
        LTWGH = GZTWGH()
      IF ( LTWGH .GT. 0 ) THEN
        GZTRWG = LC(LTWGH - COUCHE)
      ENDIF
  999 RETURN
      END
