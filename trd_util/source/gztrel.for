      FUNCTION GZTREL(COUCHE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns current link to TREL(couche)
C-
C-   Returned value  : 
C-   Inputs  : layer couche (1-6)
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-OCT-1990   JFG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER COUCHE
      INCLUDE 'D0$LINKS:IZTELH.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZTELH,GZTREL,LTELH
      GZTREL = 0
        LTELH = GZTELH()
      IF ( LTELH .GT. 0 ) THEN
        GZTREL = LC(LTELH - COUCHE)
      ENDIF
  999 RETURN
      END
