      INTEGER FUNCTION GZDSWP ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to bank DSWP
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-JUN-1992   Domenico Pizzuto
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDSWP.LINK'

      INTEGER GZDTMH

      GZDSWP = 0
      LDTMH = GZDTMH ()
      IF (LDTMH.GT.0) GZDSWP = LC (LDTMH-IZDSWP)

  999 RETURN
      END
