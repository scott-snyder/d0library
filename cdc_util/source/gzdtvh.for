      INTEGER FUNCTION GZDTVH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to bank DTVH
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   22-SEP-1992   Domenico Pizzuto
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDTVH.LINK'

      INTEGER GZDTMH,LLDTMH

      GZDTVH = 0
      LLDTMH = GZDTMH ()
      IF (LLDTMH.GT.0) GZDTVH = LC (LLDTMH-IZDTVH)

  999 RETURN
      END
