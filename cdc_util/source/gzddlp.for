      INTEGER FUNCTION GZDDLP ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to bank DDLP
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
      INCLUDE 'D0$LINKS:IZDDLP.LINK'

      INTEGER GZDTMH

      GZDDLP = 0
      LDTMH = GZDTMH ()
      IF (LDTMH.GT.0) GZDDLP = LC (LDTMH-IZDDLP)      

  999 RETURN
      END
