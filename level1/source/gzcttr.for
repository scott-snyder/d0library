      FUNCTION GZCTTR ( )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Zebra pointer to the CTTR bank.
C-
C-   Returned value  : Zebra pointer to CTTR bank.
C-
C-   Inputs  : None.
C-   Outputs : None.
C-   Controls: None.
C-
C-   Created  08-AUG-1990   Sylvain Tisserant (MSU)
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCTTR.LINK'
C
      INTEGER GZCTTR, GZCAEP
C
C----------------------------------------------------------------------
C
      GZCTTR = GZCAEP()
      IF(GZCTTR.EQ.0) RETURN
      GZCTTR = LQ(GZCTTR - IZCTTR)
      RETURN
      END
