      FUNCTION GZTRGR ( )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Zebra pointer to TRGR bank.
C-
C-   Returned value  : Zebra pointer to TRGR bank.
C-
C-   Inputs  : None.
C-   Outputs : None.
C-   Controls: None.
C-
C-   Created  20-MAR-1990   Sylvain Tisserant (MSU)
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER GZTRGR
C
C----------------------------------------------------------------------
C
      IF (LHEAD.NE.0) THEN
        GZTRGR = LQ(LHEAD - IZTRGR)
      ELSE
        GZTRGR = 0
      ENDIF
      RETURN
      END
