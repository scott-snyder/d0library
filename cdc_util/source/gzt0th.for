C----------------------------------------------------------------------
      INTEGER FUNCTION GZT0TH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointer to the T0TH bank
C-
C-   Inputs       : None
C-   Outputs      : None
C-   Return value : Pointer to T0TH bank or 0 if this bank is not exists
C-   Controls     : None
C-
C-   Created  25-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZT0TH.LINK'
      INTEGER       LZTRH, GZZTRH
C----------------------------------------------------------------------
      GZT0TH = 0
      LZTRH  = GZZTRH()
      IF (LZTRH .GT. 0) GZT0TH = LQ(LZTRH-IZT0TH)
C----------------------------------------------------------------------
      RETURN
      END
