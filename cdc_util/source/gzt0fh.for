C----------------------------------------------------------------------
      INTEGER FUNCTION GZT0FH( IHIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointer to the T0TH bank
C-
C-   Inputs       : IHIT - hit number; if 0 the address of the first
C-                  bank in the chain returned
C-   Outputs      : None
C-   Return value : Pointer to T0FH bank or 0 if this bank is not exists
C-   Controls     : None
C-
C-   Created  25-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZT0FH.LINK'
      INTEGER       LT0HT, GZT0HT, LT0FH, LZFIND, IHIT
C----------------------------------------------------------------------
      GZT0FH = 0
      LT0HT  = GZT0HT()
      IF (LT0HT .EQ. 0) RETURN
      LT0FH  = LQ(LT0HT-IZT0FH)
      GZT0FH = LT0FH
      IF ((LT0FH .NE. 0).AND.(IHIT .NE. 0))
     &   GZT0FH = LZFIND(IXCOM,LT0FH,IHIT,-5)
C----------------------------------------------------------------------
      RETURN
      END
