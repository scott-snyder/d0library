C----------------------------------------------------------------------
      INTEGER FUNCTION GZT0DH( ITRACK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointer to the T0TH bank
C-
C-   Inputs       : ITRACK - track number; if 0 the address of the first
C-                  bank in the chain returned
C-   Outputs      : None
C-   Return value : Pointer to T0DH bank or 0 if this bank is not exists
C-   Controls     : None
C-
C-   Created  25-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZT0DH.LINK'
      INTEGER       LT0HT, GZT0HT, LT0DH, LZFIND, ITRACK
C----------------------------------------------------------------------
      GZT0DH = 0
      LT0HT  = GZT0HT()
      IF (LT0HT .EQ. 0) RETURN
      LT0DH  = LQ(LT0HT-IZT0DH)
      GZT0DH = LT0DH
      IF ((LT0DH .NE. 0).AND.(ITRACK .NE. 0))
     &   GZT0DH = LZFIND(IXCOM,LT0DH,ITRACK,-5)
C----------------------------------------------------------------------
      RETURN
      END
