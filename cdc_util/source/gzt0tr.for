C----------------------------------------------------------------------
      INTEGER FUNCTION GZT0TR( ITRACK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointer to the T0TR bank
C-
C-   Inputs       : ITRACK - track number; if 0 the address of the first
C-                  bank in the chain returned
C-   Outputs      : None
C-   Return value : Pointer to T0TR bank or 0 if this bank is not exists
C-   Controls     : None
C-
C-   Created  17-MAR-1993   Greg Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZT0TR.LINK'
      INTEGER       LT0TH, GZT0TH, LT0TR, LZFIND, ITRACK
C----------------------------------------------------------------------
      GZT0TR = 0
      LT0TH  = GZT0TH()
      IF (LT0TH .EQ. 0) RETURN
C
      LT0TR  = LQ(LT0TH-IZT0TR)
      GZT0TR = LT0TR
      IF ((LT0TR .NE. 0).AND.(ITRACK .NE. 0))
     &   GZT0TR = LZFIND(IXCOM,LT0TR,ITRACK,-5)
C----------------------------------------------------------------------
      RETURN
      END
