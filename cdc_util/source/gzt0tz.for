C----------------------------------------------------------------------
      INTEGER FUNCTION GZT0TZ( ITRACK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointer to the T0TH bank
C-
C-   Inputs       : ITRACK - track number; if 0 the address of the first
C-                  bank in the chain returned
C-   Outputs      : None
C-   Return value : Pointer to T0TH bank or 0 if this bank is not exists
C-   Controls     : None
C-
C-   Created  25-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZT0TZ.LINK'
      INTEGER       LT0TH, GZT0TH, LT0TZ, LZFIND, ITRACK
C----------------------------------------------------------------------
      GZT0TZ = 0
      LT0TH  = GZT0TH()
      IF (LT0TH .EQ. 0) RETURN
      LT0TZ  = LQ(LT0TH-IZT0TZ)
      GZT0TZ = LT0TZ
      IF ((LT0TZ .NE. 0).AND.(ITRACK .NE. 0))
     &   GZT0TZ = LZFIND(IXCOM,LT0TZ,ITRACK,-5)
C----------------------------------------------------------------------
      RETURN
      END
