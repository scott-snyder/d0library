C----------------------------------------------------------------------
      INTEGER FUNCTION GZT0RW( IFADC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointer to the T0RW bank
C-
C-   Inputs       : IFADC - FADC number; if 0 the address of the first
C-                  bank in the chain returned
C-   Outputs      : None
C-   Return value : Pointer to T0RW bank or 0 if this bank is not exists
C-   Controls     : None
C-
C-   Created  25-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZT0RW.LINK'
      INTEGER       LT0HT, GZT0HT, LT0RW, LZFIND, IFADC
C----------------------------------------------------------------------
      GZT0RW = 0
      LT0HT  = GZT0HT()
      IF (LT0HT .EQ. 0) RETURN
      LT0RW  = LQ(LT0HT-IZT0RW)
      GZT0RW = LT0RW
      IF ((LT0RW .NE. 0).AND.(IFADC .NE. 0))
     &   GZT0RW = LZFIND(IXCOM,LT0RW,IFADC,-5)
C----------------------------------------------------------------------
      RETURN
      END
