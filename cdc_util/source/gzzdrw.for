C----------------------------------------------------------------------
      INTEGER FUNCTION GZZDRW( IFADC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointer to the ZDRW bank
C-
C-   Inputs       : IFADC - FADC number; if 0 the address of the first
C-                  bank in the chain returned
C-   Outputs      : None
C-   Return value : Pointer to ZDRW bank or 0 if this bank is not exists
C-   Controls     : None
C-
C-   Created  25-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZZDRW.LINK'
      INTEGER       LZDHT, GZZDHT, LZDRW, LZFIND, IFADC
C----------------------------------------------------------------------
      GZZDRW = 0
      LZDHT  = GZZDHT()
      IF (LZDHT .EQ. 0) RETURN
      LZDRW  = LQ(LZDHT-IZZDRW)
      GZZDRW = LZDRW
      IF ((LZDRW .NE. 0).AND.(IFADC .NE. 0))
     &   GZZDRW = LZFIND(IXCOM,LZDRW,IFADC,-5)
C----------------------------------------------------------------------
      RETURN
      END
