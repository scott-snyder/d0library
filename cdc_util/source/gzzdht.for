C----------------------------------------------------------------------
      INTEGER FUNCTION GZZDHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointer to the ZDHT bank
C-
C-   Inputs       : None
C-   Outputs      : None
C-   Return value : Pointer to ZDHT bank or 0 if this bank is not exists
C-   Controls     : None
C-
C-   Created  25-APR-1992   Gregory L. Landsberg
C-   Updated  24-MAY-1995   Gregory L. Landsberg  -- First look under
C-                                  default, then under 'FILT' paths.
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$LINKS:IZZDHT.LINK'
      INTEGER       LHITS, GZHITS
      Character*4   Path
C----------------------------------------------------------------------
      GZZDHT = 0
      Call PathGT(Path)
      LHITS  = GZHITS()   !try present path 
      IF (LHITS .NE. 0) GZZDHT = LQ(LHITS-IZZDHT)   
      IF (GZZDHT .LE. 0) Then
        Call PathST('FILT') 
        LHITS  = GZHITS() !then FILT if that fails
        IF (LHITS .NE. 0) GZZDHT = LQ(LHITS-IZZDHT)
      End If
      Call PathST(Path)
C----------------------------------------------------------------------
      RETURN
      END
