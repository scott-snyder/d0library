      FUNCTION GZFGPH(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGPH 
C-
C-   Returned value  : 
C-   Inputs  : HALF
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZFGPH
      INTEGER HALF,UNIT,LFGHF,GZFGHF
C----------------------------------------------------------------------
      UNIT=2
      GZFGPH=0
      LFGHF=GZFGHF(HALF)
      IF ( LFGHF .NE. 0 ) GZFGPH=LC(LFGHF-(UNIT+1))
C----------------------------------------------------------------------
  999 RETURN
      END
