      FUNCTION GZFGTH(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGTH 
C-
C-   Returned value  : 
C-   Inputs  : HALF, UNIT
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZFGTH
      INTEGER HALF,UNIT,LFGHF,GZFGHF
C----------------------------------------------------------------------
      GZFGTH=0
      LFGHF=GZFGHF(HALF)
      IF ( LFGHF .NE. 0 ) GZFGTH=LC(LFGHF-(UNIT+1))
C----------------------------------------------------------------------
  999 RETURN
      END
