      FUNCTION GZFTCE(HALF,UNIT,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTCE 
C-
C-   Returned value  : 
C-   Inputs  : HALF, UNIT, QUAD, SECTOR
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZFTCE
      INTEGER HALF,UNIT,QUAD,SECTOR,LFTQU,GZFTQU
C----------------------------------------------------------------------
      GZFTCE=0
      LFTQU=GZFTQU(HALF,UNIT,QUAD)
      IF ( LFTQU .NE. 0 ) GZFTCE=LC(LFTQU-(SECTOR+1))
C----------------------------------------------------------------------
  999 RETURN
      END
