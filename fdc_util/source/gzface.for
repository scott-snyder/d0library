      FUNCTION GZFACE(HALF,UNIT,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FACE 
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
      INTEGER GZFACE
      INTEGER HALF,UNIT,QUAD,SECTOR,LFAQD,GZFAQD
C----------------------------------------------------------------------
      GZFACE=0
      LFAQD=GZFAQD(HALF,UNIT,QUAD)
      IF ( LFAQD .NE. 0 ) GZFACE=LC(LFAQD-(SECTOR+1))
C----------------------------------------------------------------------
  999 RETURN
      END
