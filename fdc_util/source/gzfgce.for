      FUNCTION GZFGCE(HALF,UNIT,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGCE 
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
      INTEGER GZFGCE
      INTEGER HALF,UNIT,QUAD,SECTOR,LFGQD,GZFGQD
C----------------------------------------------------------------------
      GZFGCE=0
      LFGQD=GZFGQD(HALF,UNIT,QUAD)
      IF ( LFGQD .NE. 0 ) GZFGCE=LC(LFGQD-(SECTOR+1))
C----------------------------------------------------------------------
  999 RETURN
      END
