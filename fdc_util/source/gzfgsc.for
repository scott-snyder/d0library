      FUNCTION GZFGSC(HALF,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGSC 
C-
C-   Returned value  : 
C-   Inputs  : HALF, SECTOR
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZFGSC
      INTEGER HALF,SECTOR,LFGPH,GZFGPH
C----------------------------------------------------------------------
      GZFGSC=0
      LFGPH=GZFGPH(HALF)
      IF ( LFGPH .NE. 0 ) GZFGSC=LC(LFGPH-(SECTOR+1))
C----------------------------------------------------------------------
  999 RETURN
      END
