      FUNCTION GZFPCE(HALF,UNIT,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FPCE 
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
      INTEGER GZFPCE
      INTEGER HALF,UNIT,QUAD,SECTOR,LFPQD,GZFPQD
C----------------------------------------------------------------------
      GZFPCE=0
      LFPQD=GZFPQD(HALF,UNIT,QUAD)
      IF ( LFPQD .NE. 0 ) GZFPCE=LC(LFPQD-(SECTOR+1))
C----------------------------------------------------------------------
  999 RETURN
      END
