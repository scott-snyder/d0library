      FUNCTION GZFASC(HALF,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FASC 
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
      INTEGER GZFASC
      INTEGER HALF,SECTOR,LFAPH,GZFAPH
C----------------------------------------------------------------------
      GZFASC=0
      LFAPH=GZFAPH(HALF)
      IF ( LFAPH .NE. 0 ) GZFASC=LC(LFAPH-(SECTOR+1))
C----------------------------------------------------------------------
  999 RETURN
      END
