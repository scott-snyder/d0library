      FUNCTION GZFPPH(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FPPH 
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
      INTEGER GZFPPH
      INTEGER HALF,UNIT,LFPHF,GZFPHF
C----------------------------------------------------------------------
      UNIT=2
      GZFPPH=0
      LFPHF=GZFPHF(HALF)
      IF ( LFPHF .NE. 0 ) GZFPPH=LC(LFPHF-(UNIT+1))
C----------------------------------------------------------------------
  999 RETURN
      END
