      FUNCTION GZFPTH(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FPTH 
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
      INTEGER GZFPTH
      INTEGER HALF,UNIT,LFPHF,GZFPHF
C----------------------------------------------------------------------
      GZFPTH=0
      LFPHF=GZFPHF(HALF)
      IF ( LFPHF .NE. 0 ) GZFPTH=LC(LFPHF-(UNIT+1))
C----------------------------------------------------------------------
  999 RETURN
      END
