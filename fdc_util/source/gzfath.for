      FUNCTION GZFATH(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FATH 
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
      INTEGER GZFATH
      INTEGER HALF,UNIT,LFAHF,GZFAHF
C----------------------------------------------------------------------
      GZFATH=0
      LFAHF=GZFAHF(HALF)
      IF ( LFAHF .NE. 0 ) GZFATH=LC(LFAHF-(UNIT+1))
C----------------------------------------------------------------------
  999 RETURN
      END
