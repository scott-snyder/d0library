      FUNCTION GZFAPH(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FAPH 
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
      INTEGER GZFAPH
      INTEGER HALF,UNIT,LFAHF,GZFAHF
C----------------------------------------------------------------------
      UNIT=2
      GZFAPH=0
      LFAHF=GZFAHF(HALF)
      IF ( LFAHF .NE. 0 ) GZFAPH=LC(LFAHF-(UNIT+1))
C----------------------------------------------------------------------
  999 RETURN
      END
