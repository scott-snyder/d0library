      FUNCTION GZFTPH(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTPH 
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
      INTEGER GZFTPH
      INTEGER HALF,UNIT,LFTHF,GZFTHF
C----------------------------------------------------------------------
      UNIT=2
      GZFTPH=0
      LFTHF=GZFTHF(HALF)
      IF ( LFTHF .NE. 0 ) GZFTPH=LC(LFTHF-(UNIT+1))
C----------------------------------------------------------------------
  999 RETURN
      END
