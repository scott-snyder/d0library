      FUNCTION GZFTTH(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTTH 
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
      INTEGER GZFTTH
      INTEGER HALF,UNIT,LFTHF,GZFTHF
C----------------------------------------------------------------------
      GZFTTH=0
      LFTHF=GZFTHF(HALF)
      IF ( LFTHF .NE. 0 ) GZFTTH=LC(LFTHF-(UNIT+1))
C----------------------------------------------------------------------
  999 RETURN
      END
