      FUNCTION GZL2FTPH(HALF)
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
C-   Modified 15-JUN-1992   Yi-Cheng Liu ( for Level2 stuff )
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZL2FTPH
      INTEGER HALF,UNIT,LFTHF,GZL2FTHF
C----------------------------------------------------------------------
      UNIT=2
      GZL2FTPH=0
      LFTHF=GZL2FTHF(HALF)
      IF ( LFTHF .NE. 0 ) GZL2FTPH=LC(LFTHF-(UNIT+1))
C----------------------------------------------------------------------
  999 RETURN
      END
