      FUNCTION GZL2FTUN(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTUN 
C-
C-   Returned value  : 
C-   Inputs  : HALF,UNIT
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-   Updated  15-JUL-1992   Yi-Cheng Liu 9 for Level2 stuff )
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZL2FTUN
      INTEGER HALF,UNIT,LKFTHF,GZL2FTHF
C----------------------------------------------------------------------
      LFTUN(HALF,UNIT) = 0
      LKFTHF=GZL2FTHF(HALF)
      IF ( LKFTHF .NE. 0 ) LFTUN(HALF,UNIT)=LC(LKFTHF-(UNIT+1))
      GZL2FTUN=LFTUN(HALF,UNIT)
C----------------------------------------------------------------------
  999 RETURN
      END
