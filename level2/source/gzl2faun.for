      FUNCTION GZL2FAUN(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FAUN 
C-
C-   Returned value  : 
C-   Inputs  : HALF,UNIT
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeff Bantly  use logical format 
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-   Updated  15-JUN-1992   Yi-Cheng Liu ( for Level2 stuff )
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZL2FAUN
      INTEGER HALF,UNIT,LKFAHF,GZL2FAHF
C----------------------------------------------------------------------
      LFAUN(HALF,UNIT) = 0
      LKFAHF=GZL2FAHF(HALF)
      IF ( LKFAHF .NE. 0 ) LFAUN(HALF,UNIT)=LC(LKFAHF-(UNIT+1))
      GZL2FAUN=LFAUN(HALF,UNIT)
C----------------------------------------------------------------------
  999 RETURN
      END
