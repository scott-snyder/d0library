      FUNCTION GZL2FAQD(HALF,QUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FAQD 
C-
C-   Returned value  : 
C-   Inputs  : HALF, QUAD
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
      INTEGER GZL2FAQD
      INTEGER HALF,QUAD,LKFAUN,GZL2FAUN
C----------------------------------------------------------------------
      LFAQD(HALF,QUAD) = 0
      LKFAUN=GZL2FAUN(HALF,0)
      IF ( LKFAUN .NE. 0 ) LFAQD(HALF,QUAD)= LC(LKFAUN-(QUAD+1))
      GZL2FAQD=LFAQD(HALF,QUAD)
C----------------------------------------------------------------------
  999 RETURN
      END
