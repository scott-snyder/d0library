      FUNCTION GZL2FTQU(HALF,QUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTQU 
C-
C-   Returned value  : 
C-   Inputs  : HALF,  QUAD
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-   Updated  15-JUL-1992   Yi-Cheng liu ( for Level2 stuff )
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZL2FTQU
      INTEGER HALF,QUAD,LKFTUN,GZL2FTUN
C----------------------------------------------------------------------
      LFTQU(HALF,QUAD) = 0
      LKFTUN=GZL2FTUN(HALF,0)              ! 
      IF ( LKFTUN .NE. 0 ) LFTQU(HALF,QUAD)= LC(LKFTUN-(QUAD+1))
      GZL2FTQU=LFTQU(HALF,QUAD)
C----------------------------------------------------------------------
  999 RETURN
      END
