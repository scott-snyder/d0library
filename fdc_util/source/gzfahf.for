      FUNCTION GZFAHF(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FAHF 
C-
C-   Returned value  : 
C-   Inputs  : HALF
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeff Bantly  use logical format 
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZFAHF
      INTEGER HALF,GZFALH,LKFALH
C----------------------------------------------------------------------
      IF(LFAHF(HALF).EQ.0) THEN         ! link not set
        LKFALH=GZFALH()
        IF ( LKFALH .NE. 0 ) LFAHF(HALF)=LC(LKFALH-(HALF+1))
        GZFAHF=LFAHF(HALF)
      ELSE                              ! link set
        GZFAHF=LFAHF(HALF)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
