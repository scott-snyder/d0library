      FUNCTION GZFAQD(HALF,QUAD)
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
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZFAQD
      INTEGER HALF,QUAD,LKFAUN,GZFAUN
C----------------------------------------------------------------------
      IF(LFAQD(HALF,QUAD).EQ.0) THEN         ! link not set
        LKFAUN=GZFAUN(HALF,0)
        IF ( LKFAUN .NE. 0 ) LFAQD(HALF,QUAD)= LC(LKFAUN-(QUAD+1))
        GZFAQD=LFAQD(HALF,QUAD)
      ELSE                              ! link set
        GZFAQD=LFAQD(HALF,QUAD)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
