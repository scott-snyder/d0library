      FUNCTION GZFPQD(HALF,QUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FPQD 
C-
C-   Returned value  : 
C-   Inputs  : HALF, QUAD
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZFPQD
      INTEGER HALF,QUAD,LKFPUN,GZFPUN
C----------------------------------------------------------------------
      IF(LFPQD(HALF,QUAD).EQ.0) THEN         ! link not set
        LKFPUN=GZFPUN(HALF,0)
        IF ( LKFPUN .NE. 0 ) LFPQD(HALF,QUAD)= LC(LKFPUN-(QUAD+1))
        GZFPQD=LFPQD(HALF,QUAD)
      ELSE                              ! link set
        GZFPQD=LFPQD(HALF,QUAD)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
