      FUNCTION GZFGQD(HALF,QUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGQD 
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
      INTEGER GZFGQD
      INTEGER HALF,QUAD,LKFGUN,GZFGUN
C----------------------------------------------------------------------
      IF(LFGQD(HALF,QUAD).EQ.0) THEN         ! link not set
        LKFGUN=GZFGUN(HALF,0)
        IF ( LKFGUN .NE. 0 ) LFGQD(HALF,QUAD)= LC(LKFGUN-(QUAD+1))
        GZFGQD=LFGQD(HALF,QUAD)
      ELSE                              ! link set
        GZFGQD=LFGQD(HALF,QUAD)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
