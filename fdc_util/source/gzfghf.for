      FUNCTION GZFGHF(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGHF 
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
      INTEGER GZFGHF
      INTEGER HALF,GZFGNH,LKFGNH
C----------------------------------------------------------------------
      IF(LFGHF(HALF).EQ.0) THEN         ! link not set
        LKFGNH=GZFGNH()
        IF ( LKFGNH .NE. 0 ) LFGHF(HALF)=LC(LKFGNH-(HALF+1))
        GZFGHF=LFGHF(HALF)
      ELSE                              ! link set
        GZFGHF=LFGHF(HALF)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
