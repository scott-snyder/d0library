      FUNCTION GZFPHF(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FPHF 
C-
C-   Returned value  : 
C-   Inputs  : HALF
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
      INTEGER GZFPHF
      INTEGER HALF,GZFPDH,LKFPDH
C----------------------------------------------------------------------
      IF(LFPHF(HALF).EQ.0) THEN         ! link not set
        LKFPDH=GZFPDH()
        IF ( LKFPDH .NE. 0 ) LFPHF(HALF)=LC(LKFPDH-(HALF+1))
        GZFPHF=LFPHF(HALF)
      ELSE                              ! link set
        GZFPHF=LFPHF(HALF)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
