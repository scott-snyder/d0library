      FUNCTION GZFTHF(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTHF 
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
      INTEGER GZFTHF
      INTEGER HALF,GZFTMH,LKFTMH
C----------------------------------------------------------------------
      IF(LFTHF(HALF).EQ.0) THEN         ! link not set
        LKFTMH=GZFTMH()
        IF ( LKFTMH .NE. 0 ) LFTHF(HALF)=LC(LKFTMH-(HALF+1))
        GZFTHF=LFTHF(HALF)
      ELSE                              ! link set
        GZFTHF=LFTHF(HALF)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
