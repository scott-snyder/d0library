      FUNCTION GZFGUN(HALF,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGUN 
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
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZFGUN
      INTEGER HALF,UNIT,LKFGHF,GZFGHF
C----------------------------------------------------------------------
      IF(LFGUN(HALF,UNIT).EQ.0) THEN         ! link not set
        LKFGHF=GZFGHF(HALF)
        IF ( LKFGHF .NE. 0 ) LFGUN(HALF,UNIT)=LC(LKFGHF-(UNIT+1))
        GZFGUN=LFGUN(HALF,UNIT)
      ELSE                              ! link set
        GZFGUN=LFGUN(HALF,UNIT)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
