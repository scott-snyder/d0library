      FUNCTION GZFASE(HALF,UNIT,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FASE
C-
C-   Returned value  :
C-   Inputs  : HALF, UNIT, QUAD, SECTOR
C-   Outputs :
C-   Controls:
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeff Bantly  use logical format 
C-   Updated  22-FEB-1991   Robert E. Avery  Check for valid theta sector
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-   Updated  14-AUG-1992   Susan K. Blessing  Rearrange logic to speed up. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZFASE
      INTEGER HALF,UNIT,QUAD,SECTOR,LKFAQD,GZFAQD,LKFAUN,GZFAUN
C----------------------------------------------------------------------
C
      IF (LFASE(HALF,UNIT,QUAD,SECTOR).EQ.0) THEN
C
        IF (UNIT.EQ.0) THEN
          LKFAQD = GZFAQD(HALF,QUAD)
          IF ( ( LKFAQD .NE. 0 ) .AND. ( SECTOR .LE. 5 ) ) THEN
            LFASE(HALF,UNIT,QUAD,SECTOR) = LC(LKFAQD-(SECTOR+1))
          ENDIF
          GZFASE = LFASE(HALF,UNIT,QUAD,SECTOR)
C
        ELSE
          LKFAUN = GZFAUN(HALF,UNIT)
          IF ( LKFAUN .NE. 0 ) 
     &      LFASE(HALF,UNIT,QUAD,SECTOR) = LC(LKFAUN-(SECTOR+1))
          GZFASE = LFASE(HALF,UNIT,QUAD,SECTOR)
C
        END IF
C
      ELSE
          GZFASE = LFASE(HALF,UNIT,QUAD,SECTOR)
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
