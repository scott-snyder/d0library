      FUNCTION GZFGSE(HALF,UNIT,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGSE
C-
C-   Returned value  :
C-   Inputs  : HALF, UNIT, QUAD, SECTOR
C-   Outputs :
C-   Controls:
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  22-FEB-1991   Robert E. Avery  Check for valid theta sector
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZFGSE
      INTEGER HALF,UNIT,QUAD,SECTOR,LKFGQD,GZFGQD,LKFGUN,GZFGUN
C----------------------------------------------------------------------
      IF(UNIT.LE.0) THEN
        IF(LFGSE(HALF,UNIT,QUAD,SECTOR).EQ.0) THEN         ! link not set
          LKFGQD=GZFGQD(HALF,QUAD)
          IF ( ( LKFGQD .NE. 0 ) .AND. ( SECTOR .LE. 5 ) ) THEN
            LFGSE(HALF,UNIT,QUAD,SECTOR)= LC(LKFGQD-(SECTOR+1))
          ENDIF
          GZFGSE=LFGSE(HALF,UNIT,QUAD,SECTOR)
        ELSE                              ! link set
          GZFGSE=LFGSE(HALF,UNIT,QUAD,SECTOR)
        ENDIF
      ELSE
        IF(LFGSE(HALF,UNIT,QUAD,SECTOR).EQ.0) THEN         ! link not set
          LKFGUN=GZFGUN(HALF,UNIT)
          IF ( LKFGUN .NE. 0 ) LFGSE(HALF,UNIT,QUAD,SECTOR)= 
     &                                 LC(LKFGUN-(SECTOR+1))
          GZFGSE=LFGSE(HALF,UNIT,QUAD,SECTOR)
        ELSE                              ! link set
          GZFGSE=LFGSE(HALF,UNIT,QUAD,SECTOR)
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
