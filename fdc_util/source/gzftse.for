      FUNCTION GZFTSE(HALF,UNIT,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTSE
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
      INTEGER GZFTSE
      INTEGER HALF,UNIT,QUAD,SECTOR,LKFTQU,GZFTQU,LKFTUN,GZFTUN
C----------------------------------------------------------------------
      IF(UNIT.LE.0) THEN
        IF(LFTSE(HALF,UNIT,QUAD,SECTOR).EQ.0) THEN         ! link not set
          LKFTQU=GZFTQU(HALF,QUAD)
          IF ( ( LKFTQU .NE. 0 ) .AND. ( SECTOR .LE. 5 ) ) THEN
            LFTSE(HALF,UNIT,QUAD,SECTOR)= LC(LKFTQU-(SECTOR+1))
          ENDIF
          GZFTSE=LFTSE(HALF,UNIT,QUAD,SECTOR)
        ELSE                              ! link set
          GZFTSE=LFTSE(HALF,UNIT,QUAD,SECTOR)
        ENDIF
      ELSE
        IF(LFTSE(HALF,UNIT,QUAD,SECTOR).EQ.0) THEN         ! link not set
          LKFTUN=GZFTUN(HALF,UNIT)
          IF ( LKFTUN .NE. 0 ) LFTSE(HALF,UNIT,QUAD,SECTOR)=
     &                                 LC(LKFTUN-(SECTOR+1))
          GZFTSE=LFTSE(HALF,UNIT,QUAD,SECTOR)
        ELSE                              ! link set
          GZFTSE=LFTSE(HALF,UNIT,QUAD,SECTOR)
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
