      FUNCTION GZFPSE(HALF,UNIT,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FPSE
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
      INTEGER GZFPSE
      INTEGER HALF,UNIT,QUAD,SECTOR,LKFPQD,GZFPQD,LKFPUN,GZFPUN
C----------------------------------------------------------------------
      IF(UNIT.LE.0) THEN
        IF(LFPSE(HALF,UNIT,QUAD,SECTOR).EQ.0) THEN         ! link not set
          LKFPQD=GZFPQD(HALF,QUAD)
          IF ( ( LKFPQD .NE. 0 ) .AND. ( SECTOR .LE. 5 ) ) THEN
            LFPSE(HALF,UNIT,QUAD,SECTOR)= LC(LKFPQD-(SECTOR+1))
          ENDIF
          GZFPSE=LFPSE(HALF,UNIT,QUAD,SECTOR)
        ELSE                              ! link set
          GZFPSE=LFPSE(HALF,UNIT,QUAD,SECTOR)
        ENDIF
      ELSE
        IF(LFPSE(HALF,UNIT,QUAD,SECTOR).EQ.0) THEN         ! link not set
          LKFPUN=GZFPUN(HALF,UNIT)
          IF ( LKFPUN .NE. 0 ) LFPSE(HALF,UNIT,QUAD,SECTOR)= 
     &                                 LC(LKFPUN-(SECTOR+1))
          GZFPSE=LFPSE(HALF,UNIT,QUAD,SECTOR)
        ELSE                              ! link set
          GZFPSE=LFPSE(HALF,UNIT,QUAD,SECTOR)
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
