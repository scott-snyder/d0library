      FUNCTION GZL2FTSE(HALF,UNIT,QUAD,SECTOR)
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
      INTEGER GZL2FTSE
      INTEGER HALF,UNIT,QUAD,SECTOR,L2FTQU,GZL2FTQU,L2FTUN,GZL2FTUN
C----------------------------------------------------------------------
      LFTSE(HALF,UNIT,QUAD,SECTOR) = 0
C
      IF(UNIT.LE.0) THEN
          L2FTQU=GZL2FTQU(HALF,QUAD)
          IF ( ( L2FTQU .NE. 0 ) .AND. ( SECTOR .LE. 5 ) ) THEN
            LFTSE(HALF,UNIT,QUAD,SECTOR)= LC(L2FTQU-(SECTOR+1))
          ENDIF
          GZL2FTSE=LFTSE(HALF,UNIT,QUAD,SECTOR)
      ELSE
          L2FTUN=GZL2FTUN(HALF,UNIT)
          IF ( L2FTUN .NE. 0 ) LFTSE(HALF,UNIT,QUAD,SECTOR)=
     &                                 LC(L2FTUN-(SECTOR+1))
          GZL2FTSE=LFTSE(HALF,UNIT,QUAD,SECTOR)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
