      FUNCTION GZL2FASE(HALF,UNIT,QUAD,SECTOR)
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
C-    Updated  15-JUN-1992  Yi-Cheng Liu ( for Level2 stuff )
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZL2FASE
      INTEGER HALF,UNIT,QUAD,SECTOR,LKFAQD,GZL2FAQD,LKFAUN,GZL2FAUN
C----------------------------------------------------------------------
      LFASE(HALF,UNIT,QUAD,SECTOR) = 0
      IF(UNIT.EQ.0) THEN
          LKFAQD=GZL2FAQD(HALF,QUAD)
          IF ( ( LKFAQD .NE. 0 ) .AND. ( SECTOR .LE. 5 ) ) THEN
            LFASE(HALF,UNIT,QUAD,SECTOR)= LC(LKFAQD-(SECTOR+1))
          ENDIF
          GZL2FASE=LFASE(HALF,UNIT,QUAD,SECTOR)
      ELSE
          LKFAUN=GZL2FAUN(HALF,UNIT)
          IF ( LKFAUN .NE. 0 ) LFASE(HALF,UNIT,QUAD,SECTOR)=
     &                                 LC(LKFAUN-(SECTOR+1))
          GZL2FASE=LFASE(HALF,UNIT,QUAD,SECTOR)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
