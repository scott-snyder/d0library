      FUNCTION GZFTQU(HALF,QUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTQU 
C-
C-   Returned value  : 
C-   Inputs  : HALF,  QUAD
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
      INTEGER GZFTQU
      INTEGER HALF,QUAD,LKFTUN,GZFTUN
C----------------------------------------------------------------------
      IF(LFTQU(HALF,QUAD).EQ.0) THEN         ! link not set
        LKFTUN=GZFTUN(HALF,0)
        IF ( LKFTUN .NE. 0 ) LFTQU(HALF,QUAD)= LC(LKFTUN-(QUAD+1))
        GZFTQU=LFTQU(HALF,QUAD)
      ELSE                              ! link set
        GZFTQU=LFTQU(HALF,QUAD)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
