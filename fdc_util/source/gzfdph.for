      FUNCTION GZFDPH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FDPH 
C-
C-   Returned value  : 
C-   Inputs  : none
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFDPH.LINK'
      INTEGER GZFDPH
      INTEGER GZFDRT, LKFDRT
C----------------------------------------------------------------------
      IF(LFDPH.EQ.0) THEN               ! link not set
        LKFDRT=GZFDRT()
        IF ( LKFDRT .NE. 0 ) LFDPH=LC(LKFDRT-IZFDPH)
        GZFDPH=LFDPH
      ELSE                              ! link set
        GZFDPH=LFDPH
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
