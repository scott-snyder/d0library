      FUNCTION GZFDRT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank  
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
      INCLUDE 'D0$LINKS:IZFDRT.LINK'
      INTEGER GZFDRT
      INTEGER GZFGEH, LKFGEH
C----------------------------------------------------------------------
      IF(LFDRT.EQ.0) THEN               ! link not set
        LKFGEH=GZFGEH()
        IF ( LKFGEH .NE. 0 ) LFDRT=LC(LKFGEH-IZFDRT)
        GZFDRT=LFDRT
      ELSE                              ! link set
        GZFDRT=LFDRT
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
