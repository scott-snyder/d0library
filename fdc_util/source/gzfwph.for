      FUNCTION GZFWPH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FWPH 
C-
C-   Returned value  : 
C-   Inputs  : 
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
      INCLUDE 'D0$LINKS:IZFWPH.LINK'
      INTEGER GZFWPH
      INTEGER GZFWAL, LKFWAL
C----------------------------------------------------------------------
      IF(LFWPH.EQ.0) THEN               ! link not set
        LKFWAL=GZFWAL()
        IF ( LKFWAL .NE. 0 ) LFWPH=LC(LKFWAL-IZFWPH)
        GZFWPH=LFWPH
      ELSE                              ! link set
        GZFWPH=LFWPH
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
