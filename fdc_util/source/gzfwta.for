      FUNCTION GZFWTA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FWTA 
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
      INCLUDE 'D0$LINKS:IZFWTA.LINK'
      INTEGER GZFWTA
      INTEGER GZFWAL, LKFWAL
C----------------------------------------------------------------------
      IF(LFWTA.EQ.0) THEN               ! link not set
        LKFWAL=GZFWAL()
        IF ( LKFWAL .NE. 0 ) LFWTA=LC(LKFWAL-IZFWTA)
        GZFWTA=LFWTA
      ELSE                              ! link set
        GZFWTA=LFWTA
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
