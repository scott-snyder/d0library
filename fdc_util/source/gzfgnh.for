      FUNCTION GZFGNH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGNH 
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
      INCLUDE 'D0$LINKS:IZFGNH.LINK'
      INTEGER GZFGNH
      INTEGER GZSFDC, LKSFDC
C----------------------------------------------------------------------
      IF(LFGNH.EQ.0) THEN               ! link not set
        LKSFDC=GZSFDC()
        IF ( LKSFDC .NE. 0 ) LFGNH=LC(LKSFDC-IZFGNH)
        GZFGNH=LFGNH
      ELSE                              ! link set
        GZFGNH=LFGNH
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
