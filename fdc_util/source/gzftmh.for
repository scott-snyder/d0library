      FUNCTION GZFTMH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FTMH 
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
      INCLUDE 'D0$LINKS:IZFTMH.LINK'
      INTEGER GZFTMH
      INTEGER GZSFDC, LKSFDC
C----------------------------------------------------------------------
      IF(LFTMH.EQ.0) THEN               ! link not set
        LKSFDC=GZSFDC()
        IF ( LKSFDC .NE. 0 ) LFTMH=LC(LKSFDC-IZFTMH)
        GZFTMH=LFTMH
      ELSE                              ! link set
        GZFTMH=LFTMH
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
