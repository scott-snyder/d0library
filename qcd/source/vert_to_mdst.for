      SUBROUTINE VERT_TO_MDST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find VERT bank and copy under MDST bank
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-JUL-1993   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZVERH, LVERH, GZVERT, LVERT, GZMDST, LMDST
C----------------------------------------------------------------------
      LMDST = GZMDST()
      LVERH = GZVERH()
      LVERT=LQ(LVERH-1)
      IF ( LMDST.LE.0 .OR. LVERT.LE.0 ) RETURN
      CALL MZCOPY( IXMAIN, LVERT,  IXMAIN, LMDST, -3, 'L') 
  999 RETURN
      END
