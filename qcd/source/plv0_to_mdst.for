      SUBROUTINE PLV0_TO_MDST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find PLV0 bank and copy under MDST bank
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
      INTEGER GZPLV0, LPLV0, GZMDST, LMDST
C----------------------------------------------------------------------
      LMDST = GZMDST()
      LPLV0 = GZPLV0()
      IF ( LMDST.LE.0 .OR. LPLV0.LE.0 ) RETURN
      CALL MZCOPY( IXMAIN, LPLV0,  IXMAIN, LMDST, -2, ' ') 
  999 RETURN
      END
