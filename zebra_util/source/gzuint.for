      INTEGER FUNCTION GZUINT()
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Return the pointer to the UINT bank
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-Oct-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUINT.LINK'
      INTEGER LANLS
      INTEGER GZANLS
C------- ----------------------------------------------------------------
C
      GZUINT=0
      LANLS=GZANLS()
      IF( LANLS.LE.0 ) GOTO 999
      GZUINT = LQ(LANLS-IZUINT)
C
 999  RETURN
      END


