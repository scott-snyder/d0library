      INTEGER FUNCTION GZCAID()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find pointer to CAID.
C-
C-   Created   8-APR-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAID.LINK'
      INTEGER GZPROC, LPROC, LCAID
C----------------------------------------------------------------------
      LPROC = GZPROC ()
      LCAID = 0
      IF ( LPROC .GT. 0 ) LCAID = LQ (LPROC - IZCAID)
      GZCAID = LCAID
C
      RETURN
      END
