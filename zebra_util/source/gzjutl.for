      FUNCTION GZJUTL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return zebra pointer to JUTL (QCD Jet Utility bank)
C-
C-   Returned value  : Pointer to JUTL
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  12-DEC-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZJUTL.LINK/LIST'
      INTEGER GZJUTL, GZANLS, LANLS
C----------------------------------------------------------------------
      GZJUTL = 0
      LANLS  = GZANLS()
      IF ( LANLS .LE. 0 ) RETURN
      GZJUTL = LQ( LANLS - IZJUTL )
  999 RETURN
      END
