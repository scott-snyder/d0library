      SUBROUTINE PRPACT( PRUNIT, LPACT, NPACT, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : prints the PACT bank pointed to by LPACT
C-
C-   Inputs  : PRUNIT [I] : Fortran output unit number
C-             LPACT  [I] : Pointer on the bank
C-             3 other are unused...
C-   Outputs :
C-   Controls:
C-
C-   Created  18-OCT-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBPIX.INC'
      INTEGER PRUNIT, LPACT, NPACT, IFL, J
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      WRITE( PRUNIT, 1000 ) ( IP( LPACT+J ), J=1, IP(LPACT-1) )
 1000 FORMAT(6X,I3,' action routine(s) for this viewport :',
     &       10(2X,A4,A2))
  999 RETURN
      END
