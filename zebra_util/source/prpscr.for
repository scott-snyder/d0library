      SUBROUTINE PRPSCR( PRUNIT, LPSCR, NPSCR, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints on unit PRUNIT the content of the bank PSCR
C-   pointed by LPSCR. Other arguments are ignored
C-
C-   Inputs  : PRUNIT [I] : Fortran unit number for output
C-             LPSCR  [I] : Pointer on the PSCR bank
C-   Outputs :
C-   Controls:
C-
C-   Created  18-OCT-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBPIX.INC'
      INTEGER PRUNIT, LPSCR, NPSCR, IFL, J
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      WRITE( PRUNIT, 1000 ) (IP(LPSCR+J), J=1,5 )
 1000 FORMAT(//' We have ',I3,' viewport(s) in the screen "', 4A4, '"' )
  999 RETURN
      END
