      SUBROUTINE PRPVPR( PRUNIT, LPVPR, NPVPR, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints on unit PRUNIT the content of the bank PVPR
C-   pointed by LPVPR. All other arguments are ignored
C-
C-   Inputs  : PRUNIT [I] : Fortran output unit number
C-             LPVPR  [I] : pointer ion the bank to be printed
C-   Outputs :
C-   Controls:
C-
C-   Created  18-OCT-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBPIX.INC'
      INTEGER PRUNIT, LPVPR, NPVPR, IFL, J
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      WRITE( PRUNIT, 1000 ) ( P(LPVPR+J), J=1,8 )
 1000 FORMAT(/' Viewport limits ( x1,x2,y1,y2 )= ',4F10.4/
     &        ' Window   limits ( x1,x2,y1,y2 )= ',4F10.1)
      IF( IP(LPVPR+9) .EQ. 0 ) GOTO 999
      WRITE( PRUNIT, 1100 ) ( P(LPVPR+J), J=10,18)
 1100 FORMAT(10X,'View reference point = ',3F12.4/
     &       10X,'Up vector            = ',3F12.4/
     &       10X,'Camera point         = ',3F12.4 )
  999 RETURN
      END
