      SUBROUTINE PRQUAN ( PRUNIT, LQUAN, NQUAN, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'QUAN'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LQUAN  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NQUAN  [I] : Bank number, used only if CFL='ONE' and LQUAN = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LQUAN point to a bank, or if <0, NQUAN is
C-                                  the bank number.
C-                          'LINEAR' : LQUAN points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  22-DEC-1990 14:56:34.54  Rajendran Raja
C-   Updated  20-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZQUAN.LINK'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
C
      INTEGER PRUNIT, LQUAN, NQUAN, IFL
      CHARACTER*(*) CFL
      INTEGER  J
C----------------------------------------------------------------------
C
C  ***  Print the content of the bank pointed by LQUAN
C
      WRITE( PRUNIT, 1100 ) (J,VISIBLE_QUANTITIES(J),
     &   C( LQUAN + J ) , J = 1, VIS_DIM )
 1100 FORMAT(/' Print of HMATRIX bank QUAN . Visible quantities ',/,
     & (2X,I5,2X,A32,2X,F15.7) )
C
      WRITE( PRUNIT, 1101 ) (J,INVISIBLE_QUANTITIES(J),
     &   C( LQUAN + J+VIS_DIM ) , J = 1, INVIS_DIM )
 1101 FORMAT(/' Print of HMATRIX bank QUAN . Invisible quantities ',/,
     & (2X,I5,2X,A32,2X,F15.7) )
C
      RETURN
      END
