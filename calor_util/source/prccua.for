      SUBROUTINE PRCCUA ( PRUNIT, LCCUA, NCCUA, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CCUA'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCCUA [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NCCUA  [I] : NOT USED
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LCCUApoint to a bank, or if <0, NCCUAis
C-                                  the bank number.
C-                          'LINEAR' : LCCUApoints to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   31-JUL-1991 Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER PRUNIT, LCCUA, NCCUA, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      IF( CFL .EQ. 'ONE' ) THEN
        IF( LCCUA.LE. 0 ) THEN
          CALL ERRMSG(' BANK NUMBER NOT USED ','PRCCUA',
     &      ' ASKED FOR ''ONE'' IN LINEAR CHAIN','W')
        ENDIF
      ENDIF
      CALL PRCCP (PRUNIT, LCCUA, 1, CFL, IFL )
      END
