      SUBROUTINE PRCCPC ( PRUNIT, LCCPC, NCCPC, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CCPC'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCCPC [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NCCPC  [I] : NOT USED
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LCCPCpoint to a bank, or if <0, NCCPCis
C-                                  the bank number.
C-                          'LINEAR' : LCCPCpoints to the first bank of the
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
      INTEGER PRUNIT, LCCPC, NCCPC, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      IF( CFL .EQ. 'ONE' ) THEN
        IF( LCCPC.LE. 0 ) THEN
          CALL ERRMSG(' BANK NUMBER NOT USED ','PRCCPC',
     &      ' ASKED FOR ''ONE'' IN LINEAR CHAIN','W')
        ENDIF
      ENDIF
      CALL PRCCP (PRUNIT, LCCPC, 1, CFL, IFL )
      END
