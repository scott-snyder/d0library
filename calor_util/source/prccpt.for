       SUBROUTINE PRCCPT ( PRUNIT, LCCPT, NCCPT, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CCPT'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCCPT [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NCCPT  [I] : NOT USED
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LCCPTpoint to a bank, or if <0, NCCPTis
C-                                  the bank number.
C-                          'LINEAR' : LCCPTpoints to the first bank of the
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
      INTEGER PRUNIT, LCCPT, NCCPT, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      IF( CFL .EQ. 'ONE' ) THEN
        IF( LCCPT.LE. 0 ) THEN
          CALL ERRMSG(' BANK NUMBER NOT USED ','PRCCPT',
     &      ' ASKED FOR ''ONE'' IN LINEAR CHAIN','W')
        ENDIF
      ENDIF
      CALL PRCCP (PRUNIT, LCCPT, 1, CFL, IFL )
      END
