      SUBROUTINE PRDIAG ( PRUNIT, LDIAG, NDIAG, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'DIAG'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LDIAG  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NDIAG  [I] : Bank number, used only if CFL='ONE' and LDIAG = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LDIAG point to a bank, or if <0, NDIAG is
C-                                  the bank number.
C-                          'LINEAR' : LDIAG points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-   Updated  20-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDIAG.LINK'
C
      INTEGER PRUNIT, LDIAG, NDIAG, IFL
      CHARACTER*(*) CFL
      INTEGER LDIAG1, GZDIAG, LZLOC, J
C----------------------------------------------------------------------
      LDIAG1 = LDIAG
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LDIAG .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LDIAG .LE. 0 ) THEN
          IF( NDIAG .EQ. 0 ) GOTO 980          ! Error exit
          LDIAG1 = LZLOC( IDVSTP, 'DIAG', NDIAG )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LDIAG1 = GZDIAG( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRDIAG ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LDIAG1
C
      WRITE( PRUNIT, 1100 ) ( IC( LDIAG1 + J ) , J = 1, IC( LDIAG1-1) )
 1100 FORMAT(/' to be defined, that''s your job...'/)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LDIAG1 = LC( LDIAG1 )
        IF( LDIAG1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LDIAG1 = GZDIAG()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LDIAG
 2000 FORMAT(/' ** PRDIAG ** called for LINEAR without valid bank ',
     &        'pointer, LDIAG =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LDIAG, NDIAG
 2100 FORMAT(/'  ** PRDIAG ** called for ONE without bank pointer and ',
     &        'bank number, LDIAG =',I10,' NDIAG =', I10/)
      GOTO 999
      END
