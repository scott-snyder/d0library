      SUBROUTINE PRISRC ( PRUNIT, LISRC, NISRC, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'ISRC'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LISRC  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NISRC  [I] : Bank number, used only if CFL='ONE' and LISRC = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LISRC point to a bank, or if <0, NISRC is
C-                                  the bank number.
C-                          'LINEAR' : LISRC points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  11-JAN-1990 16:49:35.86  Chip Stewart
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISRC.LINK'
C
      INTEGER PRUNIT, LISRC, NISRC, IFL
      CHARACTER*(*) CFL
      INTEGER LISRC1, GZISRC, LZLOC, J
C----------------------------------------------------------------------
      LISRC1 = LISRC
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LISRC .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LISRC .LE. 0 ) THEN
          IF( NISRC .EQ. 0 ) GOTO 980          ! Error exit
          LISRC1 = LZLOC( IXMAIN, 'ISRC', NISRC )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LISRC1 = GZISRC( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRISRC ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LISRC1
C
      CALL ISRC_DUMP(PRUNIT, LISRC1)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LISRC1 = LQ( LISRC1 )
        IF( LISRC1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LISRC1 = GZISRC()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LISRC
 2000 FORMAT(/' ** PRISRC ** called for LINEAR without valid bank ',
     &        'pointer, LISRC =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LISRC, NISRC
 2100 FORMAT(/'  ** PRISRC ** called for ONE without bank pointer and ',
     &        'bank number, LISRC =',I10,' NISRC =', I10/)
      GOTO 999
      END
