      SUBROUTINE PRRECB ( PRUNIT, LRECB, NRECB, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'RECB'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LRECB  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NRECB  [I] : Bank number, used only if CFL='ONE' and LRECB = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LRECB point to a bank, or if <0, NRECB is
C-                                  the bank number.
C-                          'LINEAR' : LRECB points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  26-JUL-1990 10:27:01.49  Chip Stewart
C-   Updated  20-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRECB.LINK'
C
      INTEGER PRUNIT, LRECB, NRECB, IFL
      CHARACTER*(*) CFL
      INTEGER LRECB1, GZRECB, LZLOC, J
C----------------------------------------------------------------------
      LRECB1 = LRECB
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LRECB .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LRECB .LE. 0 ) THEN
          IF( NRECB .EQ. 0 ) GOTO 980          ! Error exit
          LRECB1 = LZLOC( IXDVR, 'RECB', NRECB )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LRECB1 = GZRECB( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRRECB ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LRECB1
C
      WRITE( PRUNIT, 1100 ) ( IQ( LRECB1 + J ) , J = 1, IQ( LRECB1-1) )
 1100 FORMAT(/' RECB contents to be defined ', /10I8)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LRECB1 = LQ( LRECB1 )
        IF( LRECB1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LRECB1 = GZRECB()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LRECB
 2000 FORMAT(/' ** PRRECB ** called for LINEAR without valid bank ',
     &        'pointer, LRECB =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LRECB, NRECB
 2100 FORMAT(/'  ** PRRECB ** called for ONE without bank pointer and ',
     &        'bank number, LRECB =',I10,' NRECB =', I10/)
      GOTO 999
      END
