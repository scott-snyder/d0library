      SUBROUTINE PRCCPH ( PRUNIT, LCCPH, NCCPH, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CCPH'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LCCPH  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NCCPH  [I] : Bank number, used only if CFL='ONE' and LCCPH = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LCCPH point to a bank, or if <0, NCCPH is
C-                                  the bank number.
C-                          'LINEAR' : LCCPH points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   5-JUL-1990 17:59:21.21  Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCCPH.LINK'
C
      INTEGER PRUNIT, LCCPH, NCCPH, IFL
      CHARACTER*(*) CFL
      INTEGER LCCPH1, GZCCPH, LZLOC, J
C----------------------------------------------------------------------
      LCCPH1 = LCCPH
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LCCPH .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LCCPH .LE. 0 ) THEN
          IF( NCCPH .EQ. 0 ) GOTO 980          ! Error exit
          LCCPH1 = LZLOC( IXSTP, 'CCPH', NCCPH )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LCCPH1 = GZCCPH( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRCCPH ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LCCPH1
C
      WRITE( PRUNIT, '(//'' CCPH BANK:'')')
      WRITE( PRUNIT, 1100 ) ( IC( LCCPH1 + J ) , J = 1, 5)
 1100 FORMAT(/' VERSION ',I5,/' LOW RUN ',I9,' HIGH RUN ',I9,
     &       /' DATE ',I9.9,' TIME ',I9.9)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LCCPH1 = LC( LCCPH1 )
        IF( LCCPH1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LCCPH1 = GZCCPH()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LCCPH
 2000 FORMAT(/' ** PRCCPH ** called for LINEAR without valid bank '
     &        ,'pointer, LCCPH =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LCCPH, NCCPH
 2100 FORMAT(/'  ** PRCCPH ** called for ONE without bank pointer and '
     &        ,'bank number, LCCPH =',I10,' NCCPH =', I10/)
      GOTO 999
      END
