      SUBROUTINE PRHMTR ( PRUNIT, LHMTR, NHMTR, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'HMTR'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LHMTR  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NHMTR  [I] : Bank number, used only if CFL='ONE' and LHMTR = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LHMTR point to a bank, or if <0, NHMTR is
C-                                  the bank number.
C-                          'LINEAR' : LHMTR points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  20-DEC-1990 14:17:30.58  Rajendran Raja
C-   Updated  20-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZHMTR.LINK'
C
      INTEGER PRUNIT, LHMTR, NHMTR, IFL
      CHARACTER*(*) CFL
      INTEGER LHMTR1, GZHMTR, LZLOC, J
C----------------------------------------------------------------------
      LHMTR1 = LHMTR
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LHMTR .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LHMTR .LE. 0 ) THEN
          IF( NHMTR .EQ. 0 ) GOTO 980          ! Error exit
          LHMTR1 = LZLOC( IDVSTP, 'HMTR', NHMTR )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LHMTR1 = GZHMTR( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRHMTR ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LHMTR1
C
      WRITE( PRUNIT, 1100 ) ( IC( LHMTR1 + J ) , J = 1, IC( LHMTR1-1) )
 1100 FORMAT(/' Print of HMTR header bank for Hmatrix '//
     &  ' Version number = ',I8)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LHMTR1 = LC( LHMTR1 )
        IF( LHMTR1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LHMTR1 = GZHMTR()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LHMTR
 2000 FORMAT(/' ** PRHMTR ** called for LINEAR without valid bank ',
     &        'pointer, LHMTR =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LHMTR, NHMTR
 2100 FORMAT(/'  ** PRHMTR ** called for ONE without bank pointer and ',
     &        'bank number, LHMTR =',I10,' NHMTR =', I10/)
      GOTO 999
      END
