      SUBROUTINE PRVCOR ( PRUNIT, LVCOR, NVCOR, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'VCOR'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LVCOR  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NVCOR  [I] : Bank number, used only if CFL='ONE' and LVCOR = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LVCOR point to a bank, or if <0, NVCOR is
C-                                  the bank number.
C-                          'LINEAR' : LVCOR points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  17-NOV-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT, LVCOR, NVCOR, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVCOR.LINK'
C----------------------------------------------------------------------
      INTEGER LVCOR1, GZVCOR, LZLOC, J
      CHARACTER*8 CFLAG
C----------------------------------------------------------------------
      CFLAG  = CFL(1:LEN(CFL))
      LVCOR1 = LVCOR
C
      IF    ( CFLAG .EQ. 'LINEAR' ) THEN
        IF( LVCOR .LE. 0 ) GOTO 990
      ELSEIF( CFLAG .EQ. 'ONE' ) THEN
        IF( LVCOR .LE. 0 ) THEN
          IF( NVCOR .EQ. 0 ) GOTO 980          ! Error exit
          LVCOR1 = LZLOC( IXMAIN, 'VCOR', NVCOR )
        ENDIF
      ELSEIF((CFLAG .EQ. 'ALL') .OR.
     &       (CFLAG .EQ. ' '  ) ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LVCOR1 = GZVCOR()
      ELSE
        WRITE( PRUNIT, 1000 ) CFLAG
 1000   FORMAT(/' ** PRVCOR ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
C
C ****  Check address
C
      IF ( LVCOR1 .LE. 0 ) THEN
        WRITE( PRUNIT, 1005 )
 1005   FORMAT(/' No VCOR bank FOUND')
        GOTO 999
      ENDIF
C
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LVCOR1
C
      WRITE( PRUNIT, 1100 ) ( IQ( LVCOR1 + J ) , J = 1, IQ( LVCOR1-1) )
 1100 FORMAT(/' to be defined, that''s your job...'/)
C
C  ***  Look if another bank is needed
C
      IF( CFLAG .EQ. 'ONE' ) GOTO 999
C
      IF( CFLAG .EQ. 'LINEAR' ) THEN
        LVCOR1 = LQ( LVCOR1 )
        IF( LVCOR1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LVCOR1 = GZVCOR()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 CONTINUE
      WRITE( PRUNIT, 2000 ) LVCOR
 2000 FORMAT(/' ** PRVCOR ** called for LINEAR without valid bank '
     &,        'pointer, LVCOR =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 CONTINUE
      WRITE( PRUNIT, 2100 ) LVCOR, NVCOR
 2100 FORMAT(/'  ** PRVCOR ** called for ONE without bank pointer and '
     &,        'bank number, LVCOR =',I10,' NVCOR =', I10/)
      GOTO 999
      END
