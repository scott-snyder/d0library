      SUBROUTINE PRFIT2 ( PRUNIT, LFIT2, NFIT2, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'FIT2'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LFIT2  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NFIT2  [I] : Bank number, used only if CFL='ONE' and LFIT2 = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LFIT2 point to a bank, or if <0, NFIT2 is
C-                                  the bank number.
C-                          'LINEAR' : LFIT2 points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   3-SEP-1993 23:29:14.64  Pushpa C. Bhat
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT, LFIT2, NFIT2, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFIT2.LINK'
C----------------------------------------------------------------------
      INTEGER LFIT21, GZFIT2, LZLOC, J
      CHARACTER*8 CFLAG
C----------------------------------------------------------------------
      CFLAG  = CFL(1:LEN(CFL))
      LFIT21 = LFIT2
C
      IF    ( CFLAG .EQ. 'LINEAR' ) THEN
        IF( LFIT2 .LE. 0 ) GOTO 990
      ELSEIF( CFLAG .EQ. 'ONE' ) THEN
        IF( LFIT2 .LE. 0 ) THEN
          IF( NFIT2 .EQ. 0 ) GOTO 980          ! Error exit
          LFIT21 = LZLOC( IXMAIN, 'FIT2', NFIT2 )
        ENDIF
      ELSEIF((CFLAG .EQ. 'ALL') .OR.
     &       (CFLAG .EQ. ' '  ) ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LFIT21 = GZFIT2()
      ELSE
        WRITE( PRUNIT, 1000 ) CFLAG
 1000   FORMAT(/' ** PRFIT2 ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
C
C ****  Check address
C
      IF ( LFIT21 .LE. 0 ) THEN
        WRITE( PRUNIT, 1005 )
 1005   FORMAT(/' No FIT2 bank FOUND')
        GOTO 999
      ENDIF
C
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LFIT21
C
      WRITE( PRUNIT, 1100 ) ( IQ( LFIT21 + J ) , J = 1, IQ( LFIT21-1) )
 1100 FORMAT(/' to be defined, that''s your job...'/)
C
C  ***  Look if another bank is needed
C
      IF( CFLAG .EQ. 'ONE' ) GOTO 999
C
      IF( CFLAG .EQ. 'LINEAR' ) THEN
        LFIT21 = LQ( LFIT21 )
        IF( LFIT21 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LFIT21 = GZFIT2()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 CONTINUE
      WRITE( PRUNIT, 2000 ) LFIT2
 2000 FORMAT(/' ** PRFIT2 ** called for LINEAR without valid bank ',
     &        'pointer, LFIT2 =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 CONTINUE
      WRITE( PRUNIT, 2100 ) LFIT2, NFIT2
 2100 FORMAT(/'  ** PRFIT2 ** called for ONE without bank pointer and ',
     &        'bank number, LFIT2 =',I10,' NFIT2 =', I10/)
      GOTO 999
      END
