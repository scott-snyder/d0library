      SUBROUTINE PRXXXX ( PRUNIT, LXXXX, NXXXX, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'XXXX'. 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LXXXX  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NXXXX  [I] : Bank number, used only if CFL='ONE' and LXXXX = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LXXXX point to a bank, or if <0, NXXXX is
C-                                  the bank number.
C-                          'LINEAR' : LXXXX points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  XDATE  XAUTHOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT, LXXXX, NXXXX, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZXXXX.LINK'
C----------------------------------------------------------------------
      INTEGER LXXXX1, GZXXXX, LZLOC, J
      CHARACTER*8 CFLAG
C----------------------------------------------------------------------
      CFLAG  = CFL(1:LEN(CFL))
      LXXXX1 = LXXXX
C
      IF    ( CFLAG .EQ. 'LINEAR' ) THEN
        IF( LXXXX .LE. 0 ) GOTO 990
      ELSEIF( CFLAG .EQ. 'ONE' ) THEN
        IF( LXXXX .LE. 0 ) THEN
          IF( NXXXX .EQ. 0 ) GOTO 980          ! Error exit
          LXXXX1 = LZLOC( IXMAIN, 'XXXX', NXXXX )
        ENDIF
      ELSEIF((CFLAG .EQ. 'ALL') .OR.
     &       (CFLAG .EQ. ' '  ) ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LXXXX1 = GZXXXX()
      ELSE
        WRITE( PRUNIT, 1000 ) CFLAG
 1000   FORMAT(/' ** PRXXXX ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
C
C ****  Check address
C
      IF ( LXXXX1 .LE. 0 ) THEN
        WRITE( PRUNIT, 1005 )
 1005   FORMAT(/' No XXXX bank FOUND')
        GOTO 999
      ENDIF
C
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LXXXX1
C
      WRITE( PRUNIT, 1100 ) ( IQ( LXXXX1 + J ) , J = 1, IQ( LXXXX1-1) )
 1100 FORMAT(/' to be defined, that''s your job...'/)
C
C  ***  Look if another bank is needed
C
      IF( CFLAG .EQ. 'ONE' ) GOTO 999
C
      IF( CFLAG .EQ. 'LINEAR' ) THEN
        LXXXX1 = LQ( LXXXX1 )
        IF( LXXXX1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command. 
C
        LXXXX1 = GZXXXX()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 CONTINUE
      WRITE( PRUNIT, 2000 ) LXXXX
 2000 FORMAT(/' ** PRXXXX ** called for LINEAR without valid bank '
     &        'pointer, LXXXX =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 CONTINUE
      WRITE( PRUNIT, 2100 ) LXXXX, NXXXX
 2100 FORMAT(/'  ** PRXXXX ** called for ONE without bank pointer and '
     &        'bank number, LXXXX =',I10,' NXXXX =', I10/)
      GOTO 999
      END
