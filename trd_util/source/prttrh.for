      SUBROUTINE PRTTRH ( PRUNIT, LTTRH, NTTRH, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'TTRH'. 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LTTRH  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NTTRH  [I] : Bank number, used only if CFL='ONE' and LTTRH = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LTTRH point to a bank, or if <0, NTTRH is
C-                                  the bank number.
C-                          'LINEAR' : LTTRH points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  27-OCT-1989 18:42:05.09  A. Zylberstej
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTTRH.LINK'
C
      INTEGER PRUNIT, LTTRH, NTTRH, IFL
      CHARACTER*(*) CFL
      INTEGER LTTRH1, GZTTRH, LZLOC, J
C----------------------------------------------------------------------
      IF(PRUNIT.LE.0)GO TO 999
      LTTRH1 = LTTRH
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LTTRH .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LTTRH .LE. 0 ) THEN
          IF( NTTRH .EQ. 0 ) GOTO 980          ! Error exit
          LTTRH1 = LZLOC( IXMAIN, 'TTRH', NTTRH )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LTTRH1 = GZTTRH( )
      ELSE
        WRITE( PRUNIT, 1000 ) CFL
1000    FORMAT(/' ** PRTTRH ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LTTRH1
C
      WRITE( PRUNIT, 1100 ) ( IQ( LTTRH1 + J ) , J = 1, IQ( LTTRH1-1) )
 1100 FORMAT(/' to be defined, that''s your job...'/)
C
C  ***  Look if another bank is needed
C
      IF( CFL .EQ. 'ONE' ) GOTO 999
      IF( CFL .EQ. 'LINEAR' ) THEN
        LTTRH1 = LQ( LTTRH1 )
        IF( LTTRH1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command. 
C
        LTTRH1 = GZTTRH()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 ) LTTRH
 2000 FORMAT(/' ** PRTTRH ** called for LINEAR without valid bank ',
     &        'pointer, LTTRH =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 WRITE( PRUNIT, 2100 ) LTTRH, NTTRH
 2100 FORMAT(/'  ** PRTTRH ** called for ONE without bank pointer and ',
     &        'bank number, LTTRH =',I10,' NTTRH =', I10/)
      GOTO 999
      END

