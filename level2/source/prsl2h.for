      SUBROUTINE PRSL2H ( PRUNIT, LSL2H, NSL2H, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'SL2H'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LSL2H  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NSL2H  [I] : Bank number, used only if CFL='ONE' and LSL2H = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LSL2H point to a bank, or if <0, NSL2H is
C-                                  the bank number.
C-                          'LINEAR' : LSL2H points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  25-NOV-90         Richard V Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSL2H.LINK'
C
      INTEGER PRUNIT, LSL2H, NSL2H, IFL
      CHARACTER*(*) CFL
      CHARACTER*4 BNAME
      INTEGER LSL2H1, GZSL2H, LZLOC, J,LSUP,I,INAME
C----------------------------------------------------------------------
      LSL2H1 = LSL2H
      IF( CFL .EQ. 'LINEAR' ) THEN
        IF( LSL2H .LE. 0 ) GOTO 990
      ELSEIF( CFL .EQ. 'ONE' ) THEN
        IF( LSL2H .LE. 0 ) THEN
          IF( NSL2H .EQ. 0 ) GOTO 980          ! Error exit
          LSL2H1 = LZLOC( IXMAIN, 'SL2H', NSL2H )
        ENDIF
      ELSEIF( CFL .EQ. 'ALL' ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LSL2H1 = GZSL2H( )
      ELSE
        WRITE( PRUNIT, 1000 ) 
 1000   FORMAT(' ** PRSL2H ** Illegal value of CFL')
        GOTO 999
      ENDIF
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LSL2H1
C
      WRITE(PRUNIT, *) ' Dumping header bank: SL2H  Version number = '
     & , IC(LSL2H1+ 1)
C
C--- Which links are used?
C
      DO I = 1,IC(LSL2H1 - 3)
        LSUP = LC(LSL2H1 - I)
        IF (LSUP .GT. 0) THEN
          INAME = IC(LSUP - 4)            ! GET BANK NAME
          CALL UHTOC(INAME,4,BNAME,4)
          WRITE(PRUNIT, *) BNAME, ' has DATA '
          END IF
        END DO


C
C  ***  Look if another bank is needed
C
        IF( CFL .EQ. 'ONE' ) GOTO 999
        IF( CFL .EQ. 'LINEAR' ) THEN
          LSL2H1 = LQ( LSL2H1 )
          IF( LSL2H1 .NE. 0 ) GOTO 1
        ELSE
C
C ****  Find the next bank for the ALL command.
C
          LSL2H1 = GZSL2H()
        ENDIF
  999   RETURN
C
C  *** Error : Linear without bank pointer
C
  990   WRITE( PRUNIT, 2000 ) LSL2H
 2000   FORMAT(' ** PRSL2H ** called for LINEAR without valid bank ',
     &        'pointer, LSL2H =',I10)
        GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980   WRITE( PRUNIT, 2100 ) LSL2H, NSL2H
 2100   FORMAT(
     &    '  ** PRSL2H ** called for ONE without bank pointer and ',
     &        'bank number, LSL2H =',I10,' NSL2H =', I10)
        GOTO 999
        END
