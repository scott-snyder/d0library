      SUBROUTINE PRGLOB ( PRUNIT, LGLOB, NGLOB, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'GLOB'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LGLOB  [I] : Pointer to the one bank ( CFL = 'ONE' ) or to the
C-                          first of a linear structure ( CFL = 'LINEAR' ).
C-                          Unused if CFL = 'ALL'.
C-             NGLOB  [I] : Bank number, used only if CFL='ONE' and LGLOB = 0
C-             CFL    [C*]: Character flag, other input depends on it's value:
C-                          'ONE' : LGLOB point to a bank, or if <0, NGLOB is
C-                                  the bank number.
C-                          'LINEAR' : LGLOB points to the first bank of the
C-                                  Linear structure
C-                          'ALL' : Prints all banks
C-             IFL    [I] : Defines the amount of printing: 0 means full
C-                          printout, 1 is the minimum, 2 gives more, ...
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  15-DEC-1992 22:30:48.87  Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT, LGLOB, NGLOB, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGLOB.LINK'
C----------------------------------------------------------------------
      INTEGER LGLOB1, GZGLOB, LZLOC, J
      CHARACTER*8 CFLAG
C----------------------------------------------------------------------
      CFLAG  = CFL(1:LEN(CFL))
      LGLOB1 = LGLOB
C
      IF    ( CFLAG .EQ. 'LINEAR' ) THEN
        IF( LGLOB .LE. 0 ) GOTO 990
      ELSEIF( CFLAG .EQ. 'ONE' ) THEN
        IF( LGLOB .LE. 0 ) THEN
          IF( NGLOB .EQ. 0 ) GOTO 980          ! Error exit
          LGLOB1 = LZLOC( IXMAIN, 'GLOB', NGLOB )
        ENDIF
      ELSEIF((CFLAG .EQ. 'ALL') .OR.
     &       (CFLAG .EQ. ' '  ) ) THEN
C
C ****  Here, you have to find the first bank to be printed
C
        LGLOB1 = GZGLOB()
      ELSE
        WRITE( PRUNIT, 1000 ) CFLAG
 1000   FORMAT(/' ** PRGLOB ** Illegal value of CFL = ',a/)
        GOTO 999
      ENDIF
C
C ****  Check address
C
      IF ( LGLOB1 .LE. 0 ) THEN
        WRITE( PRUNIT, 1005 )
 1005   FORMAT(/' No GLOB bank FOUND')
        GOTO 999
      ENDIF
C
    1 CONTINUE
C
C  ***  Print the content of the bank pointed by LGLOB1
C
      WRITE( PRUNIT, 1100 ) ( IQ( LGLOB1 + J ) , J = 1, 4 ),
     &  ( Q( LGLOB1 + J ) , J = 5, 17),
     &  ( IQ( LGLOB1 + J ) , J = 18, 20)
 1100 FORMAT(/' GLOB bank (Global event quantities)  Version',I4/
     &        ' QUALITY = ',Z8,'  Number CD tracks = ',I10/
     &        ' Number CC cells above 0.3 = ',I10/
     &        ' Cal        Sum ET = ',E12.4/
     &        ' EC         Sum ET = ',E12.4/
     &        ' CC         Sum ET = ',E12.4/
     &        ' Cal        Sum E  = ',E12.4/
     &        ' EC South   Sum E  = ',E12.4/
     &        ' CC Up      Sum E  = ',E12.4/
     &        ' CC Down    Sum E  = ',E12.4/
     &        ' EC North   Sum E  = ',E12.4/
     &        ' Main Ring  Sum E  = ',E12.4/
     &        ' Main Ring  Sum ET = ',E12.4/
     &        ' Hot Cells  Sum E  = ',E12.4/
     &        ' Hot Cells  Sum ET = ',E12.4/
     &        ' Time since Evt29 (s) = ',F10.3/
     &        ' Main Ring bits (Hex) = ',Z8/
     &        ' Toroid polarities: WAMUS = ',I2,'  SAMUS = ',I2)
C
C  ***  Look if another bank is needed
C
      IF( CFLAG .EQ. 'ONE' ) GOTO 999
C
      IF( CFLAG .EQ. 'LINEAR' ) THEN
        LGLOB1 = LQ( LGLOB1 )
        IF( LGLOB1 .NE. 0 ) GOTO 1
      ELSE
C
C ****  Find the next bank for the ALL command.
C
        LGLOB1 = GZGLOB()
      ENDIF
  999 RETURN
C
C  *** Error : Linear without bank pointer
C
  990 CONTINUE
      WRITE( PRUNIT, 2000 ) LGLOB
 2000 FORMAT(/' ** PRGLOB ** called for LINEAR without valid bank '
     & ,    'pointer, LGLOB =',I10/)
      GOTO 999
C
C  *** Error : One bank, but neither pointer nor number
C
  980 CONTINUE
      WRITE( PRUNIT, 2100 ) LGLOB, NGLOB
 2100 FORMAT(/'  ** PRGLOB ** called for ONE without bank pointer and '
     &  ,      'bank number, LGLOB =',I10,' NGLOB =', I10/)
      GOTO 999
      END
