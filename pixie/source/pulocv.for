      SUBROUTINE PULOCV( XIN, YIN, ICHAR, XOUT, YOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put the echo position at WINDOW coordinates XIN, YIN.
C-               Wait for user input ( cursor movement, then one character )
C-               Return the VIRTUAL position, and the UPPERCASED terminating
C-               character used ( 1-9 = digit, other = ASCII value )
C-
C-   Inputs  : XIN    [F] : Window coordinate for Echo point
C-             YIN    [F] : Window coordinate for Echo point
C-   Outputs : ICHAR  [I] : ASCII character typed, Uppercased, 1-9 for the
C-                          corresponding digit.
C-             XOUT   [F] : VIRTUAL coordinate of the selected point
C-             YOUT   [F] : VIRTUAL coordinate of the selected point
C-   Controls:
C-
C-   Created   8-NOV-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      REAL    XIN, YIN, XOUT, YOUT, XV, YV
      INTEGER ICHAR
      CHARACTER*3 DRIVER
      DATA  DRIVER / '   ' /
C----------------------------------------------------------------------
C-
      CALL JCONWV( XIN, YIN, 0., XV, YV )
      CALL JPECHO( 1, 2, 1, XV, YV )
      CALL JIENAB( 1, 2, 1 )
      CALL JLOCAT( 1, 1, 1, ICHAR, XV, YV )
      CALL JIDISA( 1, 2, 1 )
C
      XOUT = XV
      YOUT = YV
C
C ****  Convert lower case to UPPER case characters...
C
      IF( ICHAR .GE. 96 .AND. ICHAR .LE. 122 ) ICHAR = ICHAR - 32
C
C ****  If GPV driver, ignore locator value ( i.e. Character ) and return 997
C
      IF ( DRIVER .EQ. '   ' ) CALL D0HDRV( 1, DRIVER )
      IF((DRIVER .EQ. 'GPV').OR.(DRIVER.EQ.'XDW')) THEN
        IF( ICHAR .LT. 997 ) ICHAR = 997
      ENDIF
  999 RETURN
      END
