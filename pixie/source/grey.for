      SUBROUTINE GREY( IFTAB, ICTAB, DRVNAM )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a  table with grey shades for simulate
C-                 color  in a black and white terminal using GPV driver.
C-                 This subroutine assumes that you are in a B/W Vax
C-                 station.
C-
C-   Outputs : IFTAB - shades table
C-             ICTAB - color table for shades
C-             DRVNAM- driver's name
C-
C-   Created  20-APR-1989   LUPE ROSAS
C-   Updated  14-SEP-1989   Lupe Rosas Black and white set
C-   Update    4-OCT-1989   Lupe Rosas Set the foreground color
C-   Updated   4-APR-1990   Lupe Howell  Work with XDW
C-   Updated  15-FEB-1993   Lupe Howell  Add Grey scale for PST 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICTAB(*), IFTAB(*), GREYPA(16), I, NUMCOL, COLPA(16), 
     &        XDWPA(16),PST_GREY_ORDER(16)
      CHARACTER*3 DRVNAM
C----------------------------------------------------------------------
      DATA NUMCOL /14/
      DATA GREYPA/47, 47, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,
     X            57,58, 59/
      DATA XDWPA/47, 47, 23, 24, 28, 29, 38, 39, 40, 51, 52, 53, 54,
     X            12, 61, 62/
      DATA COLPA/7, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C PST driver's order
      DATA PST_GREY_ORDER/
     &  7, 8,40,  ! White, Black, D. Purple
     &  20,39, 3, ! Purple, D. Blue, Blue
     &  6,51,2,   ! Cyan, D. Green, Green
     &  34,41,43, ! Blue Green, D. Magenta, D. Red,
     &  4,5,27,1/ ! Red, Magenta, Orange, Yellow, Complement
C----------------------------------------------------------------------
C
C ****  Setting shades for T10 drivers
C
      IF ( DRVNAM .EQ. 'T10' ) THEN
        CALL HRDCPY( DRVNAM, ICTAB, IFTAB )
        GOTO 999
      ENDIF
C
C ****  Setting shades for XDW driver
C
      IF( (DRVNAM .EQ. 'XDW') .OR.(DRVNAM .EQ. 'X11') ) THEN
        DO 90 I=1, (NUMCOL+2)
          IFTAB(I) = XDWPA(I)
          ICTAB(I) = COLPA(I)
   90   CONTINUE
        GO TO 200
      ENDIF
C
C ****  Setting shades order for the GPV
C
      IF ( DRVNAM .EQ. 'GPV' ) THEN
        DO 100 I=1, (NUMCOL+2)
          IFTAB(I) = GREYPA(I)
          ICTAB(I) = COLPA(I)
  100   CONTINUE
      ENDIF
C
C ****  Setting shades order for the PST
C
      IF ( DRVNAM .EQ. 'PST' ) THEN
        DO 150 I=1, 16
          IFTAB(I) = 1
          ICTAB(I) = PST_GREY_ORDER(I)
  150   CONTINUE
      ENDIF
C
C ****  Setting foreground color
C
  200 CONTINUE
      IF( (DRVNAM .EQ. 'GPV').OR.(DRVNAM.EQ.'T10')
     X      .OR.(DRVNAM.EQ.'XDW')) THEN
        ICTAB(17) = 0
        IFTAB(17) = 47
      ELSE                             ! Black background (default)
        ICTAB(17) = ICTAB(1)
        IFTAB(17) = 47
      ENDIF
  999 RETURN
      END
