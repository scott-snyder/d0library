      SUBROUTINE P0VSTR( XPOS, YPOS, PRCENT, YXRATI, TEXT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output a string at the given position.
C-
C-   Inputs  : XPOS   [F] is the x position where to output the string
C-             YPOS   [F] is the y position where to output the string
C-             PRCENT [F] is the percentage of window size used for 
C-                        character size, default=1.
C-             YXRATI [F] is the Y / X character size ratio, default=2.
C-             TEXT  [C*] is the text to be output
C-   Outputs :
C-   Controls:
C-
C-   Created  15-MAY-1989   Jeffrey Bantly   
C-   Updated   5-FEB-1991   Lupe Howell  The string quality was eliminated the 
C-      print will be done with quality 2. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XPOS, YPOS, XSIZE, YSIZE, PRCENT, YXRATI
      REAL    XW1, XW2, YW1, YW2
      REAL    percnt
      INTEGER NUSTR
      CHARACTER*(*) TEXT
C----------------------------------------------------------------------
      CALL J4RGET(1,XW1,XW2,YW1,YW2)
      IF( PRCENT .LE. 0. .OR. PRCENT .GT. 100. ) PRCENT = 1.
      PERCNT = PRCENT / 100.
      IF( YXRATI .LE. 0. ) YXRATI = 2.
      XSIZE = ( XW2 - XW1 ) * PERCNT
      YSIZE = ( YW2 - YW1 ) * PERCNT * YXRATI
      CALL JSIZE(XSIZE,YSIZE)
      CALL JMOVE( XPOS, YPOS )
      CALL J3STRG( TEXT )
  999 RETURN
      END
