      SUBROUTINE PUSTRG( XPOS, YPOS, TEXT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output a string at the given position.
C-               The quality of the output depends on the parameter
C-               STRING QUALITY and is 1,2,3,4 for the JxSTRG DI3000 routine   
C-               ( 4 for JHSTRG )
C-
C-   Inputs  : XPOS   [F] is the x position where to output the string
C-             YPOS   [F] is the y position where to output the string
C-             TEXT  [C*] is the text to be output
C-   Outputs :
C-   Controls:
C-
C-   Created  27-JUL-1988   Olivier Callot
C-   Updated  17-AUG-1990   Lupe Howell  The String quality parameter is not
C-                                       use.   The quality will be always 2
C-   Updated  11-SEP-1990   Lupe Howell  the String quality parameter is use
C-                                       in some displays so it was reisntalled
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XPOS, YPOS
      INTEGER NUSTR
      CHARACTER*(*) TEXT
C----------------------------------------------------------------------
      CALL JMOVE( XPOS, YPOS )
C
      IF (NUSTR .LE. 2) THEN
        CALL J2STRG( TEXT )
      ELSE
        CALL J3STRG( TEXT )
      ENDIF
  999 RETURN
      END
