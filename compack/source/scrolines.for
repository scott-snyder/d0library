      SUBROUTINE SCROLINES(OLD_LINE,NEW_LINE,NAMES,VALUES,NVALS,
     &  TOP_LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scrolls lines that have being displayed.
C-
C-   Inputs  : OLD_LINE - Line where the cursor was priviously 
C-             NEW_LINE - New position of the cursor
C-             NAMES    - String array containing the names of the parameters to
C-                        be displayed
C-             VALUES   - String array of the values of the parameters to be
C-                        displayed next to the names
C-             NVALS    - Total number of values in the arrays
C-             TOP_LINE - Number of the top line of the display
C-
C-   Outputs : TOP_LINE - Number of the top line of the diplay after the scroll
C-                        if necessary
C-
C-   Created  24-MAY-1990   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER OLD_LINE, NEW_LINE 
      CHARACTER*(*) NAMES(*)
      CHARACTER*(*) VALUES(*)
      INTEGER NVALS, TOP_LINE
C
      INTEGER MAXITMS,TOP_LINE_ITEM
C
      INTEGER MINLINE
      PARAMETER( MINLINE = 1 )
C
      INTEGER TOP_DISPLAY_LINE
      PARAMETER( TOP_DISPLAY_LINE = 3 )
C
      INTEGER LABEL_LENGTH
      PARAMETER( LABEL_LENGTH = 44 )
C
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
C
C ****  Getting maximun parameter item number to be displayed
C
      MAXITMS = (PBROWS-4) + TOP_LINE - TOP_DISPLAY_LINE         
      TOP_LINE_ITEM = TOP_LINE - TOP_DISPLAY_LINE + 1 
      IF ( NEW_LINE .NE. OLD_LINE ) THEN
C
C ****  Checking if redraw is necessary
C
        IF ((NEW_LINE .GT. MAXITMS) .OR. (NEW_LINE .LT.(TOP_LINE_ITEM)))
     &    THEN
C
C ****  Scrolling down
C
          IF((NEW_LINE .GT. OLD_LINE).AND.(NEW_LINE.GT.MAXITMS))THEN
            TOP_LINE = TOP_LINE + 1
C
C ****  Scroling up
C
          ELSEIF((NEW_LINE.LT.OLD_LINE).AND.
     &           (NEW_LINE.LT.(TOP_LINE_ITEM))
     &                 .AND.(NEW_LINE.GE.MINLINE))THEN
            TOP_LINE = TOP_LINE - 1
C
C ****   Return if no need to redraw
C
          ELSE
            GO TO 999
          ENDIF
C
C ****  Redrawing Menu
C
          CALL DISPLAY_LINES(NAMES,VALUES,NVALS,TOP_LINE)
        ENDIF
      ENDIF
  999 RETURN
      END
