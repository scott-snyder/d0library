      SUBROUTINE DISPLAY_LINES(LABEL,VALUES,TOTVALS,CURRENT_TOP_LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays a parameter list.  The names (LABEL) and
C-   the values (VALUES) of the parameters are place side by side.
C-   This subroutine assumes that the display area for the parameters is
C-   between the third line and bottom line - 2 line.   If there is more
C-   elements to display than lines available a diamond will be placed at the
C-   bottom of the list.  If the list is being scroll and the first line is not
C-   displayed another diamod will be place at the top of the list.
C-
C-   Inputs  : LABEL    - String array containing the names of the para
C-             VALUES   - String array containing the values of the
C-                        parameters to be displayed
C-             TOTVALS  - Total number of elements in the array
C-             CURRENT_TOP_LINE - The current top line of display.   Assumed
C-                        the first time equal 3
C-
C-   Outputs : none
C-
C-   Created  24-MAY-1990   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TOTVALS, CURRENT_TOP_LINE
      CHARACTER*(*) LABEL(*)
      CHARACTER*(*) VALUES(*)
C
      INTEGER STATUS, LIBPUT, LIBCAR,LIBERL,I, MAXITMS_DISPLAY
      INTEGER ITEM,DISPLAY_LINE,LAST_DISPLAYED_ITEM
      INTEGER TOP_DIAMOND_LINE,BOTTOM_DIAMOND_LINE
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      INTEGER ATTR0,ATTR1                     ! SMG attribute (3=reverse video)
      PARAMETER( ATTR0 = 0 )
      PARAMETER( ATTR1 = 1 )
C
      INTEGER LABEL_LENGTH
      PARAMETER( LABEL_LENGTH = 44 )
C
      INTEGER COLUMN1, COLUMN2, COLUMN3
      PARAMETER( COLUMN1 = 1 )
      PARAMETER( COLUMN2 = LABEL_LENGTH + 1)
      PARAMETER( COLUMN3 = 5 )
C
      INTEGER TOP_DISPLAY_LINE,BOTTOM_DISPLAY_LINE
      PARAMETER( TOP_DISPLAY_LINE = 3 )
      INTEGER NOTUSE_LINES
      PARAMETER( NOTUSE_LINES = 4 )
C
      INTEGER MAXELEMENTS
      PARAMETER( MAXELEMENTS = 50 )
      CHARACTER*44 LABEL_STRING(MAXELEMENTS)
C
      INCLUDE 'D0$INC:SMGCOM.INC/LIST'
C
C----------------------------------------------------------------------
C
C ****  Setting display variables
C
      IF ( FIRST ) THEN
        BOTTOM_DISPLAY_LINE =
     &      PBROWS + TOP_DISPLAY_LINE - NOTUSE_LINES - 1
        MAXITMS_DISPLAY = BOTTOM_DISPLAY_LINE - TOP_DISPLAY_LINE + 1
        TOP_DIAMOND_LINE = TOP_DISPLAY_LINE - 1
        BOTTOM_DIAMOND_LINE = PBROWS - 1
        FIRST = .FALSE.
      ENDIF
C
C ****  Setting up the Item of the parameter list that has to be display and
C ****  the line number designated to it.
C
      ITEM = CURRENT_TOP_LINE - TOP_DISPLAY_LINE + 1
      DISPLAY_LINE = TOP_DISPLAY_LINE
C
C ****  Display lines
C
      DO I =  1, MIN0(TOTVALS,MAXITMS_DISPLAY)
        STATUS = LIBPUT(LABEL(ITEM),DISPLAY_LINE,COLUMN1,ATTR0)
        STATUS = LIBPUT(VALUES(ITEM),DISPLAY_LINE,COLUMN2,ATTR0)
        DISPLAY_LINE = DISPLAY_LINE + 1
        ITEM = ITEM + 1
      ENDDO
C
C ****  Placing marker (diamond) indicating more parameters
C
      LAST_DISPLAYED_ITEM = ITEM - 1
      IF ( LAST_DISPLAYED_ITEM .LT. TOTVALS ) THEN
        STATUS = LIBCAR(BOTTOM_DIAMOND_LINE,COLUMN3,ATTR1) !Place bottom diamond
      ELSE
        STATUS = LIBERL(BOTTOM_DIAMOND_LINE,COLUMN3)      ! Erase the diamond
      ENDIF
      IF ( CURRENT_TOP_LINE .GT. TOP_DISPLAY_LINE ) THEN
        STATUS = LIBCAR(TOP_DIAMOND_LINE,COLUMN3,ATTR1)   ! Place top diamond
      ELSE
        STATUS = LIBERL(TOP_DIAMOND_LINE,COLUMN3)         ! Erase diamond
      ENDIF
  999 RETURN
      END
