      SUBROUTINE MODIFY_PARAMS(NVALS,NAMES,VALUES,PARAM_HELP,MODFLG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays a list of parameters names and values and
C-   allows the user to modify them.   The display will let you use the PFKEYs
C-   in the following way:
C-         PF1 - MODIFY Perss this key and enter new value to modify a
C-               parameter
C-         PF2 - HELP Displays help about the parameter where the cursor is on
C-               in the upper part of the window
C-         PF3 - QUIT Send you back to previous menu without saving any changes
C-               made.
C-         PF4 - SAVE Send you back to previous menu saving  all changes made.
C-
C-   Inputs  : NVALS [ I ]: Total number of parameters to be displayed
C-             NAMES [ C*]: Names of the parameters to be displayed
C-             VALUES[ C*]: Values of the parameters
C-         PARAM_HELP[ C*]: String array with comments (help) about paramemeters
C-
C-   Outputs : VALUES[ C*]: Values of the parameters
C-             MODFLG[ L*]: Logical array that will indicate what element has
C-                          been modified.
C-
C-   Created  18-MAY-1990   Lupe Howell, Harrison B. Prosper
C-   Updated  18-MAR-1991   Harrison B. Prosper  
C-      Use SAVE rather than BACK 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAMES(*)
      CHARACTER*(*) VALUES(*)
      CHARACTER*(*) PARAM_HELP(*)
      LOGICAL MODFLG(*)
      INTEGER NVALS
C
      INTEGER LIBBIG,LIBPUT, LIBSCR, LIBCAR, STATUS
      INTEGER NEW_LINE,CURRENT_LINE,LINE,CURRENT_POSITION,TOP_LINE
      INTEGER NUMBER,I,PFKEY,START,END,LENGTH,CONTRY,FIRSTNUM,MAXLINES
      INTEGER LASTNUM
      LOGICAL FULSCR,FULMOD,PFCOMAND,GETDEV
      CHARACTER*43 PROMPT
      DATA PROMPT/'{#+VAL,LIST,#+HELP,NEXT,PREVIOUS,QUIT,BACK}'/
C
      INTEGER NOPFKEY
      PARAMETER( NOPFKEY = 0 )
C
      INTEGER MODIFY
      PARAMETER( MODIFY  = 1 )
C
      INTEGER HELP
      PARAMETER( HELP = 2 )
C
      INTEGER QUIT
      PARAMETER( QUIT   = 3 )
C
      INTEGER BACK
      PARAMETER( BACK    = 4 )
C
      INTEGER NEXT
      PARAMETER( NEXT = 5 )
C
      INTEGER PREVIOUS
      PARAMETER( PREVIOUS = 6 )
C
      INTEGER LIST
      PARAMETER( LIST = 7 )
C
      INTEGER BAD_RANGE
      PARAMETER( BAD_RANGE = 8 )
C
      INTEGER MAXPARAMS
      PARAMETER( MAXPARAMS = 50 )
      INTEGER LABEL_LENGTH
      PARAMETER( LABEL_LENGTH = 44 )
      CHARACTER*44 LABEL(MAXPARAMS),LABEL_STRING(MAXPARAMS)
      CHARACTER*35 PARAM(MAXPARAMS),INITIAL_STRING,OUTPUT_STRING
C
      INTEGER TOP_DISPLAY_LINE
      PARAMETER( TOP_DISPLAY_LINE = 3 )
C
      CHARACTER*16 BLANK
      DATA BLANK/'                '/
      CHARACTER*1   BOUNDARY
      PARAMETER( BOUNDARY = '|' )
C
      INTEGER TAB
      PARAMETER( TAB = 4 )

C
      LOGICAL ACTIVE
      INCLUDE 'D0$INC:SMGCOM.INC/LIST'
C----------------------------------------------------------------------
C
C ****  Copy ORIGINAL VALUES into local buffers
C
      IF ( NVALS .GT. MAXPARAMS ) THEN
        NUMBER = MAXPARAMS
      ELSE
        NUMBER = NVALS
      ENDIF
C
      DO I =  1,NUMBER
        LABEL(I) = NAMES(I)
        PARAM(I) = VALUES(I)
        MODFLG(I)= .FALSE.
      ENDDO

      ACTIVE = .TRUE.
C
C ****  Clear screen
C
      CALL OUTMSG('1')
C
C ****  Finding out if Full screen mode is on
C
      FULSCR = FULMOD()
C
C ****  Setting limited scroll region
C
      STATUS = LIBSCR(3,PBROWS-2)     !  2,PBROWS-1)
C
C ****  Display labels
C
      BLANK   = ' '
      TOP_LINE = TOP_DISPLAY_LINE    !3
C
      DO I =  1, NUMBER
        LINE = I
C
C ****  Placing numbers next to the parametersIF NOT in full screen mode
C
        LABEL_STRING(I) = BLANK(1:TAB)//
     &                 LABEL(I)(1:LABEL_LENGTH-TAB-2)//
     &                 BOUNDARY
      ENDDO
      PFKEY    = NOPFKEY                ! Initialize PF key NO key
      IF ( FULSCR ) THEN
        CALL DISPLAY_LINES(LABEL_STRING,PARAM,NVALS,TOP_LINE)

C
C ****  Label PFKEYs
C
        CALL PFLABL('MODIFY','HELP','QUIT','SAVE')
C
C ****  Read string at line number LINE
C
        NEW_LINE = 1
        PFKEY    = NOPFKEY                ! Initialize PF key NO key
        ACTIVE = .TRUE.
        DO WHILE ( ACTIVE )
C
C ****  Set cursor at the current position & set LINE (parameter list line) to
C ****  the current line
C
          CURRENT_POSITION =
     &         (NEW_LINE + TOP_DISPLAY_LINE) - TOP_LINE + 2
          CALL LIBCUR(CURRENT_POSITION,1)
          LINE = NEW_LINE
C
C ****  Check if MODIFY key was entered in previous read
C
          IF ( PFKEY .EQ. MODIFY ) THEN
            INITIAL_STRING = ' '
          ELSE
            INITIAL_STRING = VALUES(LINE)
          ENDIF
C
C ****  Read from value field
C
          PFKEY = NOPFKEY                 ! Clear key
          CALL GREADS(NVALS,INITIAL_STRING,OUTPUT_STRING,PFKEY,NEW_LINE)
C
C ****  Update user value buffer ONLY if changed
C
          IF ( OUTPUT_STRING .NE. PARAM(LINE) ) THEN
            MODFLG(LINE) = .TRUE.
            VALUES(LINE) = OUTPUT_STRING   ! Update value
          ELSE
            MODFLG(LINE) = .FALSE.
          ENDIF
C
C ****  Update Parameter Display (scroll) if necessary
C
          CALL SCROLINES(LINE,NEW_LINE,LABEL_STRING,VALUES,NVALS,
     &             TOP_LINE)
C
C ****  Check for EXIT
C
          ACTIVE = (PFKEY .EQ. NOPFKEY)   .OR.
     &             (PFKEY .EQ. MODIFY)    .OR.
     &             (PFKEY .EQ. HELP)
C
C ****  Displaying help on the upper part of the window
C
          IF ( PFKEY .EQ. HELP ) THEN
            CALL INTMSG(PARAM_HELP(LINE))
          ENDIF
        ENDDO
C
C ****  Clearing message screen
C
C
        CALL OUTMSG('1')
      ELSE
C
C ****  Line Mode
C
        STATUS = LIBSCR(2,PBROWS)     !  2,PBROWS-1)
        PFKEY    = NOPFKEY                ! Initialize PF key NO key
C
C ****  Displaying parameters
C
        MAXLINES = PBROWS - TOP_DISPLAY_LINE - 1
        IF ( NVALS .GT. MAXLINES ) THEN
          LASTNUM = MAXLINES
        ELSE
          LASTNUM = NVALS
        ENDIF
        FIRSTNUM = 1
        CALL GLINE2(LABEL_STRING,PARAM,FIRSTNUM,LASTNUM)

  200   DO WHILE ( ACTIVE )
C
C ****  Getting and interpreting input from the user
C
          PFCOMAND = .FALSE.
          PFKEY = NOPFKEY                 ! Clear key
          CALL GPAR_LINDSPL_INPT(MAXLINES,NVALS,FIRSTNUM,LASTNUM,
     &         OUTPUT_STRING,PFKEY,LINE,PFCOMAND)
C
C ****  Displaying the parameters if list, next or previus command
C
          IF (PFCOMAND) THEN
            IF (PFKEY.EQ.LIST.OR.PFKEY.EQ.NEXT.OR.
     &          PFKEY.EQ.PREVIOUS) THEN
              CALL GLINE2(LABEL_STRING,VALUES,FIRSTNUM,LASTNUM)
            ENDIF
C
C ****  Bad range entered in the command line
C
          ELSEIF (PFKEY .EQ. BAD_RANGE) THEN
            GOTO 200
          ELSE
C
C ****  Updating parameter value if it was changed
C
            PFKEY = NOPFKEY
            IF(OUTPUT_STRING.NE.PARAM(LINE)) THEN
              MODFLG(LINE) = .TRUE.
              VALUES(LINE) = OUTPUT_STRING
              CALL GLINE2(LABEL_STRING,VALUES,FIRSTNUM,LASTNUM)
            ELSE
              MODFLG(LINE) = .FALSE.
            ENDIF
          ENDIF
          ACTIVE = (PFKEY .EQ. NOPFKEY).OR.(PFKEY.EQ. HELP)
     &             .OR. (PFKEY .EQ. NEXT)
     &             .OR. (PFKEY .EQ. PREVIOUS)
     &             .OR. (PFKEY .EQ. LIST)
C
C ****  Displaying help on the upper part of the window
C
          IF ( PFKEY .EQ. HELP ) THEN
            CALL INTMSG(PARAM_HELP(LINE))
          ENDIF
        ENDDO
      ENDIF
C
C ****  Check for QUIT; If so, return original values
C
      IF ( PFKEY .EQ. QUIT ) THEN
C
        DO I =  1,NUMBER
          VALUES(I) = PARAM(I)
          MODFLG(I) = .FALSE.
        ENDDO
      ENDIF
  999 RETURN
      END
