      SUBROUTINE GPAR_LINDSPL_INPT(MAXLINES,NVALS,FIRST_ITEM,
     &             LAST_ITEM,OUTPUT_STRING,PFKEY,COMNUM,PFCOMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine will prompt a message for the user to
C-   enter in the LINE mode the command desired to manipulate a list of
C-   parameters in COMPAK.   There are several posible commands that the user
C-   can entered:
C-     NUM NEW_VALUE - To change the value of a parameter displayed where,
C-                     NUM is the number of the parameters (1, 2, 3..)
C-                     NEW_VALUE is the new value desired for that parameter
C-                     After a modification was made the list of parameters
C-                     will be displayed again so the user can see the new
C-                     value.
C-
C-     NUM HELP      - To get help about a particular parameter where,
C-                     NUM is the number of the parameters (1, 2, 3..)
C-
C-     LIST          - List the parameter list 
C-
C-     NEXT          - List the rest of the parameters that did not fit in the
C-                     screen.
C-
C-     PREVIOUS      - List the previous parameter list
C
C-     QUIT          - Returns to the previous menu without saving any changes
C-                     made to the parameters
C-
C-     BACK          - Returns to the previous menu saving all changes made to
C-                     the parameters
C-
C-   NOTE: This routine was desing to edit parameter list in the line mode.
C-
C-   Inputs  : MAXLINES     [I ]: Maximun number of lines that will fit in the
C-             NVALS        [I ]: Maximun number of parameters in the list
C-             FIRST_ITEM   [I ]: The number of the first item displayed in the
C-                                parameter list
C-             LAST_ITEM    [I ]: The number of the last item displayed in the
C-                                parameter list
C-
C-   Outputs : OUTPUT_STRING[C*]: String containing the value of the input from
C-                                the user.
C-             PFKEY         [I]: Value of the command key entered if any
C-             COMNUM       [I ]: Command number entered if any
C-             PFCOMAND     [L ]: Flag to indicate if a command was entered
C-             FIRST_ITEM   [I ]: The number of the first item displayed in the
C-                                parameter list (output only when PREVIOUS or
C-                                NEXT command are enetered)
C-             LAST_ITEM    [I ]: The number of the last item displayed in the
C-                                parameter list (output only when PREVIOUS or
C-                                NEXT command are enetered)
C-
C-   Created  11-JUN-1990   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXLINES,PFKEY,COMNUM,FIRST_ITEM,LAST_ITEM,NVALS
      LOGICAL PFCOMAND
      CHARACTER*(*) OUTPUT_STRING
C
      INTEGER START,END,LENGTH,CONTRY
      LOGICAL GETDEV
C
      CHARACTER*43 PROMPT
      DATA PROMPT/'{#+VAL,#+HELP,LIST,NEXT,PREVIOUS,QUIT,BACK}'/
C
      INTEGER HELP
      PARAMETER( HELP = 2 )
C
      INTEGER QUIT
      PARAMETER( QUIT = 3 )
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
C----------------------------------------------------------------------
C
C ****  Displaying Prompt and getting input from the user
C
      CALL README(OUTPUT_STRING,PFKEY,GETDEV(),
     &       'Select: '//PROMPT(1:LEN(PROMPT))//' > ')
C
C ****  Getting the command number from the input (if any)
C
      COMNUM = CONTRY(OUTPUT_STRING)
      IF ( COMNUM .LT.0.OR.COMNUM.GT.NVALS.OR.OUTPUT_STRING(1:1).EQ.'0')
     &        THEN
        CALL OUTMSG('0Command out of range!')
        PFKEY = BAD_RANGE
        GOTO  999 
C
C ****  Placing the new value entered (if any) in the output string and getting
C ****  its size
C
      ELSEIF ( COMNUM .NE. 0 ) THEN
        OUTPUT_STRING = OUTPUT_STRING(INDEX(OUTPUT_STRING,' ')+1:)
      ENDIF
      CALL SWORDS(OUTPUT_STRING,START,END,LENGTH)
C
C ****  Checking for comand words
C
      IF (OUTPUt_STRING(START:END).EQ.'QUIT') THEN
        PFKEY = QUIT
        PFCOMAND = .TRUE.
      ELSEIF(OUTPUT_STRING(START:END).EQ.'BACK') THEN
        PFKEY = BACK
        PFCOMAND = .TRUE.
      ELSEIF(OUTPUT_STRING(START:END).EQ.'LIST') THEN
        PFKEY = LIST
        PFCOMAND = .TRUE.
      ELSEIF(OUTPUT_STRING(START:END).EQ.'HELP') THEN
        PFKEY = HELP
        PFCOMAND = .TRUE.
      ELSEIF(OUTPUT_STRING(START:END).EQ.'NEXT') THEN
        IF(LAST_ITEM .NE. NVALS) THEN
          FIRST_ITEM = LAST_ITEM + 1
          LAST_ITEM  = LAST_ITEM + MAXLINES
          IF ( LAST_ITEM .GT. NVALS ) THEN
            LAST_ITEM  = NVALS
          ENDIF
        ENDIF
        PFKEY = NEXT
        PFCOMAND = .TRUE.
      ELSEIF(OUTPUT_STRING(START:END).EQ.'PREVIOUS') THEN
        IF ( FIRST_ITEM .GT. 1 ) THEN
          LAST_ITEM  = FIRST_ITEM -1
          FIRST_ITEM = LAST_ITEM - MAXLINES + 1
        ENDIF
        PFKEY = PREVIOUS
        PFCOMAND = .TRUE.
      ENDIF
  999 RETURN
      END
