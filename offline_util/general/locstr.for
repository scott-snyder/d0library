      SUBROUTINE LOCSTR (NAME,LIST,NUMBER,FOUND,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perfrom a binary search for string NAME in
C-                         ordered list of strings LIST. Strings should
C-                         have a maximum length of 80 characters.
C-
C-   Inputs  : 
C-             NAME        Name of string to be searched for.
C-             LIST(*)     List of strings to be searched.
C-             NUMBER      Number of strings in list.
C-
C-   Outputs : 
C-             FOUND       TRUE if NAME found in LIST; FALSE otherwise.
C-             ID          Location of NAME in LIST; zero if not found.
C-
C-   Controls: None
C-
C-   Created   9-JUL-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME,LIST(*)
      INTEGER       NUMBER,ID
      LOGICAL       FOUND
      INTEGER       UP,DOWN,MEDIAN,N,MAXCHR
      PARAMETER( MAXCHR = 80 )
      CHARACTER*80  TEMP
C----------------------------------------------------------------------
      N    = LEN(NAME)
      IF ( N .GT. MAXCHR ) N = MAXCHR
C
C ****  Find string by binary search.
C
      UP   = NUMBER + 1
      DOWN = 0
  100 CONTINUE
      IF ( (UP-DOWN) .LE. 1 ) GOTO 200
      MEDIAN = ISHFT(UP+DOWN,-1)
      TEMP = LIST(MEDIAN)
C
      IF ( NAME(1:N) .LT. TEMP(1:N)  ) THEN
        UP   = MEDIAN
      ELSEIF ( NAME(1:N) .GT. TEMP(1:N) ) THEN
        DOWN = MEDIAN  
      ELSEIF ( NAME(1:N) .EQ. TEMP(1:N) ) THEN
        FOUND = .TRUE.
        ID    = MEDIAN
        GOTO 999
      ENDIF
C
      GOTO 100
  200 CONTINUE
      FOUND = .FALSE.
      ID    = 0
C
  999 RETURN
      END
