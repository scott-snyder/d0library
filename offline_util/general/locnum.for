      SUBROUTINE LOCNUM (NUMBER,LIST,NL,FOUND,INDEX)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Perform a binary search for integer NUMBER in
C                         ordered list of integers LIST.
C
C   Inputs  : 
C             NUMBER      Integer to search for
C             LIST(*)     List of integers to be searched.
C             NL          Number of integers in list.
C
C   Outputs : 
C             FOUND       TRUE if NUMBER in LIST; FALSE otherwise.
C             INDEX       Location of NUMBER in LIST; zero if not found.
C
C   Controls: None
C
C   Created   11-MAR-1989  K. Wyatt Merritt
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       NUMBER,LIST(*)
      INTEGER       NL,INDEX
      LOGICAL       FOUND
C
      INTEGER       UP,DOWN,MEDIAN,N,MAXCHR
      INTEGER       TEMP
C----------------------------------------------------------------------
C
C ****  Find string by binary search.
C
      UP   = NL + 1
      DOWN = 0
  100 CONTINUE
      IF ( (UP-DOWN) .LE. 1 ) GOTO 200
      MEDIAN = ISHFT(UP+DOWN,-1)
      TEMP = LIST(MEDIAN)
C
      IF ( NUMBER .LT. TEMP  ) THEN
        UP   = MEDIAN
      ELSEIF ( NUMBER .GT. TEMP ) THEN
        DOWN = MEDIAN  
      ELSEIF ( NUMBER .EQ. TEMP ) THEN
        FOUND = .TRUE.
        INDEX = MEDIAN
        GOTO 999
      ENDIF
C
      GOTO 100
  200 CONTINUE
      FOUND = .FALSE.
      INDEX = 0
C
  999 RETURN
      END
