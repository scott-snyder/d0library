      FUNCTION L1COOR_NEXT_CHAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the next character off the input string.
C-      NOTE: A call to L1COOR_INIT_CHAR must be made before the first call to
C-      this function.
C-
C-   Returned value  : The next character off the input stream.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  25-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   7-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Now skips blank lines and lines beginning with '!'
C-   Updated  15-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Modified COOR parser to take its input from a string
C-                      rather than a file.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
C
      CHARACTER*1 L1COOR_NEXT_CHAR,L1COOR_CUR_CHAR,L1COOR_INIT_CHAR
C
      INTEGER MAX_LENGTH
      PARAMETER (MAX_LENGTH = MAX_COOR_MESG_LENGTH)
C
      INTEGER TRULEN
      EXTERNAL TRULEN
C
      INTEGER CURRENT
      INTEGER LAST
      CHARACTER*1 CURRENT_CHAR
      CHARACTER*1122 LINEBUF, LINEIN
      SAVE CURRENT,LAST,LINEBUF
C
C
      CURRENT = CURRENT + 1
      IF (CURRENT .GT. LAST) THEN
        CURRENT_CHAR = 'l'
        GOTO 999
      ENDIF
      CURRENT_CHAR = LINEBUF(CURRENT:CURRENT)
C
  999 L1COOR_NEXT_CHAR = CURRENT_CHAR
      RETURN
C
      ENTRY L1COOR_CUR_CHAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the last character read from the input
C-   string. 
C-   NOTE: L1COOR_INIT_CHAR and L1COOR_NEXT_CHAR must be called before this 
C-   routine is called.
C-
C-   Retured Value: The last character read from the input
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  25-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  15-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Modified COOR parser to take its input from a string
C-                      rather than a file.
C-
C----------------------------------------------------------------------
      L1COOR_CUR_CHAR = CURRENT_CHAR
      RETURN
C----------------------------------------------------------------------
      ENTRY L1COOR_INIT_CHAR(LINEIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize variables used by the level 1 simulator
C-   parser input routines.
C-
C-   Inputs  : LINEIN   The input line.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  25-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  15-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Modified COOR parser to take its input from a string
C-                      rather than a file.
C-
C----------------------------------------------------------------------
      CALL UPCASE(LINEIN, LINEBUF)
      CURRENT = 0
      LAST = MAX(INDEX(LINEBUF, '                ') - 1, 1)
      L1COOR_INIT_CHAR = ' '
      RETURN
C----------------------------------------------------------------------
      END
