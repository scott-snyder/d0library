      SUBROUTINE ASKYNO (PRT,DEF,STR,FLAG,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Ask for a YES/NO answer with a default
C-                         response.
C-
C-   Inputs  : PRT         Prompt string
C-             DEF         Default response, either 'Y' or 'N'.
C-
C-   Outputs : STR         Response, either '(YES)' or '(NO)'.
C-             FLAG        TRUE if default or if response equals the 
C-                         default response
C-             BACK        If TRUE return to upper menu level.
C-   Controls: None
C-
C-   Created  11-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PRT
      CHARACTER*(*) DEF
      CHARACTER*(*) STR
      LOGICAL       FLAG
      LOGICAL       BACK
C
      INTEGER I,J,K,L,N
      LOGICAL ANY
      CHARACTER*1 DEFAUL,ANSWER
      CHARACTER*80 PRMT
C----------------------------------------------------------------------
C
C ****  Complete prompt
C
      L = LEN(PRT)
      CALL UPCASE (DEF(1:1),DEFAUL)
      IF ( DEFAUL .EQ. 'Y' ) THEN
        PRMT = PRT(1:L)//' [Y]> '
      ELSE
        PRMT = PRT(1:L)//' [N]> '
      ENDIF
C
      CALL GETSTR (PRMT(1:L+6),ANSWER,ANY,BACK)
      IF ( BACK ) GOTO 999
C
      IF ( .NOT. ANY ) THEN             ! If no input set default
        IF ( DEFAUL .EQ. 'Y' ) THEN
          ANSWER = 'Y'
        ELSE
          ANSWER = 'N'
        ENDIF
      ENDIF
C
      FLAG = ANSWER .EQ. DEFAUL
C
      IF ( ANSWER .EQ. 'Y' ) THEN
        STR = '(YES)'
      ELSE
        STR = '(NO)'
      ENDIF
C
  999 RETURN
      END
