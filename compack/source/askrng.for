C DEC/CMS REPLACEMENT HISTORY, Element ASKRNG.FOR
C *1    11-MAY-1988 10:45:01 HARRY "COMPACK routine to prompt for range of integers or reals"
C DEC/CMS REPLACEMENT HISTORY, Element ASKRNG.FOR
      SUBROUTINE ASKRNG (PRT,TYPE,LIMITS,DEFAUL,STR,NUMBER,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prompt user for a number or a range of numbers
C-                         in the range (LIMITS(1),LIMITS(2)) and set
C-                         default values if RETURN entered.
C-
C-   Inputs:   PRT         Prompt
C-             TYPE        "R" => real numbers; otherwise integers
C-             LIMITS(2)   Range of acceptable numbers
C-             DEFAUL(2)   Default range of numbers
C-             STR         String indicating default number or number range
C-
C-   Outputs:  STR         String indicating number or number range
C-                         selected
C-             NUMBER(2)   Number or number range
C-             BACK        If true go back to upper menu level
C-
C-   Created  14-APR-1988   Harrison B. Prosper
C-   Modified 29-APR-1988
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL       FIRST,OK,BACK,ACTIVE,BOOK
      REAL          LIMITS(2),DEFAUL(2),NUMBER(2)
      INTEGER       I,J,K,L
      CHARACTER*(*) PRT,TYPE,STR
      CHARACTER*64  STRING
      CHARACTER*4   NUM
C----------------------------------------------------------------------
      J = LEN (PRT)
      L = LEN (STR)
      IF ( (TYPE(1:1) .EQ. 'R') .or. (TYPE(1:1) .EQ. 'r' ) ) THEN
        CALL ASKRLS (PRT(1:J),' ',LIMITS,DEFAUL,STR(1:L),NUMBER,BACK)
        IF ( BACK ) GO TO 999
      ELSE
        CALL ASKINS (PRT(1:J),'E',LIMITS,DEFAUL,STR(1:L),NUMBER,BACK)
        IF ( BACK ) GO TO 999
      ENDIF
  999 RETURN
      END
