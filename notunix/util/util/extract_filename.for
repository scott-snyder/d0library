      SUBROUTINE EXTRACT_FILENAME (STRING,II,JJ,KK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search STRING for a file-spec and return the
C-   start II and end JJ of the file-spec within the string.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-NOV-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) STRING
      INTEGER       II,JJ,KK
C
      INTEGER L
      LOGICAL MATCH_WILD
C----------------------------------------------------------------------
      L = LEN(STRING)
      IF ( MATCH_WILD (STRING(1:L),'*[*]*.*') ) THEN
        CALL EXTRACT_WORD (']',STRING(1:L),II,JJ,KK)
      ELSE
        CALL EXTRACT_WORD ('.',STRING(1:L),II,JJ,KK)
      ENDIF
C
      IF ( STRING(II:II) .EQ. ',' .OR.
     &     STRING(II:II) .EQ. '+' .OR.
     &     STRING(II:II) .EQ. '-' .OR.
     &     STRING(II:II) .EQ. '/' .OR.
     &     STRING(II:II) .EQ. '\' ) THEN
        II = II + 1
      ENDIF
C
      IF ( STRING(JJ:JJ) .EQ. ',' .OR.
     &     STRING(JJ:JJ) .EQ. '+' .OR.
     &     STRING(JJ:JJ) .EQ. '-' .OR.
     &     STRING(JJ:JJ) .EQ. '/' .OR.
     &     STRING(JJ:JJ) .EQ. '\' ) THEN
        JJ = JJ - 1
      ENDIF
      KK = JJ - II + 1
  999 RETURN
      END
