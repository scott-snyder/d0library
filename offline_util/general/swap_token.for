      SUBROUTINE SWAP_TOKEN(OLD_TOKEN,NEW_TOKEN,NTEXT,TEXT,LTEXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Replace one token with another in specified
C-   array of strings.
C-
C-   Inputs  : OLD_TOKEN        [C*]    Old token to be replaced
C-             NEW_TOKEN        [C*]    New token
C-             NTEXT            [I]     Number of elements in text buffer
C-             TEXT(*)          [C*]    Text buffer
C-   Outputs : TEXT(*)          [C*]    Altered text
C-             LTEXT(*)         [I]     Length of each line
C-   Controls: None
C-
C-   Created  13-SEP-1990   Harrison B. Prosper
C-   Updated  25-JUN-1991   Harrison B. Prosper
C-      Handle blank old_token or new_token
C-   Updated  19-AUG-1992   Laura A. Paterno  Allow swap of anything to null
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) OLD_TOKEN
      CHARACTER*(*) NEW_TOKEN
      INTEGER NTEXT
      CHARACTER*(*) TEXT(*)
      INTEGER LTEXT(*)
C----------------------------------------------------------------------
      INTEGER I,J,K,N,II,JJ,NN,III
      INTEGER ISTART,IEND,JEND,LINE
      INTEGER OLD_LEN,NEW_LEN,DIFFERENCE
C
      LOGICAL MATCHED
C
      CHARACTER*132 STRING,RECORD
C----------------------------------------------------------------------

C *********************************************
C ****  Get OLD and NEW string lengths
C *********************************************

      CALL SWORDS(OLD_TOKEN,I,OLD_LEN,J)
      IF ( OLD_LEN .LE. 0 ) OLD_LEN = LEN(OLD_TOKEN)
      CALL SWORDS(NEW_TOKEN,I,NEW_LEN,J)
      IF ( NEW_LEN .LE. 0 ) NEW_LEN = LEN(NEW_TOKEN)
C
C ****  Check for a null
C
      IF ( ICHAR(NEW_TOKEN(1:1)) .EQ. 0 ) THEN
        NEW_LEN = 0
      ENDIF
      DIFFERENCE = NEW_LEN - OLD_LEN
C
C ****  Loop of text buffer
C
      DO LINE = 1, NTEXT
C
C ****  Get string length
C
        STRING = TEXT(LINE)
        RECORD = TEXT(LINE)
        CALL SWORDS (STRING,ISTART,IEND,NN)
        JJ   = 1
        JEND = IEND
C
C ****  Find matching string
C
        I    = INDEX (RECORD(JJ:JEND),OLD_TOKEN(1:OLD_LEN))
        MATCHED = I .GT. 0
C
        IF ( MATCHED ) THEN
C
C ****  Make substitution
C
          II = 0
          JJ = I + OLD_LEN
          DO WHILE ( MATCHED )
            IF ( NEW_LEN .GT. 0 ) THEN
              STRING = STRING(1:I-1)//NEW_TOKEN(1:NEW_LEN)
     &                              //STRING(I+OLD_LEN:)
            ELSE
              STRING = STRING(1:I-1)//STRING(I+OLD_LEN:)
            ENDIF
            IEND   = IEND + DIFFERENCE
            II     = II + DIFFERENCE
C
            I  = INDEX (RECORD(JJ:JEND),OLD_TOKEN(1:OLD_LEN))
            MATCHED = I .GT. 0
            I  = JJ + I - 1         ! Convert to absolute position
            JJ = I + OLD_LEN
            I  = I + II             ! Adjust for difference in lengths
          ENDDO
        ENDIF
C
C ****  Update string
C
        TEXT(LINE)  = STRING(1:IEND)
        LTEXT(LINE) = IEND
      ENDDO
  999 RETURN
      END
