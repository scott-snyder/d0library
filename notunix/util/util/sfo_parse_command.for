      SUBROUTINE SFO_PARSE_COMMAND
     &  (POSTR,STRUCT,NSTRUCT,RECORD,TOKEN,LTOKEN,NTOKEN,ISTRUCT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Parse the !SQL macro command
C-
C-   Inputs  : POSTR    [I]     Position of Structure type within
C-                              macro command
C-             STRUCT(*)[C*]    List of structure types
C-             NSTRUCT  [I]     Number of structure types
C-             RECORD   [C*]    Current input record
C-           
C-   Outputs : TOKEN(*) [C*]    List of tokens
C-             LTOKEN(*)[I]     Length of each token
C-             NTOKEN   [I]     Number of tokens
C-             ISTRUCT  [I]     Id of structure type
C-   Controls:
C-
C-   Created   8-OCT-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER POSTR
      CHARACTER*(*) STRUCT(*)
      INTEGER NSTRUCT
      CHARACTER*(*) RECORD
      CHARACTER*(*) TOKEN(*)
      INTEGER LTOKEN(*)
      INTEGER NTOKEN
      INTEGER ISTRUCT
C----------------------------------------------------------------------
      CHARACTER*32 NAME
      CHARACTER*80 STRING
      INTEGER JCOM,LCOM,I,J,LL
C----------------------------------------------------------------------
C
C ****  Chop string after !SQL command macro
C
      JCOM = INDEX(RECORD,'!')
      LCOM = INDEX(RECORD(JCOM:),' ') - 1
      STRING = RECORD(JCOM+LCOM:)
      CALL CHOP(STRING,TOKEN,LTOKEN,NTOKEN)

      NAME = TOKEN(POSTR)               ! Name of structure type
      LL   = LTOKEN(POSTR)
C
C ****  Find ID of structure type
C
      ISTRUCT = 0
      I  = 0
      DO WHILE ( I .LT. NSTRUCT )
        I = I + 1
        IF ( STRUCT(I)(1:LL) .EQ. NAME(1:LL) ) THEN
          ISTRUCT = I
          I = NSTRUCT                ! Found so exit loop
        ENDIF
      ENDDO

  999 RETURN
      END
