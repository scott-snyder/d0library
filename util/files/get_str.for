      SUBROUTINE GET_STR(PROMPT,STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a string from the terminal, replace only of
C-                              non-blank
C-
C-   Inputs  : PROMPT   Prompt string
C-             STRING   Default string
C-   Outputs : STRING   Returned string
C-   Controls:
C-
C-   Created   3-APR-1990   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PROMPT,STRING
      CHARACTER*80 save_string
      CHARACTER*132 temp
      INTEGER*2 outlen
      INTEGER trulen,len
      INTEGER n,str$find_first_in_set
C----------------------------------------------------------------------
c
      save_string = string
      len = trulen(string)
      IF ( len.LE.0 ) len = 1
      temp = ' '//prompt//' ['//string(1:len)//']'
      WRITE(6,*) temp
      READ(5,'(A)') string
      n = index(string,'!')
      IF ( n.GT.0 ) THEN
        IF ( n.GT.1 ) THEN
          string = string(1:n-1)
        ELSE
          string = ' '
        ENDIF
      ENDIF
      CALL str$trim(string,string,outlen)
      IF ( string.EQ.' ' ) string = save_string
      CALL STR$UPCASE(STRING,STRING)
c
  999 RETURN
      END
