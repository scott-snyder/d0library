      SUBROUTINE GET_NEXT_TOKEN(STRING,DLIMIT,I,J,N,LAST,CONTROL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the position of the next token bounded
C-   by the given delimiter. Set CONTROL = 0 to signal the first call to
C-   this routine. The flag LAST is set true if the current token is the 
C-   last in the string. Note: CONTROL must be a variable, NOT a constant.
C-   If CONTROL < 0 then (I,J) are adjusted so that leading and trailing
C-   blanks are excluded.
C-
C-   Inputs  : STRING   [C*]    Input string containing tokens
C-             DLIMIT   [C*1]   Delimiter
C-             
C-   Outputs : I,J      [I]     First and Last position of token
C-             N        [I]     Length of token
C-             LAST     [L]     TRUE if current token is the last
C-   Controls: CONTROL  [I]     CONTROL = 0 or -1 upon first entry
C-                              It is set to 1 upon return.
C-
C-   Created   7-JUN-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      CHARACTER*(*) DLIMIT
      INTEGER I,J,N
      LOGICAL LAST
      INTEGER CONTROL
C
      LOGICAL STRIP_BLANKS
      INTEGER II,JJ,KK,LL,LSTR,LDEL,ILAST,JLAST
      SAVE ILAST,JLAST
C----------------------------------------------------------------------
      LSTR = LEN(STRING)
      LDEL = LEN(DLIMIT)
C
      I = 1
      J = 0
      N = 0
C
C ****  Initialize scan
C
      IF ( CONTROL .LE. 0 ) THEN
        ILAST = 0
        JLAST = 0
        LAST  = .FALSE.
        IF ( CONTROL .LT. 0 ) THEN
          STRIP_BLANKS = .TRUE.
        ELSE
          STRIP_BLANKS = .FALSE.
        ENDIF
        CONTROL = 1
      ENDIF
      IF ( JLAST .GE. LSTR ) GOTO 999
C
      CALL TOKEN (STRING(JLAST+1:LSTR),DLIMIT(1:LDEL),II,JJ,LL)
      I = JLAST + II
      J = JLAST + JJ
      JLAST = J                         ! Note end of current token
      LAST  = JLAST .GE. LSTR
C
C ****  Check whether to strip blanks
C
      IF ( STRIP_BLANKS ) THEN
        CALL WORD(STRING(I:J),II,JJ,LL)
        J = I + JJ - 1
        I = I + II - 1
      ENDIF
C
C ****  Return length of token
C
      N = J - I + 1
  999 RETURN
      END
