C DEC/CMS REPLACEMENT HISTORY, Element GETSTR.FOR
C *2     1-JUN-1988 09:51:55 HARRY "Modified call to GETHLP"
C *1    11-MAY-1988 10:48:25 HARRY "COMPACK routine to get general user input"
C DEC/CMS REPLACEMENT HISTORY, Element GETSTR.FOR
      SUBROUTINE GETSTR (PRT,TEXT,OK,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get input string using COMPACK routine
C-                         GETPAR. The OK flag is set if something has
C-                         been entered and the PF4 key has NOT been struck.
C-                         The BACK flag is set if PF4 has been struck.
C-                         All characters are converted to upper case.
C-
C-   Inputs : PRT      Prompt
C-
C-   Outputs: TEXT     Text input buffer
C-            OK       TRUE if buffer not empty
C-            BACK     TRUE if PF4 has been struck
C-
C-   Created  17-FEB-1988   Harrison B. Prosper
C-   Modified  8-MAR-1988
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ACTIVE,OK,AGAIN,HELP,BACK
      INTEGER NP,NT,PFNUM
      CHARACTER*(*) prt,text
C----------------------------------------------------------------------
      NP = LEN (prt)
      NT = LEN (text)
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        CALL GETPAR (1,PRT(1:NP),'U',TEXT(1:NT))
        HELP = PFNUM() .EQ. 2
        AGAIN = PFNUM() .EQ. 1
        IF ( HELP ) THEN
          CALL GETHLP (1,PRT(1:NP))
        ENDIF
        ACTIVE = AGAIN .OR. HELP
      ENDDO
      BACK = PFNUM() .EQ. 4
      OK = ( Text(1:1) .NE. ' ' ) .AND. ( .NOT. BACK )
  999 RETURN
      END
