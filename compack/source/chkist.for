      LOGICAL FUNCTION CHKIST(COMIN,PROLIN,PROPRT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if COMIN contains part of system type command
C-
C-   Inputs  : COMIN:  Command string to be checked
C-             PROLIN: Command to search for
C-             PROPRT: Prompt string used to find match
C-   Outputs : NONE (COMIN may be changed when ambiguity found)
C-
C-   Modified  9-DEC-1987   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMIN,PROLIN,PROPRT
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      CHARACTER*80 TRANUP,COM
      INTEGER I,J,TRULEN
      LOGICAL TEMP
C----------------------------------------------------------------------
      TEMP=.FALSE.
      COM=TRANUP(COMIN)
      I=INDEX(COM,' ')-1
      IF(I.EQ.-1) THEN
        I=LEN(COM)                             ! Use all of command when no blanks
      ENDIF
      IF(INDEX(PROLIN,PROPRT).GT.0.AND.
     *   PROPRT(1:I).EQ.COM(1:I).AND.
     *   TRULEN(COMIN).GT.0) THEN             ! PROPRT included in prompt
        IF(POS.EQ.0) THEN            !POS=0 when coming from menu
C                                    !selection prompt
          DO J=1,MAXPOS              !Look for commands beginning with COMIN
            IF(TRANUP(MENLIN(J,CURLEV)(1:I)).EQ.COM(1:I)) THEN
              COMIN=' '
              CALL OUTMSG('0Ambigous command: '//COM(1:I)//CHAR(7))
              GOTO 1000
            ENDIF
          ENDDO
        ENDIF
        TEMP=.TRUE.
      ENDIF
 1000 CONTINUE
      CHKIST=TEMP
      RETURN
      END
