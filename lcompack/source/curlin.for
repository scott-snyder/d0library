      SUBROUTINE CURLIN(COMIN,PF1,POS1,MAXPO1,COMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Issue prompt and read command from line-mode 
C-                         display. Interpret input string.
C-
C-   Inputs  : COMIN:  List of possible system selections 
C-             MAXPO1: Max possible selection at this level
C-   Outputs : PF1:    Possible PF-key struck
C-             POS1:   Number of the command selected
C-             COMAND: Command string entered
C-   Controls: 
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C-      27-Oct-1989 Penelope Constanta-Fanourakis
C-          Replaced closing of LUN 5 with an assignement
C-	    of INPLUN to be 5
C-   Updated   6-MAY-1992   Harrison B. Prosper   
C-      Add entry point SETPROMPT to avoid PFWAIT at end of sequence
C-      of commands.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMIN,COMAND
      CHARACTER*80 CTEMP
      INTEGER PF1,POS1,MAXPO1
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      LOGICAL PROMPT, FLAG
      SAVE PROMPT
      DATA PROMPT /.TRUE./
C----------------------------------------------------------------------
      POS1=0
      COMAND=' '
      IF(.NOT.ASTFLG) THEN
        CALL HLPROC(0)
        CALL OUTMSG(' ')
        CTEMP = 'Select: '//COMIN(1:LEN(COMIN))//' > '
        CALL README(COMAND,PF1,.FALSE.,CTEMP)
      ELSE
        CALL README(COMAND,PF1,.FALSE.,' ')
      ENDIF
      IF(PF1.EQ.0) THEN
        CALL LINCHK(COMAND,COMIN,PF1,POS1,MAXPO1)
      ENDIF
      IF(PF1.EQ.4.AND.RDCOM.AND.COMAND.NE.'BACK') THEN
        TRMFLG=SAVTRM                ! RESTORE old terminal indication
        SAVTRM=.FALSE.
        RDCOM=.FALSE.
        POS=1                        ! Start at top of menu again
C	CLOSE(5)
        INPLUN=5		     ! Set input to be the terminal
        IF(.NOT.ONEFLG) THEN         ! Was reading command file before real ONE line command
          IF ( PROMPT ) THEN
            CALL PFWAIT
          ENDIF
          CALL MENDIS(.TRUE.)       ! Redisplay menu
        ENDIF
      ENDIF
      RETURN
C
      ENTRY SETPROMPT(FLAG)
      PROMPT = FLAG
      RETURN
      END
