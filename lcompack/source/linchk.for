      SUBROUTINE LINCHK(COMAND,PROLIN,PFUSE,POSUSE,MAXP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check a command line for system commands or
C-                         number of command chosen directly
C-
C-   Inputs  : COMAND:  Command to check
C-             PROLIN:  Prompt line with possible system commands
C-             MAXP:    Maximum number of commands at this level.
C-   Outputs : PFUSE:   Possible match with one of the choices
C-             POSUSE:  Number of chosen command
C-   Controls: None
C-
C-   Documneted 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PROLIN,COMAND
      CHARACTER*132 CTEMP
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER PFUSE,POSUSE,MAXP
      INTEGER I,K,ISTAT,CONTRY,TRULEN
      LOGICAL CHKIST
C----------------------------------------------------------------------
      POSUSE=0
      PFUSE=0
      I=TRULEN(COMAND)
      IF(I.NE.0) THEN
        DO 1000 K=1,I
          IF(COMAND(1:1).NE.' ') THEN
            GOTO 1001
          ELSE
            COMAND=COMAND(2:)         ! Strip off leading blanks
          ENDIF
 1000   CONTINUE
 1001   CONTINUE
        IF(COMAND(1:1).EQ.'@') THEN
          CTEMP='RUN COMMAND FILE '//COMAND(2:)
          COMAND=CTEMP
        ENDIF
        IF(CHKIST(COMAND,PROLIN,'HELP').OR.(COMAND(1:1).EQ.'?'
     *        .AND.INDEX(PROLIN,'HELP').GT.0)) THEN
          I=INDEX(COMAND,' ')+1
          POSUSE=CONTRY(COMAND(I:))
          IF(POSUSE.LT.0.OR.POSUSE.GT.MAXP) THEN
            CALL OUTMSG('0Command out of range!'//CHAR(7))
            GOTO 979
          ENDIF
          PFUSE=2
        ELSEIF(CHKIST(COMAND,PROLIN,'MENU')) THEN
          PFUSE=3
        ELSEIF(CHKIST(COMAND,PROLIN,'OPT')) THEN
          PFUSE=3
          POSUSE=CONTRY(COMAND(I+1:))
        ELSEIF(CHKIST(COMAND,PROLIN,'LIST')) THEN
          PFUSE=3
        ELSEIF(CHKIST(COMAND,PROLIN,'EXIT').OR.
     *            CHKIST(COMAND,PROLIN,'BACK').OR.
     *            CHKIST(COMAND,PROLIN,'ABORT').OR.
     *            CHKIST(COMAND,PROLIN,'CLOSE')) THEN
          PFUSE=4
        ELSE
C
C     Try to find command number from string
C
          POSUSE=CONTRY(COMAND)
          IF(POSUSE.LT.0.OR.POSUSE.GT.MAXP.OR.COMAND(1:1).EQ.'0') THEN
            CALL OUTMSG('0Command out of range!'//CHAR(7))
            GOTO 979
          ELSEIF(POSUSE.NE.0) THEN
            PFUSE=1
            COMAND=COMAND(INDEX(COMAND,' '):)
          ENDIF
          IF(TRULEN(COMAND).GT.0) PFUSE=1   ! To make sure the command is
C                                           ! not reinterpreted when ambigous
        ENDIF
      ENDIF
  979 CONTINUE
      RETURN
      END
