      SUBROUTINE RUNLOG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up logging of commands in command file.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: Command file is opened and LOGUP flag set to .TRUE.
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C-   Updated   1-APR-1992   Harrison B. Prosper  
C-    Add carriagecontrol='LIST' 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C&IF VAXVMS
      CHARACTER*132 FILNAM,TRANUP
      INTEGER IERR,TRULEN,I,IOS,K
      INTEGER ISTS,ISTV,IUNI
      CHARACTER*32 INTXT,PRTNAM
C----------------------------------------------------------------------
      CALL GETPAR(1,' Enter name of file to save commands in > ',
     *     'C',INTXT)
      IF(PF.EQ.0) THEN
        PRTNAM=NAMLEV(CURLEV)
        IF(PRTNAM.EQ.'MENUDEF') THEN
          PRTNAM=NAMLEV(SAVLEV)(1:TRULEN(NAMLEV(SAVLEV)))//'$MENUDEF'
        ENDIF
        CALL GTUNIT(555,COMUNI,IERR)
        IF(IERR.EQ.0) THEN
C
C     OPEN command file unit
C
          FILNAM=TRANUP(PRTNAM(1:TRULEN(PRTNAM))//'$'//INTXT)
          K=TRULEN(FILNAM)
          OPEN(COMUNI,FILE=FILNAM(1:K),DEFAULTFILE='.INP',STATUS='NEW',
     &         CARRIAGECONTROL='LIST',IOSTAT=IOS)
          IF(IOS.NE.0) THEN
            CALL OUTMSG('0OPEN on COMMAND file->'//FILNAM(1:K)//
     &         '<-failed!'//CHAR(7))
            CALL ERRSNS(I,ISTS,ISTV,IUNI,IOS)
            CALL MSGSCR(IOS,' Reason-->')
            CALL MSGSCR(ISTS,' Secondary reason-->')
          ELSE
            LOGUP=.TRUE.
            OLDLEV=CURLEV
          ENDIF
        ENDIF
      ENDIF
C&ELSE
C&      CALL INTMSG('0Command file mode NOT supported here!'//CHAR(7))
C&      CALL INTMSG(' ')
C&ENDIF
      RETURN
      END
