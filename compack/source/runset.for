      SUBROUTINE RUNSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up to save commands in file instead of
C-                         executing them.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
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
      CALL GETPAR(1,' Enter name of file to set up > ','C',INTXT)
      IF(PF.EQ.0) THEN
        PRTNAM=NAMLEV(CURLEV)
        IF(PRTNAM.EQ.'MENUDEF') THEN
          PRTNAM=NAMLEV(SAVLEV)(1:TRULEN(NAMLEV(SAVLEV)))//'$MENUDEF'
        ENDIF
C
C     OPEN command file unit
C
        CALL GTUNIT(555,COMUNI,IERR)
        IF(IERR.EQ.0) THEN
          FILNAM=TRANUP(PRTNAM(1:TRULEN(PRTNAM))//'$'//INTXT)
          K=TRULEN(FILNAM)
          OPEN(COMUNI,FILE=FILNAM(1:K),DEFAULTFILE='.INP',STATUS='NEW',
     &         IOSTAT=IOS)
          IF(IOS.NE.0) THEN
            CALL OUTMSG('0OPEN on COMMAND file->'//FILNAM(1:K)//
     &         '<-failed!'//CHAR(7))
            CALL ERRSNS(I,ISTS,ISTV,IUNI,IOS)
            CALL MSGSCR(IOS,' Reason-->')
            CALL MSGSCR(ISTS,' Secondary reason-->')
          ELSE
            SETUP=.TRUE.
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
