      SUBROUTINE MENMAI(MAIUSE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Declare main menu level
C-
C-   Inputs  : MAIUSE: String with unique menu identifier
C-   Outputs : None
C-   Controls: None
C-
C-   Created   7-OCT-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MAIUSE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      CHARACTER*132 TRANUP,PRTNAM,FILNAM,CTEMP
      INTEGER IERR,IOS,I,TRULEN,K
      INTEGER ISTS,ISTV,IUNI
C&IF VAXVMS
      INCLUDE 'D0$COMPACK$SOURCE:COMMSG.DEF'
C&ENDIF
C----------------------------------------------------------------------
      DO 1 I=1,UPRLEV
        IF(TRANUP(MAIUSE).EQ.NAMLEV(I)) THEN
          MAILEV=I
C&IF VAXVMS
          IF(BEGJOU) THEN      ! Open journal file now that we know what
                               ! the main level is.
            PRTNAM=NAMLEV(MAILEV)
            IF(PRTNAM.EQ.'MENUDEF') THEN
              PRTNAM=NAMLEV(MAILEV)(1:TRULEN(NAMLEV(MAILEV)))//
     &               '$MENUDEF'
            ENDIF
            CALL GTUNIT(555,COMUNI,IERR)
            IF(IERR.EQ.0) THEN
C
C     OPEN journal file unit
C
              FILNAM=TRANUP(PRTNAM(1:TRULEN(PRTNAM))//'$'//JOUNAM)
              K=TRULEN(FILNAM)
              OPEN(COMUNI,FILE=FILNAM(1:K),DEFAULTFILE='.INP',
     &          STATUS='NEW',IOSTAT=IOS)
              IF(IOS.NE.0) THEN
                CALL OUTMSG('0OPEN on JOURNAL file->'//FILNAM(1:K)//
     &               '<-failed!'//CHAR(7))
                CALL ERRSNS(I,ISTS,ISTV,IUNI,IOS)
                CALL MSGSCR(IOS,' Reason-->')
                CALL MSGSCR(ISTS,' Secondary reason-->')
              ELSE
                LOGUP=.TRUE.
                OLDLEV=MAILEV
              ENDIF
            ENDIF
          ENDIF
C&ENDIF
          GOTO 2
        ENDIF
    1 CONTINUE
C&IF VAXVMS
      CALL ABOMEN(COMPACK__NOMATCH,MAIUSE)
C&ELSE
C&      CTEMP = '0No match with MENU-NAME-->'//MAIUSE(1:LEN(MAIUSE))
C&      CALL INTMSG(CTEMP)
C&      CALL INTMSG(' ')
C&      STOP
C&ENDIF
    2 CONTINUE
  999 RETURN
      END
