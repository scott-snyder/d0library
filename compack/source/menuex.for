      SUBROUTINE MENUEX(TOPS,USENAM,COMOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up for and control a menu level.
C-                         Called from MENUDO and MENCTR to avoid recursive
C-                         calling of routines.
C-
C-   Inputs  : TOPS:   Title for top of display
C-             USENAM: Name of menu level to use
C-   Outputs : COMOUT: Unique command identifier to returned to dispatch
C-                     loop
C-   Controls: None
C-
C-   Created  22-SEP-1988   Jan S. Hoftun
C-   Updated  25-SEP-1991   Herbert Greenlee
C-   Modified 14-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOPS,USENAM,COMOUT
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      CHARACTER*40 BLNK,TNAM,ADDL
      CHARACTER*132 MSGLIN,TRANUP
      CHARACTER*64 PRTNAM
      INTEGER I,USELEV,K,TRULEN,J
      LOGICAL ONEUSE,TOPLEV(MAXLEV)
      DATA BLNK/' '/,TOPLEV/MAXLEV*.TRUE./
      CHARACTER*132 FILNAM
      INTEGER IERR,IOS
      INTEGER ISTS,ISTV,IUNI
C----------------------------------------------------------------------
      USELEV=0
      TNAM=TRANUP(USENAM)
      DO 5000 I=1,UPRLEV
        IF(NAMLEV(I).EQ.TNAM) THEN
          USELEV=I
          GOTO 5001
        ENDIF
 5000 CONTINUE
 5001 CONTINUE
C
C     Set up current level of menu
C
      IF(USELEV.EQ.0) THEN
        CALL OUTMSG('0No match with menu name: '//TNAM//CHAR(7))
        CALL OUTMSG(' ')
        COMOUT='EXIT'     ! To avoid infinite loop by having this
C                         ! routine called again
      ELSE
C
C     Make the first menu used the 'main' menu if MAILEV = 0
C
        IF(MAILEV.EQ.0) THEN
          MAILEV=USELEV
          IF(BEGJOU) THEN      ! Open journal file now that we know what
                               ! the main level is.
            PRTNAM=NAMLEV(USELEV)
            IF(PRTNAM.EQ.'MENUDEF') THEN
              PRTNAM=NAMLEV(USELEV)(1:TRULEN(NAMLEV(USELEV)))//
     &               '$MENUDEF'
            ENDIF
            CALL GTUNIT(555,COMUNI,IERR)
            IF(IERR.EQ.0) THEN
C
C     OPEN journal file unit
C
              FILNAM=TRANUP(PRTNAM(1:TRULEN(PRTNAM))//'$'//JOUNAM)
              K=TRULEN(FILNAM)
C&IF VAXVMS
              OPEN(COMUNI,FILE=FILNAM(1:K),DEFAULTFILE='.INP',
     &          STATUS='NEW',IOSTAT=IOS)
C&ELSE
C&              I = K
C&              DO WHILE (I .GE. 1 .AND. FILNAM(I:I) .NE. '/')
C&                IF (FILNAM(I:I) .EQ. '.') GOTO 100
C&                I = I-1
C&              ENDDO
C&              FILNAM = FILNAM(1:K) // '.INP'
C&              K = K + 4
C& 100          OPEN(COMUNI,FILE=FILNAM(1:K),
C&     &          STATUS='NEW',IOSTAT=IOS)
C&ENDIF
              IF(IOS.NE.0) THEN
                CALL OUTMSG('0OPEN on JOURNAL file->'//FILNAM(1:K)//
     &               '<-failed!'//CHAR(7))
                CALL ERRSNS(I,ISTS,ISTV,IUNI,IOS)
                CALL MSGSCR(IOS,' Reason-->')
                CALL MSGSCR(ISTS,' Secondary reason-->')
              ELSE
                LOGUP=.TRUE.
                OLDLEV=USELEV
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        SAVLEV=USELEV
C
C     Put title line in the center of TOPMEN if in setup mode or
C     in toplin(0) for safe-keeping if not in setup mode
C
        IF(.NOT.ONEFLG) THEN   ! Titles not needed in ONEFLG mode
          IF(SAVLEV.EQ.MAILEV.AND.CURLEV.NE.MAILEV) CURLEV=0
          IF(TOPS.NE.' ') THEN
            IF(TOPLEV(SAVLEV)) THEN   ! Titles should be done only once in this mode
              TOPLEV(SAVLEV)=.FALSE.
              CURLEV=0
            ENDIF
            K=MAX((PBCOLS/2-LEN(TOPS)-1)/2,0)
            WRITE(TOPLIN(0,SAVLEV),101) BLNK(1:K),
     *               TOPS
  101       FORMAT(4(A:))
          ELSE
            IF(TOPLEV(SAVLEV)) THEN   ! Titles should be done only once in this mode
              TOPLEV(SAVLEV)=.FALSE.
              K=INDEX(NAMLEV(MAILEV),'_')
              IF(K.NE.0) THEN
                ADDL=NAMLEV(MAILEV)(1:K-1)
              ELSE
                ADDL=NAMLEV(MAILEV)
              ENDIF
              J=TRULEN(MENLIN(POS,CURLEV))
              K=MAX((PBCOLS/2-J-1)/2,0)
              WRITE(TOPLIN(0,SAVLEV),101) BLNK(1:K),
     *               MENLIN(POS,CURLEV)(1:J),
     *               BLNK(1:K-TRULEN(ADDL)-1),
     *               ADDL(1:TRULEN(ADDL))
            ENDIF
          ENDIF
        ENDIF
        CALL MENEXG(COMOUT)
      ENDIF
      RETURN
      END
