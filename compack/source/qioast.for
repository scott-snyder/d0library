      SUBROUTINE QIOAST(INID,DSPNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : AST routine which is called when there is an 
C-                         unsolicited keyboard entry on the terminal.
C-                         VAX-specific
C-
C-       It first gets the command by calling MENEXG and then calls the user
C-       supplied dispatch routine DSPNAM
C-
C-       NOTICE:   The enable of the next read DOES NOT have to called from
C-                 here, it stays active until cancelled in CANMEN.
C-                 
C-
C-   Inputs  : INID:   The display ID of the pasteboard associated with
C-                     the entry
C-             DSPNAM: User supplied dispatch routine
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated     4-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INID
      EXTERNAL DSPNAM
      CHARACTER*80 COMIN
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER ASTLEV,SYS$WAKE,ISTAT,LOCPOS
      LOGICAL DISFLG,LOCDIS
C----------------------------------------------------------------------
      ASTLEV=CURLEV
      COMIN=' '
      QIOFLG=.FALSE.
      DISFLG=.FALSE.
      IF(LOCDIS) THEN                ! MENU WASN'T PUT ON SCREEN BEFORE
        NODISP=.FALSE.              ! MAKE SURE IT IS NOW
        LOCDIS=.FALSE.
        CALL MENDIS(.TRUE.)
      ENDIF
      CALL MENEXG(COMIN)
      LOCPOS=POS
      IF(COMIN.EQ.'MENCTR') THEN
        NODISP=.FALSE.            !Make sure menu is displayed
        CALL MENCTR
        DISFLG=.TRUE.
      ELSE
        NODISP=.FALSE.            !Make sure menu is displayed
        IF(COMIN.NE.'BLANK'.AND.COMIN.NE.'EXIT'.AND.COMIN.NE.' ') THEN
          CALL DSPNAM(COMIN)
          DISFLG=.TRUE.                         !To redisplay menu when
C                                               !coming back
        ENDIF
        IF(NODISP.AND.FULSCR) THEN
          LOCDIS=.TRUE.
        ENDIF
        IF(.NOT.FULSCR.AND.PF.NE.3) DISFLG=.TRUE.   !To get prompt back on screen
      ENDIF
      IF(.NOT.ASTFLG.AND..NOT.CANFLG) THEN      !Don't restart when CANMEN has been called
        ASTFLG=.TRUE.                           !Had been in a sub-level
        CURLEV=ASTLEV
        SAVLEV=CURLEV
        DISFLG=.TRUE.
      ENDIF
      IF(QIOFLG.AND..NOT.CANFLG) THEN           !Don't restart when CANMEN has been called
        CALL SETQIO(DSPNAM)
C&IF VAXVMS
        ISTAT=SYS$WAKE(,)                       !To make sure everything is awake again
C&ENDIF
C                                               !A SPAWN in the submenu would 
C                                               !have upset the scheduled wake
        DISFLG=.TRUE.
      ENDIF
      IF(DISFLG.AND..NOT.CANFLG) THEN    !Don't redisplay menu when CANMEN called
        POS=LOCPOS
        CALL MENDIS(.TRUE.)              !Using DISFLG to make sure MENDIS gets called only once
        IF(LOCDIS.AND.FULSCR) CALL PFLABL(' ',' ',' ','MENU')
      ENDIF
      RETURN
      END
