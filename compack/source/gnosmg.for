      SUBROUTINE GNOSMG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if /NOSMG qaulifier was present
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-MAY-1989   Jan S. Hoftun
C-
C-   Modified  4-NOV-1992   Soren G. Frederiksen
C-                          For non-VMS machine turn of Full screen and
C-                          SMG mode.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$COMPACK$SOURCE:COMMSG.DEF'
      INTEGER ISTAT,CLI$PRESENT
C&IF VAXVMS
      EXTERNAL CLI$_NEGATED
C----------------------------------------------------------------------
      ISTAT=CLI$PRESENT('SMG_MODE')
      IF(ISTAT.EQ.%LOC(CLI$_NEGATED)) THEN  !Force NO use of screen routines
        FULSCR=.FALSE.    !Force FULLSCREEN mode off even if it was on
        SMGON=.FALSE.
        CALL MSGSCR(COMPACK__NOSMG,' ')
      ENDIF
C&ELSE
C&      FULSCR=.FALSE.    !Force FULLSCREEN mode off even if it was on
C&      SMGON=.FALSE.
C&      CALL MSGSCR(COMPACK__NOSMG,' ')
C&ENDIF
  999 RETURN
      END
