      SUBROUTINE GCONFI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if /CONFIRM_END qaulifier was present
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  26-MAY-1989   Jan S. Hoftun (pulled form SETCHK)
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$COMPACK$SOURCE:COMMSG.DEF'
      INTEGER ISTAT,CLI$PRESENT
      EXTERNAL CLI$_NEGATED
C----------------------------------------------------------------------
      ISTAT=CLI$PRESENT('CONFIRM_END')
      IF(ISTAT.NE.%LOC(CLI$_NEGATED).AND..NOT.ONEFLG) THEN
C                                         !NO CONFIRM in single command mode
        ENDFLG=.TRUE.
      ELSE
        ENDFLG=.FALSE.
      ENDIF
C&ENDIF
  999 RETURN
      END
