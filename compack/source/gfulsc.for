      SUBROUTINE GFULSC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if FULLSCREEN qualifier was entered
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-MAY-1989   Jan S. Hoftun (pulled form SETCHK)
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER ISTAT,CLI$PRESENT
      LOGICAL GETDEV
      EXTERNAL CLI$_PRESENT
C----------------------------------------------------------------------
      ISTAT=CLI$PRESENT('FULLSCREEN')
      IF(ISTAT.EQ.%LOC(CLI$_PRESENT).AND..NOT.ONEFLG) THEN
C                                    !NO full screen when only one command
        FULSCR=GETDEV()
      ENDIF
C&ENDIF
  999 RETURN
      END
