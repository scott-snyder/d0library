      SUBROUTINE GSPLIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check for /SPLITMODE qualifier
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  26-MAY-1989   Jan S. Hoftun
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER ISTAT,CLI$PRESENT
      EXTERNAL CLI$_PRESENT
C----------------------------------------------------------------------
      ISTAT=CLI$PRESENT('SPLITMODE')
      IF(ISTAT.EQ.%LOC(CLI$_PRESENT).AND..NOT.ONEFLG) THEN      !NO
C                                         ! split screen when only one command
        SPLFLG=.TRUE.
        CALL SPLTIT
      ENDIF
C&ENDIF
  999 RETURN
      END
