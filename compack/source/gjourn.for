      SUBROUTINE GJOURN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : See if JOURNAL mode was entered
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-MAY-1989   Jan S. Hoftun
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER ISTAT,CLI$PRESENT,CLI$GET_VALUE
      EXTERNAL CLI$_PRESENT
C----------------------------------------------------------------------
      ISTAT=CLI$PRESENT('JOURNAL')
      IF(ISTAT.EQ.%LOC(CLI$_PRESENT)) THEN
        ISTAT=CLI$GET_VALUE('JOURNAL',JOUNAM)
        BEGJOU=.TRUE.
      ENDIF
C&ENDIF
  999 RETURN
      END
