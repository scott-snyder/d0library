      SUBROUTINE ENDLOG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End logging to command file
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: LOGUP in /COMNUM/ is set to .FALSE.
C-
C-   Documented  22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INTEGER IERR
C----------------------------------------------------------------------
      IF(LOGUP) THEN
        LOGUP=.FALSE.
C
C     CLOSE command file unit
C
        CLOSE (UNIT=COMUNI)
        CALL RLUNIT(555,COMUNI,IERR)
      ENDIF
      RETURN
      END
