      SUBROUTINE INPTRM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : it sets control variables so that command
C-                         input is comming from the terminal
C-
C-   Controls: INPLUN is set to 5
C-             RDCOM  is set to FALSE
C-             FULSCR is set to FSAVE
C-             TRMFLG is set to SAVTRM
C-             SAVTRM is set to FALSE
C-             FSAVE  is set to FALSE
C-
C-   Created:  27-Oct-1989 Penelope Constanta-Fanourakis
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
C----------------------------------------------------------------------
C
C       If reading from terminal is set do nothing.
C
      IF(INPLUN.EQ.5) RETURN
C
C       Set input LUN to be COMLUN
C
      INPLUN=5
C
C       Reset terminal variables to their old values.
C
      TRMFLG=SAVTRM
      SAVTRM=.FALSE.
      FULSCR=FSAVE
      FSAVE=.FALSE.
      RDCOM=.FALSE.
      POS=1
      IF(.NOT.ONEFLG) THEN
        CALL PFWAIT
        CALL MENDIS(.TRUE.)
      ENDIF

      RETURN
      END
