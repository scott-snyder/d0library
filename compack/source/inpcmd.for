      SUBROUTINE INPCMD ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : it sets control variables so that command
C-                         input is comming from file
C-
C-   Controls: INPLUN is set to COMLUN
C-             RDCOM  is set to TRUE
C-             FULSCR is set to FALSE
C-             TRMFLG is set to FALSE
C-             SAVTRM holds old value of TRMFLG
C-             FSAVE  holds old value of FULSCR
C-             PF     is set to 0
C-
C-   Created:  27-Oct-1989 Penelope Constanta-Fanourakis
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'

      CHARACTER*132 NEWTOP
      CHARACTER*80  MSGLIN
C----------------------------------------------------------------------
C
C       If reading from command file is set do nothing.
C
      IF(INPLUN.EQ.COMLUN) RETURN
C
C       If no command file is open then no need to set anything
C
      IF (.NOT. CMDOPEN) RETURN
C
C       Set input LUN to be COMLUN
C
      INPLUN=COMLUN
C
C       Set TRMFLG to .FALSE to indicate reading from a file.
C
      SAVTRM=TRMFLG
      TRMFLG=.FALSE.
C
C       Check if in full screen mode. If so display information
C
      IF(FULSCR) THEN
        CALL LIBERA(1,1)
        NEWTOP='     Reading commands from command file'
        CALL LIBBIG(NEWTOP,1,1,3)
        CALL PFLABL('AGAIN','HELP',' ','BACK')
        CALL LIBCUR(3,1)
      ELSE
        WRITE(MSGLIN,206)
  206   FORMAT('0Reading Commands from command file')
        CALL OUTMSG(MSGLIN)
        CALL OUTMSG(' ')
      ENDIF
C
C      Turn off full screen mode while reading commands from file
C
      FSAVE=FULSCR
      FULSCR=.FALSE.
      RDCOM=.TRUE.
      PF=0

      RETURN
      END
