      SUBROUTINE MENUDO(TOPS,USENAM,COMOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main user-level control of one menu level
C-                         Only returns when a 'user' command entered
C-
C-   Inputs  : TOPS:   Title for top of display
C-             USENAM: Name of menu level being used
C-   Outputs : COMOUT: Chosen command for return to dispatch loop
C-   Controls: None
C-
C-   Created  22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOPS,USENAM,COMOUT
      LOGICAL GOTOP
C----------------------------------------------------------------------
      CALL MENUEX(TOPS,USENAM,COMOUT)
    1 CONTINUE
      IF(COMOUT.EQ.'MENCTR'.AND..NOT.GOTOP()) THEN
        CALL MENCTR
        COMOUT='BLANK'
        IF(.NOT.GOTOP()) CALL MENUEX(TOPS,USENAM,COMOUT)
        GOTO 1
      ENDIF
      RETURN
      END
