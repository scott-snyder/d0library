      SUBROUTINE MENUDO(ATITLE,MNUNAM,COMOUT)
C-   Purpose and Methods : Main user-level control of one menu level.
C-                         Only returns when a 'user' command entered.
C-   Inputs  : ATITLE:   Title for top of display
C-             MNUNAM: Name of menu level being used
C-   Outputs : COMOUT: Chosen command for return to dispatch loop.
      CHARACTER*(*) ATITLE,MNUNAM,COMOUT
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
      CALL MENUEX(ATITLE,MNUNAM,COMOUT)
      RETURN
      END
