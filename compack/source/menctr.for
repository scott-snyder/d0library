      SUBROUTINE MENCTR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dispatch routine for system menu 'Control of Menu'
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(80) COMIN
      LOGICAL GOTOP
C----------------------------------------------------------------------
    1 CONTINUE
      COMIN=' '
      CALL MENUEX(' ','MENUDEF',COMIN)
C
C     Start the search for predefined commands
C
      IF(COMIN.NE.'EXIT'.AND..NOT.GOTOP()) THEN
        IF(INDEX(COMIN,'CHCOLS').GT.0) THEN
          CALL CHCOLS
        ELSEIF(INDEX(COMIN,'CHSPAC').GT.0) THEN
          CALL CHSPAC
        ELSEIF(COMIN.NE.'EXIT'.AND..NOT.GOTOP()) THEN
          CALL INTMSG('0No action defined for that command!'//CHAR(7))
        ENDIF
        GOTO 1
      ENDIF
      RETURN
      END
