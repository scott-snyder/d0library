
      SUBROUTINE INIMEN
C---------------------------------------------------------------------
C-                                                                   -
C-    Initialize all menus and menu flags                            -
C-                                                                   -
C-                       SDP Nov.,1988                               -
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
      CALL MENSET('SETUP')          ! get menu file for initialization
      CALL MENSET('SUMMARIES')      ! get menu file for output files
      CALL MENSET('DONE_WITH_DATA') ! get menu file for end-of-run
      CALL MENSET('INTERRUPT')      ! get menu file for event interrupt
      CALL MENSET('D0H_OPTIONS')    ! get menu file for D0HPLT
      CALL MENSET('EXAMINE')        ! get interrupt menu file for D0HPLT
      CALL MENSET('MENUDEF')        ! get system menu file 
      CALL SPLSTA                   ! set a status screen
      CALL SPLTIT                   ! always start with split screen
      CALL STAMSG(' NO EVENTS PROCESSED YET',.TRUE.)
C
C   initialize flags for INTERRUPT menu to false
      CALL SETINT(.FALSE.)
      CALL SETINQ(.FALSE.)
C
      CALL SETNMR(.FALSE.) ! set NOMORE flag to false
C
      RETURN
      END
