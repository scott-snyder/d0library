      FUNCTION PXDISPLAY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This function executes the D0 Event Display.
C-   It handles the calling of PXMAIN. After exit from PIXIE one can
C-   use the entry point PXDISPLAY_THIS_EVENT to re-activate PXDISPLAY.
C-
C-   Returned value  : TRUE always
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  19-SEP-1990   Harrison B. Prosper, Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PXDISPLAY,PXDISPLAY_THIS_EVENT,FLGVAL
C----------------------------------------------------------------------
      LOGICAL FIRST,NOMORE
      DATA FIRST/.TRUE./
      SAVE FIRST,NOMORE
C----------------------------------------------------------------------
      PXDISPLAY = .TRUE.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL FLGBK ('CALL_PXMAIN',1)
        CALL FLGSET('CALL_PXMAIN',.TRUE.)
      ENDIF
C
      IF ( FLGVAL('CALL_PXMAIN') ) THEN
        CALL PXMAIN(NOMORE)
        CALL FLGSET('CALL_PXMAIN',.NOT.NOMORE)
      ENDIF
      RETURN
C
      ENTRY PXDISPLAY_THIS_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Entry point to re-activate the D0 Event 
C-   Display.
C-
C-   Created  19-SEP-1990   Harrison B. Prosper, Lupe Howell
C-
C----------------------------------------------------------------------
      CALL FLGSET('CALL_PXMAIN',.TRUE.)
  999 RETURN
      END
