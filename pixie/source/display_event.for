      FUNCTION DISPLAY_EVENT()
      ENTRY DISPLAY_EVENT1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Execute the D0 event DISPLAY.
C-
C-   Returned value  : TRUE
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-SEP-1988   K. Wyatt Merritt
C-   Updated   2-FEB-1990   Harrison B. Prosper
C-      Removed application code
C-   Updated   5-FEB-1990   Harrison B. Prosper
C-      Removed test on DISPLAY_ON
C-   Updated  15-FEB-1991   Harrison B. Prosper  
C-      Made compatible with new PIXIE (V3.0_01) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FLGVAL,QUIT
      LOGICAL DISPLAY_EVENT
      LOGICAL DISPLAY_EVENT1
C----------------------------------------------------------------------
      DISPLAY_EVENT1= .TRUE.
      DISPLAY_EVENT = .TRUE.
C
      IF (FLGVAL('EVENT_DISPLAY')) THEN
        CALL CANMEN                     ! Cancel interrupt menu
        CALL PXMAIN(QUIT)
        CALL FLGSET('EVENT_DISPLAY', .NOT. QUIT)
      ENDIF
  999 RETURN
      END
