      SUBROUTINE DISPLAY_ON
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Turn flag EVENT_DISPLAY ON. This flag 
C-   controls the package PIXIE. Use entry point:
C-   
C-      DISPLAY_OFF 
C-      
C-   to turn flag OFF.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-APR-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL FLGSET('EVENT_DISPLAY',.TRUE.)
      RETURN
C
      ENTRY DISPLAY_OFF
      CALL FLGSET('EVENT_DISPLAY',.FALSE.)
  999 RETURN
      END
