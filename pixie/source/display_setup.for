      FUNCTION DISPLAY_SETUP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check that PIXIE setup files are available.
C-   Book flag EVENT_DISPLAY.
C-
C-   Returned value  : TRUE
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-SEP-1988   K. Wyatt Merritt
C-   Updated   2-FEB-1990   Harrison B. Prosper
C-      Remove PCINIT
C-   Updated  21-FEB-1991   Harrison B. Prosper  
C-      Made compatible with PIXIE V3.0_01 
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DISPLAY_SETUP
C----------------------------------------------------------------------
      DISPLAY_SETUP = .TRUE.
      CALL FLGBK ('EVENT_DISPLAY',1)
  999 RETURN
      END
