      FUNCTION D0HPLT_DISPATCH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Provide HPLOT histograms from the EXAMINE
C-   menu.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-FEB-1990   Harrison B. Prosper
C-   Updated   1-MAY-1990   Harrison B. Prosper  
C-      Removed call to MENUDO
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL D0HPLT_DISPATCH
C----------------------------------------------------------------------
      D0HPLT_DISPATCH = .TRUE.
      CALL FLGSET ('HISTOGRAM_DISPLAY',.TRUE.)
      CALL D0HPLT_EVENT             ! Dispatch menu D0HPLT
      CALL FLGSET ('HISTOGRAM_DISPLAY',.FALSE.)
      RETURN
      END
