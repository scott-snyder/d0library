      FUNCTION ZTR_EXM_VIEW_HISTOS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Remove D0HPLT menu items for Examine.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-OCT-1992   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL ZTR_EXM_VIEW_HISTOS
C
C----------------------------------------------------------------------
C
      ZTR_EXM_VIEW_HISTOS = .TRUE.
C
      CALL MENSUB('D0HPLT','START Updating Plot')
      CALL MENSUB('D0HPLT','Plot a Histogram')
      CALL MENSUB('D0HPLT','STOP Updating Plot')
      CALL MENSUB('D0HPLT','Index of histograms')
      CALL MENSUB('D0HPLT','Change Directory')
      CALL MENSUB('D0HPLT','Laser Plot')
      CALL MENSUB('D0HPLT','Clear/Reset')
      CALL MENSUB('D0HPLT','LEGO Plot')
      CALL MENSUB('D0HPLT','List PAWC DIR')
      CALL MENSUB('D0HPLT','Show Histogram Title')
      CALL MENSUB('D0HPLT','Header')
      CALL MENSUB('D0HPLT','Type Histogram')
      CALL MENSUB('D0HPLT','Print Histogram')
      CALL MENSUB('D0HPLT','Preceding Plot')
      CALL MENSUB('D0HPLT','Same Plot')
      CALL MENSUB('D0HPLT','Next Plot')
      CALL MENSUB('D0HPLT','Zone plots')
C
  999 RETURN
      END
