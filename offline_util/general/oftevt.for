      SUBROUTINE OFTEVT(ITIM, IVAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert D0 standard time to event time.  It
C-                         is assumed, for NOW, that offset=0 between
C-                         D0 and event times.
C-
C-   Inputs  : ITIM        D0 standard time to convert
C-   Outputs : IVAX      VAX time of event
C-   Controls: None
C-
C-   Created  29-JUN-1989   Jason McCampbell (MSU)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITIM, IVAX(2)
C----------------------------------------------------------------------
      CALL OFTVAX(ITIM, IVAX)
C
  999 RETURN
      END
