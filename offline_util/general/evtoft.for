      SUBROUTINE EVTOFT(IVAX, ITIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts an event time in VAX 64-bit format
C-                         to D0 standard time.  For NOW it is assumed
C-                         that the event time has offset=0, i.e.
C-                         IVAX is FNAL time
C-
C-   Inputs  : IVAX      VAX format time of event
C-   Outputs : ITIM        D0 standard time
C-   Controls: 
C-
C-   Created  29-JUN-1989   Jason McCampbell (MSU)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IVAX(2), ITIM, D0TLOCT
C----------------------------------------------------------------------
      CALL VAXOFT(IVAX,ITIM)
C       remove FNAL time offset
      ITIM=D0TLOCT(ITIM)
C
  999 RETURN
      END
