      real function top_mark_isr_likelihood (jet, com_frame, rcp_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Given a JET and the 4-vector for the center-of-mass frame
C-     for the event, returns the `likelihood' that the jet is from
C-     an initial state parton.
C-
C-   Returned value  : 
C-     `likelihood' value, as described above.
C-
C-   Inputs  : 
C-     jet : The putative FSR jet.
C-     com_frame : The 4-vector of the center-of-mass frame for
C-                 the event.
C-     rcp_name : RCP bank giving the location of the distribution
C-                histograms.  Pass a blank string to use the default.
C-
C-   Outputs : 
C-   Controls: 
C-   Bugs:
C-     Explanation above should be clearer.
C-
C-   Created  21-FEB-1994   scott snyder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      real jet(4), com_frame(4)
      character*(*) rcp_name

      real p_jet, theta, phi, eta, boosted_jet(4)

      real     top_jet_likelihood1, vmod
      external top_jet_likelihood1, vmod
C----------------------------------------------------------------------
c
c *** Boost the jet to the top frame 
c
      call lboost (com_frame, 1, jet, boosted_jet)
      call ispeta (boosted_jet, theta, phi, eta)
      p_jet = vmod (boosted_jet, 3)
c
c *** Compute the `likelihood' function.
c
      top_mark_isr_likelihood =
     &     top_jet_likelihood1 (log10 (p_jet), sqrt (abs (eta)),
     &                          'MARK_ISR_ALL_ID',
     &                          'MARK_ISR_ISR_ID',
     &                          rcp_name)

  999 RETURN
      END
