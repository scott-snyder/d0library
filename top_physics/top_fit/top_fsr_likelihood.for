      real function top_fsr_likelihood (jet1, jet2, rcp_name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Given two jet 4-vectors JET1 and JET2, returns the `likelihood'
C-     that they came from the same hard parton.
C-
C-     The `likelihood' is a function of the joint distribution of the
C-     invariant mass of the two jets and their distance in \Delta R
C-     space.  It is derived by plotting the histograms in these two
C-     variables of an ensemble of FSR jet pairs only, and an ensemble
C-     of FSR jet pairs plus a background of incorrect jet combinations.
C-     The `likelihood' is then the ratio of these histograms
C-     (cf. Baye's theorem).
C-
C-   Returned value  :
C-     `likelihood' value, as described above.
C-
C-   Inputs  :
C-     jet1, jet2 : The putative FSR pair.
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
      real jet1(4), jet2(4)
      character*(*) rcp_name

      real dr, mjj, theta, phi1, phi2, eta1, eta2, dum5(5)

      real     top_jet_likelihood1, pair_mass
      external top_jet_likelihood1, pair_mass
C----------------------------------------------------------------------
c
c *** Compute the invariant mass and \Delta R of the two jets.
c
      call ispeta (jet1, theta, phi1, eta1)
      call ispeta (jet2, theta, phi2, eta2)
      dr = sqrt ((phi1-phi2)**2 + (eta1-eta2)**2)

      mjj = pair_mass (jet1, jet2, dum5)
c
c *** Compute the `likelihood' function.
c
      top_fsr_likelihood = top_jet_likelihood1 (mjj, dr,
     &                                          'FSR_ALL_ID',
     &                                          'FSR_FSR_ID',
     &                                          rcp_name)

  999 RETURN
      END
