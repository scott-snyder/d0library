C CMS REPLACEMENT HISTORY, Element QCD_JET_CORR.INC
C *3    13-OCT-1995 15:29:40 MEENA "Dhiman Chakraborty: CAFIX V 5.0"
C *2    20-FEB-1994 19:40:37 MEENA "Richard V. Astur: Needed for Jet Correction version -4.0"
C *1    30-JUL-1993 13:07:52 GREENLEE "QCD jet correction common"
C CMS REPLACEMENT HISTORY, Element QCD_JET_CORR.INC
C----------------------------------------------------------------------
C-
C-   Created   5-JUL-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
C: Jet parameters
      INTEGER   njetpar
      PARAMETER( njetpar = 8 )
      REAL old_jet_e, old_jet_et, old_jet_eta, old_jet_phi
      REAL old_e_fract(3), old_jet_size
      REAL new_jet_e, new_jet_et, new_jet_eta, new_jet_phi
      REAL new_e_fract(3), new_jet_size
      REAL old_jet_par(8)
      REAL new_jet_par(8)

      COMMON /QCD_JET_CORR/old_jet_e,old_jet_et,old_jet_eta,
     &  old_jet_phi, old_e_fract,old_jet_size, new_jet_e, new_jet_et,
     &  new_jet_eta, new_jet_phi, new_e_fract, new_jet_size

      EQUIVALENCE( old_jet_par(1), old_jet_e )
      EQUIVALENCE( new_jet_par(1), new_jet_e )
