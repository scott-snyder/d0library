      SUBROUTINE qcd_jet_correction(ljets,do_zsp_corr,do_undevt_corr,
     &  do_algo_losses_corr, z_vertex, isys, ijet, keep_biased,
     &  out_jet_e,out_jet_et, out_jet_eta,ier )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return corrected Jet Energy, ET and Eta
C-
C-   Inputs  : LJETS          [I] - Zebra pointer to JETS bank of the jet to
C-                                  correct
C-             DO_ZSP_CORR    [L] - Set TRUE if Zero Suppression correction is
C-                                  to be applied.
C-             DO_UNDEVT_CORR [L] - Set TRUE if Underlying Event correction is
C-                                  to be applied.
C-             DO_ALGO_LOSSES_CORR [L] - Set TRUE if out of cone showering
C-                                  correction is to be applied.
C-             Z_VERTEX       [R] - Z position of vertex
C-             ISYS           [I] - 0=nominal correction, 1=low correction
C-                                  2=high correction
C-             IJET           [I] - #of jet in list from RECO
C-             KEEP_BIASED    [L] - to force bias correction off for MET pass
C-   Outputs :
C-              OUT_JET_E     [R] - Corrected jet Energy
C-              OUT_JET_ET    [R] - Corrected jet Transverse energy
C-              OUT_JET_ETA   [R] - Corrected jet Eta
C-              IER           [I] - Error code
C-                                  0 = okay
C-                                 -1 = Cant read RCP, -2=Cant find RCP bank
C-                                 -3 = Couldnt read some RCP parameters
C-   Controls:
C-              Use QCD_JET_CORRECTION_RCP
C-
C-      ** Note: errors are described by their effect on corrected jet energy
C-      ** error(1,1) = low statistical/uncorrelated error
C-      ** error(2,1) = high statistical/uncorrelated error
C-      ** error(1,2) = low systematic/correlated error
C-      ** error(2,2) = high systematic/correlated error
C-
C-   Created   5-JUL-1993   Richard V. Astur
C-   Updated   8-SEP-1993   Meenakshi Narain
C-                          ADD  QCD_JET_CORRECTION_3 ENTRY POINT
C-                          which enables user to control selection of response
C-                          curve for a different cone size than that of the
C-                          selected jet.
C-                          Add SET_USER_CONE variable in main routine
C-                          to enable this selection
C-   Modified OCT-11-1993   R. Astur - Subtract eta dependant noise/underlying
C-                          1) Add 5% system uncertainty in method to errors
C-                          2) Add montecarlo correction routine to be called
C-                             for montecarlo events
C-                          3) Fix bug which used R=.5 instead of R=.3
C-
C-   Modified 11-NOV-1993   R. Astur - Restructure for version 4.0
C-   Modified 22-NOV-1994   R. Astur - vers 4.2 "Fix ISYS switch for MC
C-                                     correction, do EMF after MPF-DIJET"
C-   Updated   9-MAY-1995   R. Astur, Bob Kehoe - updated for version 5.0
C-   Updated  Sep-01-1995   Bob Kehoe -- added qcd_jet_correction_quans,
C-                              propagate errors
C-   Updated  Oct-12-1995   Bob Kehoe -- provide min_jet_et array to decide when
C-                              not to correct jets, interpolate up to
C-                              jet_et_threshold, fix errors
C-   Updated  Oct-18-1995   Bob Kehoe -- bug fix
C-   Updated  Dec-04-1995   Bob Kehoe -- prevent divide by zero
C-   Updated   6-DEC-1995   Dhiman Chakraborty
C-                          Get rid of unnecessary DATA assignments
C-   Updated  Dec-13-1995   Bob Kehoe -- correct ICD bug which added energy
C-                              instead of removing it in reco 10 and 11
C-   Updated  14-APR-1996   Bob Hirosky -- Add ZSP/UND words to jet_quans
C-   Updated  26-JUN-1997   Bob Hirosky  Version 5.1:
C-                              limit max # jets for unbiasing
C-                              add switch to turn off low_et bias
C-                                correction for MET pass
C-                              use correct response,etc  w/o ULE/NOISE
C-                              update jet_quans
C-   Updated  21-JUL-1997   Bob Hirosky - add response_systematics call
C-   Updated  28-OCT-1997   Bob Hirosky - use full cor. factor in quans(5)
C-   Updated   9-DEC-1997   Bob Hirosky - fix for IBM compilation error
C-   Updated   9-JAN-1998   Bob Hirosky - fix to upper sys. error
C-   Updated   9-FEB-1998   Bob Hirosky - add SAVE to FIRST
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QCD_JET_CORRECTION.INC'
      INTEGER ljets, isys, ijet, ier
      LOGICAL do_zsp_corr, do_undevt_corr, do_algo_losses_corr,
     &  keep_biased
      REAL z_vertex, out_jet_eta, out_jet_e
C
      REAL small
      PARAMETER( small = .000001 )
C
C rcp params
C
      INTEGER mc_rcp, njets_to_unbias
      SAVE    mc_rcp, njets_to_unbias
      LOGICAL do_bias_correction, unbias_all_jets,
     &  do_det_scale_correction, do_width_correction,
     &  do_response_correction
      SAVE do_bias_correction, unbias_all_jets,
     &  do_det_scale_correction, do_width_correction,
     &  do_response_correction
      REAL min_jet_et(5)
      SAVE min_jet_et
C
C ****  correction/error accounting
C
C: Noise, etc
      REAL under_err, zsp_err, zsp_err_sys, under_e, under_et,
     &  zsp_e, zsp_et, zsp_e_err, zsp_e_err_sys, under_e_err, noise(8)
      SAVE under_err, zsp_err, zsp_err_sys, under_e, under_et,
     &  zsp_e, zsp_et, zsp_e_err, zsp_e_err_sys, under_e_err
C
      REAL cfactor, cfactor_error(2,2), low_et_bias,
     &  low_et_bias_err(2,2), det_scale, det_scale_err(2,2),
     &  rel_correction_width, response, response_err(2,2), out_of_cone,
     &  ooc_err(2,2), zsp_frac, zsp_frac_err(2,2), und_frac,
     &  und_frac_err(2,2), phy_bck_error, sys_bias_error_low,
     &  sys_bias_error_high, kt_err, topo_corr
      SAVE cfactor, cfactor_error, low_et_bias, low_et_bias_err,
     &  det_scale, det_scale_err, rel_correction_width,
     &  response, response_err, out_of_cone, ooc_err, zsp_frac,
     &  zsp_frac_err, und_frac, und_frac_err, phy_bck_error,
     &  sys_bias_error_low, sys_bias_error_high, kt_err, topo_corr
      REAL error
      REAL jet_quans(50)
C
      INTEGER icone, oldicone, user_icone, nalgor
      REAL user_cone
      SAVE user_cone, user_icone
      REAL jet_et_threshold
      REAL out_jet_et,delta
      REAL cone_used, z_vertex_used
      SAVE cone_used
      REAL olde, oldet, oldeta, oldphi
      REAL oldemf, oldicdf, oldchf, oldsiz, oldcone
      REAL det_eta, peta_to_deta
      INTEGER lcaph, ialgo, istat
      LOGICAL monte_carlo_data
      LOGICAL below_et_threshold
      LOGICAL em_removed, set_user_cone, this_is_monte_carlo
      SAVE    set_user_cone
      REAL underet, zeroset
      COMMON /jet_aux/underet,zeroset
      CHARACTER*80 msg
      DATA msg / ' Jet Correction V5.1 ' /
      LOGICAL first
      DATA first /.true./
      SAVE first
C
      REAL qad, qsb, ex, ey
      REAL MAXABS, X1
C: Statement Function: qad - to add errors in quadrature
C: Statement Function: qsb - to subtract errors in quadrature
C
      QAD( ex,ey ) = sqrt( ex*ex + ey*ey )
      QSB( ex,ey ) = sqrt( ex*ex - ey*ey )
C----------------------------------------------------------------------
C
C: Statement function : MAXABS(x1) return x1 if abs(x1) > small else
C:                      return small (with the sign of x1)
C
      MAXABS( X1 ) = MAX( SMALL, ABS((X1)) ) * SIGN( 1., (X1) )
C----------------------------------------------------------------------
C
C: Initialize
C
      IF ( first ) THEN
        first = .false.
C: Report version
        CALL intmsg(msg)
C: READ RCP
        CALL inrcp('QCD_JET_CORRECTION_RCP', ier )
        IF ( ier .NE. 0 ) THEN
          CALL errmsg('RCP error','QCD_JET_CORRECTION',
     &      'Cant read in RCP file','F')
          ier = -1
          GOTO 999
        ENDIF
C: Select RCP bank
        CALL ezpick('QCD_JET_CORRECTION_RCP')
        CALL ezerr( ier )
        IF ( ier .NE. 0 ) THEN
          CALL errmsg('RCP error','QCD_JET_CORRECTION',
     &      'Cant find bank ','F')
          ier = -2
          GOTO 999
        ENDIF
C: Read parameters...
C: obtain minimum jet Et to correct
        CALL ezgeta('MIN_JET_ET',0,0,0,nalgor,ier)
        IF (ier.EQ.0)
     &    CALL ezgeta('MIN_JET_ET',1,nalgor,1,min_jet_et,ier)
c: BIAS correction
        CALL ezget('DO_BIAS_CORRECTION',do_bias_correction, ier )
        IF (do_bias_correction) THEN
          IF (ier.EQ.0)
     &      CALL ezget('UNBIAS_ALL_JETS',unbias_all_jets, ier )
          IF (ier.EQ.0)
     &      CALL ezget('NJETS_TO_UNBIAS',njets_to_unbias, ier )
        ENDIF
C: ICR/HV/CRYOSTAT correction
        IF (ier.EQ.0)
     &    CALL ezget('DO_DET_SCALE_CORRECTION',
     &    do_det_scale_correction,ier )
C: WIDTH correction
C        IF (ier.EQ.0)
c     &    CALL ezget('DO_WIDTH_CORRECTION',do_width_correction, ier )
c: Energy-dependent RESPONSE correction
        IF (ier.EQ.0)
     &    CALL ezget('DO_RESPONSE_CORRECTION',
     &    do_response_correction,ier)
C: What TYPE of RCP?
        IF (ier.EQ.0) CALL ezget('MC_RCP',mc_rcp, ier )
        CALL ezrset
        IF ( ier .NE. 0 ) THEN
          CALL errmsg('RCP error','QCD_JET_CORRECTION',
     &      'Read error:abort ','F')
          ier = -3
          GOTO 999
        ENDIF
      ENDIF
C
C ****  initialize variables
C
      cfactor = 1.0
      low_et_bias = 1.0
      det_scale = 1.0
      rel_correction_width = 1.0
      response = 1.0
      out_of_cone = 1.0
      under_err = 0.
      zsp_err = 0.
      under_e = 0.
      under_et = 0.
      zsp_e = 0.
      zsp_et = 0.
      zsp_e_err = 0.
      under_e_err = 0.
      zsp_frac = 0.
      und_frac = 0.
      phy_bck_error = 0.
      sys_bias_error_low = 0.
      sys_bias_error_high = 0.
      CALL vzero(new_e_fract,3)
      CALL vzero(zsp_frac_err,2*2)
      CALL vzero(und_frac_err,2*2)
      CALL vzero(low_et_bias_err,2*2)
      CALL vzero(det_scale_err,2*2)
      CALL vzero(response_err,2*2)
      CALL vzero(ooc_err,2*2)
      CALL vzero(cfactor_error,2*2)
C
C: What type of data is this? Check if correct.
C
      this_is_monte_carlo = monte_carlo_data()
      IF ((this_is_monte_carlo.AND.(mc_rcp.EQ.0)).OR.
     &      (.NOT.this_is_monte_carlo.AND.(mc_rcp.EQ.1))) THEN
        CALL errmsg('Wrong RCP','QCD_JET_CORRECTION',
     &        'You probably have the wrong RCP for these data','W')
      ENDIF
C
C: Initialize input/output variables
C
      CALL vzero(new_jet_par,njetpar)
      ier       =  0                          ! No Error
C
C: Get information about the jet they want corrected
C
      IF ( ljets .LE. 0 ) THEN
C
C: Assume they used entry point QCD_JET_CORRECTION_2/3 to fill these
C: variables
C
        jet_et_threshold = 8.0                          ! Default CONE
        IF ( cone_used .LE. 0. ) jet_et_threshold = 5.  ! Default NN
      ELSE
        lcaph = lq( ljets + 1 )         ! Pointer to CAPH algorithm bank
        ialgo            = iq( lcaph + 4 )
        cone_used        =  q( lcaph + 6 )
        IF (ialgo.EQ.2) cone_used = abs(cone_used)  ! d0fix D0_algorithm
        jet_et_threshold =  q( lcaph + 7 )                     ! CONE,KT
        IF ( ialgo .EQ. 3 ) jet_et_threshold = q( lcaph + 13 ) ! NN
C
C: Fill old variables
C
        old_jet_e      = q( ljets + 5  )
        old_jet_et     = q( ljets + 6  )
        old_jet_phi    = q( ljets + 8  )
        old_jet_eta    = q( ljets + 9  )
        old_jet_size = sqrt( q( ljets + 12 )**2 + q( ljets + 13 )**2)
        old_e_fract(1)   = q( ljets + 14 )
        old_e_fract(2)   = q( ljets + 17 )
        old_e_fract(3)   = q(ljets + 18)
        set_user_cone = .false.
      ENDIF
C
C: Determine which conesize they want
C
      icone = 3                             ! R=.7
      IF (cone_used.LE.0.0) THEN            ! NN
        icone = 4
      ELSEIF (cone_used.LE.0.4) THEN        ! R = 0.3
        icone = 1
      ELSEIF (cone_used.LE.0.6) THEN        ! R = 0.5
        icone = 2
      ELSEIF (cone_used.GE.0.9) THEN        ! R = 1.0
        icone = 5
      ENDIF

C: give option for the user to set cone sizes
      IF (set_user_cone) THEN
        icone = user_icone
        cone_used = user_cone
        set_user_cone = .false.
      ENDIF
C
C: Check vertex and compute detector eta
C
      z_vertex_used = z_vertex
      IF ( abs( z_vertex_used ) .GT. 150. ) z_vertex_used = 0.0
      det_eta = peta_to_deta( old_jet_eta, z_vertex_used )
C
C: Require that ET is above threshold. If too small, dont correct
C
      below_et_threshold = ( old_jet_et .LT. jet_et_threshold )
C
C: Start ---------------------------------------------------------
C
      istat = 0
      new_jet_e = old_jet_e
      new_jet_et= old_jet_et
C
C ****  calculate low et bias correction
C
      CALL correct_jets_em_removed( em_removed )  ! Electron removed?
C
      IF ( (do_bias_correction) .AND.
     &     (.NOT. em_removed) .AND. (.NOT. below_et_threshold) .AND.
     &     (.NOT. keep_biased) )  THEN     ! we might want to unbias this jet
C
        IF ( unbias_all_jets .OR. ijet .LE. njets_to_unbias )  THEN
C
          CALL mpf_low_et_bias_corr(old_jet_et,det_eta,icone,
     &      low_et_bias,low_et_bias_err,istat)
          IF (istat.NE.0) THEN
            CALL errmsg('MPF-BIAS ERROR','QCD_JET_CORRECTION',
     &        'problem with low Et bias corr','W')
            low_et_bias = 1.00
            CALL vzero(low_et_bias_err,2*2)
          ENDIF
C
          new_jet_e = new_jet_e/low_et_bias
          new_jet_et = new_jet_et/low_et_bias
          cfactor = 1.0/low_et_bias
C-        *** statistical errors ***
          cfactor_error(1,1) = low_et_bias_err(1,1)
          cfactor_error(2,1) = low_et_bias_err(2,1)
C-        *** systematic errors ***
          cfactor_error(1,2) = low_et_bias_err(1,2)
          cfactor_error(2,2) = low_et_bias_err(2,2)
        ENDIF
      ENDIF
      new_jet_eta  = old_jet_eta
      new_jet_phi  = old_jet_phi
C
C: Calculate underlying event and noise+additional MB event contributions.
C: Always do this for jets with removed electrons.
C
      CALL correct_jets_noiseu( ljets, det_eta, cone_used, under_e,
     &    under_et, zsp_e, zsp_et)
      CALL jet_undzsp_error(under_e_err,under_err,zsp_e_err,zsp_err,
     &  zsp_e_err_sys,zsp_err_sys)
C
C ****  always do offset subtraction - add back later if necessary
C
      zsp_frac = (new_jet_e  - zsp_e)/new_jet_e
      new_jet_e = max( new_jet_e - zsp_e , small )
      new_jet_et= max( new_jet_et - zsp_et , small)
C-        *** stat errors ***
      zsp_frac_err(1,1) = zsp_e_err/new_jet_e   ! low error
      zsp_frac_err(2,1) = zsp_frac_err(1,1)     ! high error
      cfactor_error(1,1) = qad(cfactor_error(1,1),zsp_frac_err(1,1))
      cfactor_error(2,1) = qad(cfactor_error(2,1),zsp_frac_err(2,1))
C-        *** systematic errors ***
      zsp_frac_err(1,2) = zsp_e_err_sys/new_jet_e   ! low error
      zsp_frac_err(2,2) = zsp_frac_err(1,2)         ! high error
      cfactor_error(1,2) = qad(cfactor_error(1,2),zsp_frac_err(1,2))
      cfactor_error(2,2) = qad(cfactor_error(2,2),zsp_frac_err(2,2))
      und_frac = (new_jet_e  - under_e)/new_jet_e
      new_jet_e = max( new_jet_e  - under_e , small )
      new_jet_et= max( new_jet_et - under_et , small )
C-        *** stat errors ***
      und_frac_err(1,1) = under_e_err/new_jet_e   ! low error
      und_frac_err(2,1) = und_frac_err(1,1)       ! high error
      cfactor_error(1,1) = qad(cfactor_error(1,1),und_frac_err(1,1))
      cfactor_error(2,1) = qad(cfactor_error(2,1),und_frac_err(2,1))
      below_et_threshold = ( new_jet_et .LT. jet_et_threshold )
C
C: Correct for ICD/MG samp weights, detector scales, and eta dependence
C
      istat = 0
      IF (do_det_scale_correction) THEN
        IF ((new_jet_et.LT.min_jet_et(icone)).OR.
     &        (new_jet_e.LT.min_jet_et(icone))) THEN
          istat = -1
          CALL ucopy(old_e_fract,new_e_fract,3)
        ELSEIF (.NOT.below_et_threshold) THEN
          CALL correct_jets_scale_factors(new_jet_et,det_eta,
     &      old_e_fract,
     &      cone_used,new_e_fract,det_scale,det_scale_err,istat)
        ELSEIF (below_et_threshold.AND.new_jet_et.GE.min_jet_et(icone))
     &      THEN
          CALL correct_jets_scale_factors(jet_et_threshold,det_eta,
     &      old_e_fract,
     &      cone_used,new_e_fract,det_scale,det_scale_err,istat)
          IF (istat.EQ.0) THEN
            delta = (det_scale-1.0)*(new_jet_et-min_jet_et(icone))
     &          /(jet_et_threshold-min_jet_et(icone))
            det_scale = 1.0 + delta
          ENDIF
        ENDIF
        IF (istat.NE.0) THEN
          IF (istat.NE.-1) call errmsg('ICD/CRYO ERROR',
     &      'QCD_JET_CORRECTION',
     &      'error in ICD/CRYOSTAT scale corr','W' )
          det_scale = 1.0
          CALL vzero(det_scale_err,2*2)
        ENDIF
        cfactor = cfactor*det_scale
C-        *** statistical errors ***
        cfactor_error(1,1) = qad(cfactor_error(1,1),det_scale_err(1,1))
        cfactor_error(2,1) = qad(cfactor_error(2,1),det_scale_err(2,1))
C-        *** systematic errors ***
        cfactor_error(1,2) = qad(cfactor_error(1,2),det_scale_err(1,2))
        cfactor_error(2,2) = qad(cfactor_error(2,2),det_scale_err(2,2))
        new_jet_e = new_jet_e*det_scale
        new_jet_et = new_jet_et*det_scale
      ELSE
        CALL ucopy(old_e_fract,new_e_fract,3)
      ENDIF
      below_et_threshold = ( new_jet_et .LT. jet_et_threshold )
C
C: Correct jet relative to an 'average' jet, based on its rms
C: width
C
      new_jet_size = old_jet_size
C      IF (do_width_correction) THEN
C        IF ((new_jet_et.LT.min_jet_et(icone)).OR.
C     &        (new_jet_e.LT.min_jet_et(icone))) THEN
C          rel_correction_width = 1.0
C        ELSEIF (.NOT.below_et_threshold) THEN
C          CALL correct_jets_width(cone_used,new_jet_et,
C     &        det_eta, new_jet_size,new_e_fract(1),rel_correction_width)
C        ELSEIF (below_et_threshold.AND.new_jet_et.GE.min_jet_et(icone))
C     &      THEN
C          CALL correct_jets_width(cone_used,jet_et_threshold,
C     &        det_eta, new_jet_size,new_e_fract(1),rel_correction_width)
C          delta = (rel_correction_width-1.0)
C     &      *(new_jet_et-min_jet_et(icone))
C     &      /(jet_et_threshold-min_jet_et(icone))
C          rel_correction_width = 1.0 + delta
C        ENDIF
C        cfactor = cfactor*rel_correction_width
C        new_jet_et  = new_jet_et * rel_correction_width
C        new_jet_e   = new_jet_e  * rel_correction_width
C      ENDIF
      below_et_threshold = ( new_jet_et .LT. jet_et_threshold )
C
C: Absolute correction from MPF
C
      istat = 0
      IF (do_response_correction) THEN
        IF ((new_jet_et.LT.min_jet_et(icone)).OR.
     &          (new_jet_e.LT.min_jet_et(icone))) THEN
          istat = -1
        ELSEIF (.NOT.below_et_threshold) THEN
          CALL mpf_jetcorr(new_jet_e,det_eta,icone,response,
     &        response_err,istat)
        ELSEIF (below_et_threshold.AND.new_jet_et.GT.min_jet_et(icone))
     &      THEN
          CALL mpf_jetcorr(new_jet_e,det_eta,icone,response,
     &        response_err,istat)
          IF (istat.EQ.0) THEN
            delta = (response-1.0)*(new_jet_et-min_jet_et(icone))
     &          /(jet_et_threshold-min_jet_et(icone))
            response = 1.0 + delta
          ENDIF
        ENDIF
        IF (istat.NE.0) THEN
          IF (istat.NE.-1) call errmsg('MPF ERROR','QCD_JET_CORRECTION',
     &      'cant do response correction','W')
          response = 1.0
          CALL vzero(response_err,2*2)
          phy_bck_error = 0.0
          sys_bias_error_low = 0.0
          sys_bias_error_high = 0.0
        ELSE
          cfactor = cfactor/response
C-        *** statistical errors ***
          cfactor_error(1,1) = qad(cfactor_error(1,1),response_err(1,1))
          cfactor_error(2,1) = qad(cfactor_error(2,1),response_err(2,1))
C-        *** systematic errors ***
          cfactor_error(1,2) = qad(cfactor_error(1,2),response_err(1,2))
          cfactor_error(2,2) = qad(cfactor_error(2,2),response_err(2,2))
C
          new_jet_e = new_jet_e/response
          new_jet_et = new_jet_et/response
        ENDIF
      ENDIF
C
      below_et_threshold = ( new_jet_et .LT. jet_et_threshold )
C
C: Finally, algorithmic correction.
C
      istat = 0
      IF ( do_algo_losses_corr ) THEN
        IF ((new_jet_et.LT.min_jet_et(icone)).OR.
     &        (new_jet_e.LT.min_jet_et(icone))) THEN
          istat = -1
        ELSEIF (.NOT.below_et_threshold) THEN
          CALL correct_jets_ooc( new_jet_e, new_jet_et, new_jet_eta,
     &        cone_used, out_of_cone, istat)
        ELSEIF (below_et_threshold.AND.new_jet_et.GT.min_jet_et(icone))
     &      THEN
          CALL correct_jets_ooc( new_jet_e, new_jet_et, new_jet_eta,
     &        cone_used, out_of_cone, istat)
          IF (istat.EQ.0) THEN
            delta = (out_of_cone-1.0)*(new_jet_et-min_jet_et(icone))
     &          /(jet_et_threshold-min_jet_et(icone))
            out_of_cone = 1.0 + delta
          ENDIF
        ENDIF
        CALL algo_loss_errors(ooc_err)
        IF (istat.NE.0) THEN
          IF (istat.NE.-1) call errmsg('LOSS ERROR',
     &      'QCD_JET_CORRECTION',
     &      'cant do loss correction','W')
          CALL vzero(ooc_err,2*2)
          out_of_cone = 1.0
        ENDIF
        cfactor = cfactor*out_of_cone
C-        *** statistical errors ***
        cfactor_error(1,1) = qad(cfactor_error(1,1),ooc_err(1,1))
        cfactor_error(2,1) = qad(cfactor_error(2,1),ooc_err(2,1))
C-        *** systematic errors ***
        cfactor_error(1,2) = qad(cfactor_error(1,2),ooc_err(1,2))
        cfactor_error(2,2) = qad(cfactor_error(2,2),ooc_err(2,2))
        new_jet_et  = new_jet_et * out_of_cone
        new_jet_e   = new_jet_e  * out_of_cone
      ENDIF
C
C ****  now calculate residual bias errors, MC topo correction
C
      CALL RESPONSE_SYSTEMATICS(new_jet_e,new_jet_et,det_eta,icone,
     &  phy_bck_error,sys_bias_error_low,sys_bias_error_high,
     &  kt_err,topo_corr)
      cfactor_error(1,2) = sqrt(cfactor_error(1,2)**2 +
     &   phy_bck_error**2 + sys_bias_error_low**2 + kt_err**2)
      cfactor_error(2,2) = sqrt(cfactor_error(2,2)**2 +
     &   phy_bck_error**2 + sys_bias_error_high**2 + kt_err**2)
      IF (MC_RCP.EQ.1) THEN
        new_jet_et = new_jet_et * topo_corr
        new_jet_e = new_jet_e * topo_corr
      ENDIF

C
C ****  add back noise or ULE if correction turned off
C
      IF (.NOT.do_zsp_corr) THEN
        new_jet_e = new_jet_e + zsp_e*det_scale/response*out_of_cone
        new_jet_et = new_jet_et + zsp_et*det_scale/response*out_of_cone
        cfactor_error(1,1) = qsb(cfactor_error(1,1),zsp_frac_err(1,1))
        cfactor_error(2,1) = qsb(cfactor_error(2,1),zsp_frac_err(2,1))
        cfactor_error(1,2) = qsb(cfactor_error(1,2),zsp_frac_err(1,2))
        cfactor_error(2,2) = qsb(cfactor_error(2,2),zsp_frac_err(2,2))
      ENDIF
      IF (.NOT.do_undevt_corr) THEN
        new_jet_e = new_jet_e + under_e*det_scale/response*out_of_cone
        new_jet_et = new_jet_et +
     &                         under_et*det_scale/response*out_of_cone
        cfactor_error(1,1) = qsb(cfactor_error(1,1),und_frac_err(1,1))
        cfactor_error(2,1) = qsb(cfactor_error(2,1),und_frac_err(2,1))
      ENDIF
C
C: Return
C
      error = 1.0
      IF (isys.EQ.1) THEN
        error = 1.0 - sqrt(cfactor_error(1,1)**2
     &      +cfactor_error(1,2)**2)
      ELSEIF (isys.EQ.2) THEN
        error = 1.0 + sqrt(cfactor_error(2,1)**2
     &      +cfactor_error(2,2)**2)
      ENDIF
      out_jet_e = new_jet_e*error
      out_jet_et= new_jet_et*error
      out_jet_eta= new_jet_eta
  999 RETURN

      ENTRY qcd_jet_correction_2( olde, oldet, oldeta, oldphi, oldemf,
     &    oldsiz,oldcone, oldicdf, oldchf )
      old_jet_e = olde
      old_jet_et= oldet
      old_jet_eta= oldeta
      old_jet_phi= oldphi
      old_e_fract(1)= oldemf
      old_jet_size= oldsiz
      old_e_fract(2)= oldicdf
      old_e_fract(3)= oldchf
      cone_used = oldcone
      RETURN

      ENTRY qcd_jet_correction_3( olde, oldet, oldeta, oldemf,
     &    oldsiz,oldcone, oldicone, oldicdf, oldchf )
      set_user_cone = .true.
      old_jet_e = olde
      old_jet_et= oldet
      old_jet_eta= oldeta
      old_e_fract(1)= oldemf
      old_jet_size= oldsiz
      old_e_fract(2)= oldicdf
      old_e_fract(3)= oldchf
      user_cone = oldcone
      user_icone = oldicone
      RETURN
C
      ENTRY qcd_jet_correction_quans(jet_quans)
C------------------------------------------------------------------------
C-    Purpose:  return values of energy fractions and jet scale errors
C-
C-      ** Note: errors are described by their effect on corrected jet energy
C-      ** error(1,1) = low statistical/uncorrelated error
C-      ** error(2,1) = high statistical/uncorrelated error
C-      ** error(1,2) = low systematic/correlated error
C-      ** error(2,2) = high systematic/correlated error
C------------------------------------------------------------------------
      CALL vzero(jet_quans,50)
      jet_quans(1) = new_e_fract(1)
      jet_quans(2) = new_e_fract(2)
      jet_quans(3) = new_e_fract(3)
      jet_quans(5) = maxabs(new_jet_et)/maxabs(old_jet_et)
C-        *** statistical errors ***
      jet_quans(6) = cfactor_error(1,1)
      jet_quans(7) = cfactor_error(2,1)
C-        *** systematic errors ***
      jet_quans(8) = cfactor_error(1,2)
      jet_quans(9) = cfactor_error(2,2)
      jet_quans(10) = low_et_bias
      jet_quans(11) = low_et_bias_err(1,1)
      jet_quans(12) = low_et_bias_err(2,1)
      jet_quans(13) = low_et_bias_err(1,2)
      jet_quans(14) = low_et_bias_err(2,2)
      jet_quans(15) = det_scale
      jet_quans(16) = det_scale_err(1,1)
      jet_quans(17) = det_scale_err(2,1)
      jet_quans(18) = det_scale_err(1,2)
      jet_quans(19) = det_scale_err(2,2)
      jet_quans(20) = rel_correction_width
      jet_quans(25) = response
      jet_quans(26) = response_err(1,1)
      jet_quans(27) = response_err(2,1)
      jet_quans(28) = response_err(1,2)
      jet_quans(29) = response_err(2,2)
      jet_quans(30) = out_of_cone
      jet_quans(31) = ooc_err(1,1)
      jet_quans(32) = ooc_err(2,1)
      jet_quans(33) = ooc_err(1,2)
      jet_quans(34) = ooc_err(2,2)
      jet_quans(35) = zsp_frac
      jet_quans(36) = zsp_frac_err(1,1)
      jet_quans(37) = zsp_frac_err(2,1)
      jet_quans(38) = zsp_frac_err(1,2)
      jet_quans(39) = zsp_frac_err(2,2)
      jet_quans(40) = und_frac
      jet_quans(41) = und_frac_err(1,1)
      jet_quans(42) = und_frac_err(2,1)
      jet_quans(43) = und_frac_err(1,2)
      jet_quans(44) = und_frac_err(2,2)
C
C ****  catalog response errors from method
C
      jet_quans(45) = phy_bck_error
      jet_quans(46) = sys_bias_error_low
      jet_quans(47) = sys_bias_error_high
      jet_quans(48) = kt_err
      jet_quans(49) = topo_corr  ! non-zero for MC data only
      RETURN

C************************************************************
C ENTRY QCD_JET_CORRECTION_GET_UNDZSP : Return noise numbers
C************************************************************
      ENTRY qcd_jet_correction_get_undzsp( noise )
      noise(1) = under_e
      noise(2) = under_e_err
      noise(3) = under_et
      noise(4) = under_err
      noise(5) = zsp_e
      noise(6) = sqrt(zsp_e_err**2 + zsp_e_err_sys**2)
      noise(7) = zsp_et
      noise(8) = sqrt(zsp_err**2 + zsp_err_sys**2)
      RETURN
      END
