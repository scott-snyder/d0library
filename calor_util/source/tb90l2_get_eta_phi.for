      FUNCTION TB90L2_get_eta_phi(rcp_bank,eta_out,phi_out)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads in eta and phi. When string eaters are
C-   working, a descision will need to be made as to which eta,phi to keep
C-              1. Reads it from string eaters
C-              2. Reads in from binf bank
C-              3. if unsecc or if requested from tb90l2_calor_hist_rcp
C-
C-   Returned value  : true if seccessful, false if read from rcp
C-   Inputs  : none
C-   Outputs : eta_out,phi_out of readout pad
C-   Controls: rcp_bank - which rcp bank to read eta,phi from if requested
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL get_binf_bank
      LOGICAL tb90l2_get_eta_phi
      LOGICAL tb90l2_read_eta_phi_from_string
      LOGICAL tb90l2_read_eta_phi_from_binf
      LOGICAL tb90l2_read_eta_phi_from_rcp
      REAL    eta_out, phi_out
      LOGICAL em_watch, had_smeta, had_lgeta ! histogram banks
      LOGICAL wide_cut, narrow_cut      ! ntuple cuts
      LOGICAL eta_phi_from_rcp          ! get from rcp if true
      CHARACTER*(*) rcp_bank
      REAL    eta(3), phi(3)
      LOGICAL string, binf, rcp
      CHARACTER*80 msg,intmsg_str
      INTEGER ier
C----------------------------------------------------------------------
      TB90L2_get_eta_phi = .TRUE.
      CALL tb90l2_read_eta_phi_reset
C
C ****  read in eta,phi of beam
C
      CALL ezpick(rcp_bank)      ! find out it we want eta,phi
      CALL ezget('ETA_PHI_FROM_RCP',eta_phi_from_rcp,ier) ! read in from
      CALL ezrset                       ! taker or rcp
      IF ( eta_phi_from_rcp ) THEN      ! get from rcp
        rcp = tb90l2_read_eta_phi_from_rcp(rcp_bank,eta(3),phi(3))
        intmsg_str = 'Read values from'//rcp_bank
        CALL intmsg(intmsg_str)
        WRITE(msg,1001) rcp_bank,eta(3),phi(3)
        CALL intmsg(msg)
        eta_out = eta(3)
        phi_out = phi(3)
      ELSE                              ! try it from binf and strings
c        string = tb90l2_read_eta_phi_from_string(eta(1),phi(1))
c        CALL intmsg(' Read values from String Eaters ')
c        WRITE (msg,1000) eta(1),phi(1)
c        CALL intmsg(msg)
c        eta_out = eta(1)
c        phi_out = phi(1)
        binf = tb90l2_read_eta_phi_from_binf(eta(2),phi(2))
        CALL intmsg(' Read values from Binf Bank')
        WRITE (msg,1000) eta(2),phi(2)
        CALL intmsg(msg)
        eta_out = eta(2)
        phi_out = phi(2)
      ENDIF
      IF ( rcp_bank .EQ. 'TB90L2_CALOR_HIST_RCP' ) THEN
C
C ****  get histogram banks that were booked. If any were booked that requires
C ****  eta and phi cuts, init the eta,phi structures in tb90l2_did_make_cuts
C
        CALL ezpick(rcp_bank)
        CALL ezget('EM_WATCH_FLAG',em_watch,ier)
        CALL ezget('HAD_SM_ETA_FLAG',had_smeta,ier)
        CALL ezget('HAD_LG_ETA_FLAG',had_lgeta,ier)
        CALL ezrset
        IF ( em_watch .OR. had_smeta .OR. had_lgeta ) THEN
          CALL tb90l2_did_make_cuts_init(rcp_bank,eta_out,phi_out)
        ENDIF
      ELSEIF ( rcp_bank .EQ. 'TB90L2_NTUPLE_RCP' ) THEN
        CALL ezpick(rcp_bank)
C
C ****  find which ntuple is desired. If cut energies are needed then do the
C ****  eta and phi cuts, init the eta,phi structures in tb90l2_did_make_cuts
C
        CALL ezget('NTUPLE_WIDE_CUT',wide_cut,ier)
        CALL ezget('NTUPLE_NARROW_CUT',narrow_cut,ier)
        CALL ezrset
        IF ( wide_cut .OR. narrow_cut ) THEN
          CALL tb90l2_did_make_cuts_init(rcp_bank,eta_out,phi_out)
        ENDIF
      ENDIF
      RETURN
 1000 FORMAT (' eta = ',f5.2,'   phi = ',f5.2)
 1001 FORMAT (' ',a40, 'eta = ',f5.2,'  phi = ',f5.2)
      END
