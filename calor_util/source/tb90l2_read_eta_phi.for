      FUNCTION TB90L2_READ_ETA_PHI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Hold entry points for reading in eta,phi coords
C-
C-   Returned value  : none
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL get_binf_bank
      EXTERNAL get_binf_bank
      LOGICAL tb90l2_read_eta_phi
      LOGICAL tb90l2_read_eta_phi_from_string
      LOGICAL tb90l2_read_eta_phi_from_binf
      LOGICAL tb90l2_read_eta_phi_from_rcp
      LOGICAL tb90l2_read_eta_phi_reset
      CHARACTER*(*) rcp_bank
      REAL    eta,phi
      INTEGER ier
C----------------------------------------------------------------------
C#######################################################################
      ENTRY tb90l2_read_eta_phi_from_string(eta,phi)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : read eta,phi from string eaters
C-
C-   Inputs  : none
C-   Outputs : output
C-   Controls: none
C-
C-   Created  23-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
c$$$      CALL string_dir(eta,phi)
      tb90l2_read_eta_phi_from_string = .false.
      RETURN
C#######################################################################
      ENTRY tb90l2_read_eta_phi_from_binf(eta,phi)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reads eta,phi from the binf bank
C-
C-   Inputs  : none
C-   Outputs : eta,phi
C-   Controls:
C-
C-   Created  23-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      IF ( get_binf_bank() ) THEN
        CALL ezpick('BINF')              ! try to get it from begn run hdr
        CALL ezget('ETA',eta,ier)
        CALL ezget('PHI',phi,ier)
        tb90l2_read_eta_phi_from_binf = .true.
        CALL EZRSET
      ELSE
        tb90l2_read_eta_phi_from_binf = .false.
      ENDIF
      RETURN
C#######################################################################
      ENTRY tb90l2_read_eta_phi_from_rcp(rcp_bank,eta,phi)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reads eta,phi from rcp_bank
C-
C-   Inputs  : none
C-   Outputs : eta,phi
C-   Controls: rcp_bank - which rcp to read
C-
C-   Created  23-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL ezpick(rcp_bank)
      CALL ezget('ETA_OF_BEAM',eta,ier)
      CALL ezget('PHI_OF_BEAM',phi,ier)
      tb90l2_read_eta_phi_from_rcp = .true.
      CALL EZRSET
      RETURN
C#######################################################################
      ENTRY tb90l2_read_eta_phi_reset
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reset flags to false
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      tb90l2_read_eta_phi_from_string = .false.
      tb90l2_read_eta_phi_from_binf = .false.
      tb90l2_Read_eta_phi_from_rcp = .false.
      RETURN
      END
