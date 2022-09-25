      SUBROUTINE TB90L2_CALOR_HIST_LRG_ETA(module,layer,eta,phi,energy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process data for filling of SMALL_ETA_HISTOGRAMS
C-   Otherwise know as EM+FH+CH+MH+OH modules
C-
C-   Inputs  :
C-      module  -  calorim. module
C-      layer   -  layer of module
C-      eta,phi -  address of cell
C-      energy  -  energy in cell
C-   Outputs : none
C-   Controls: none
C-
C-   Created  25-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_did_make_cuts
      INTEGER tb90l2_regroup_modules
      INTEGER module,layer
      REAL    eta,phi
      REAL    energy
      INCLUDE 'd0$params:tb90l2_modules.def'
      INTEGER cut                       ! narrow or wide
      INTEGER em_fh_ch_mh_oh_layer      ! layer in EM_FH_CH_MH_OH module
      INTEGER i
C
C ****  set up arrays for data
C ****  mod_channels, mod_energy holds the # of channels and energy in each
C ****  layer. Index 1 is 1 for narrow cut, 2 for wide.  Index 2 ranges
C ****  0..5 for EM_FH_CH_MH_OH layers, 1 for EM 2 FH layers, 3 for CH,
C ****  4 for OH, 5 for MH, and 0 = sum of all layers
C
      INTEGER mod_channels(2,0:5)
      REAL    mod_energy(2,0:5)
      SAVE mod_channels,mod_energy
C----------------------------------------------------------------------
C
C ****  See if we are in EM, FH, CH, OH, or MH
C
      em_fh_ch_mh_oh_layer =
     &  tb90l2_regroup_modules(module,layer,EM_FH_CH_MH_OH)
      IF ( em_fh_ch_mh_oh_layer .NE. 0 ) THEN
        DO cut = 1 , 2
          IF ( tb90l2_did_make_cuts(cut,module,eta,phi) ) THEN
            mod_channels(cut,em_fh_ch_mh_oh_layer) =
     &        mod_channels(cut,em_fh_ch_mh_oh_layer) + 1
            mod_channels(cut,0) = mod_channels(cut,0) + 1
            mod_energy(cut,em_fh_ch_mh_oh_layer) =
     &        mod_energy(cut,EM_FH_CH_MH_OH_LAYER) + energy
            mod_energy(cut,0) = mod_energy(cut,0) + energy
          ENDIF
        ENDDO
      ENDIF
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_lrg_ETA_fill
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the HAD_LRG_ETA histograms
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  25-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      DO i = 0 , 5                      ! loop over all layers
        CALL hf1(7001+i,mod_energy(1,i),1.)     ! energ nar cuts
        CALL hf1(7007+i,mod_energy(2,i),1.)     ! energ wid cuts
        CALL hf1(7013+i,float(mod_channels(1,i)),1.)     ! chans nar cuts
        CALL hf1(7019+i,float(mod_channels(2,i)),1.)     ! chans wid cuts
      ENDDO
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_lrg_eta_init
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Resets counters
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  25-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL vzero_i(mod_channels(1,0),2*6)
      CALL vzero(mod_energy(1,0),2*6)
      RETURN
      END
