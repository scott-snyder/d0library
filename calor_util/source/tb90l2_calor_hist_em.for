      SUBROUTINE TB90L2_CALOR_HIST_EM(module,layer,eta,phi,energy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process data for filling of EM_HISTOGRAMS
C-
C-   Inputs  :
C-      module  -  calorim. module
C-      layer   -  layer of module
C-      eta,phi -  address of cell
C-      energy  -  energy in cell
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-JUN-1991   James Richardson
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
      INTEGER EM_FH1_LAYER              ! layer in EM_FH1 module
      INTEGER i
C
C ****  set up arrays for data
C ****  mod_channels, mod_energy holds the # of channels and energy in each
C ****  layer. Index 1 is 1 for narrow cut, 2 for wide.  Index 2 ranges
C ****  0..5 for EM layers 1-4 and FH layer 1, 0 = sum
C
      INTEGER mod_channels(2,0:5)
      REAL    mod_energy(2,0:5)
C
C ****  max_energy_layer contains the max energy for each layer of each module.
C ****  The indices are (layer,item) item=1 - energy, 2-eta, 3-phi
C
      REAL    max_energy_layer(1:5,1:3)
      SAVE mod_channels,mod_energy,max_energy_layer
C----------------------------------------------------------------------
C
C ****  See if we are in EM or FH layer 1
C
      EM_FH1_LAYER = tb90l2_regroup_modules(module,layer,EM_FH1)
      IF ( EM_FH1_LAYER .NE. 0 ) THEN
        DO cut = 1 , 2
          IF ( tb90l2_did_make_cuts(cut,module,eta,phi) ) THEN
            mod_channels(cut,EM_FH1_LAYER) =
     &        mod_channels(cut,EM_FH1_LAYER) + 1
            mod_channels(cut,0) = mod_channels(cut,0) + 1
            mod_energy(cut,EM_FH1_LAYER) =
     &        mod_energy(cut,EM_FH1_LAYER) + energy
            mod_energy(cut,0) = mod_energy(cut,0) + energy
          ENDIF
        ENDDO
            IF ( energy .GT. max_energy_layer(layer,1) ) THEN
              max_energy_layer(layer,1) = energy
              max_energy_layer(layer,2) = eta
              max_energy_layer(layer,3) = phi
            ENDIF
      ENDIF
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_em_fill
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the EM_WATCH histograms
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      DO i = 0 , 5                      ! loop over all layers
        CALL hf1(5001+i,mod_energy(1,i),1.)     ! energ nar cuts
        CALL hf1(5007+i,mod_energy(2,i),1.)     ! energ wid cuts
        CALL hf1(5013+i,float(mod_channels(1,i)),1.)     ! chans nar cuts
        CALL hf1(5019+i,float(mod_channels(2,i)),1.)     ! chans wid cuts
      ENDDO
      DO i = 1 , 5
        CALL hf1(5025+(i-1)*3,max_energy_layer(i,1),1.)
        CALL hf1(5026+(i-1)*3,max_energy_layer(i,2),1.)
        CALL hf1(5027+(i-1)*3,max_energy_layer(i,3),1.)
      ENDDO
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_em_init
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Resets counters
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL vzero(mod_channels(1,0),2*6)
      CALL vzero(mod_energy(1,0),2*6)
      CALL vzero(max_energy_layer(1,1),5*3)
      RETURN
      END
