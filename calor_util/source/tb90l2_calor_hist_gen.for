      SUBROUTINE TB90L2_CALOR_HIST_GEN
     &             (module,slayer,layer,eta,phi,energy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do processing for MAIN WATCH histograms
C-
C-   Inputs  :
C-      module  -  calorimeter module
C-      slayer  -  software layer of calorimeter
C-      layer   -  physical layer of module
C-      eta,phi -  eta,phi address of module
C-      energy  -  energy dumped into module
C-   Outputs :
C-   Controls:
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$params:tb90l2_modules.def'
      INTEGER module,slayer,layer
      REAL    eta,phi
      REAL    energy
      INTEGER number_of_channels
C
C ****  set up arrays for data. The first index of arrays are module number as
C ****  definded in tb90l2_modules.def, the second index is the layer number of
C ****  the module( all modules have layer 1). If the first index = 0, the
C ****  number refers to the sum of all modules
C
      INTEGER mod_channels(0:NUM_OF_MODULES) ! number of channels
      REAL    mod_energy(0:NUM_OF_MODULES)      ! energy in each mod.
      REAL    energy_layer(1:LAST_LYRD_MOD,1:MAX_LYR_NUM) ! energy/layer
C
C ****  max_energy_layer contains the max energy for each layer of each module.
C ****  The indices are (module,layer,item) item= 1 - energy, 2-eta, 3-phi
C
      REAL    max_energy_layer(1:LAST_LYRD_MOD,1:MAX_LYR_NUM,1:3)
C
C ****  absolute maximum energy  1 - module
C ****                           2 - slayer
C ****                           3 - energy
C ****                           4 - eta
C ****                           5 - phi
C
      REAL    maximum_energy(5)
      INTEGER i
      SAVE mod_channels,mod_energy,energy_layer,max_energy_layer
C----------------------------------------------------------------------
      mod_channels(module) = mod_channels(module) + 1      ! add
      mod_channels(0) = mod_channels(0) + 1     ! channels and total chn.
      mod_energy(module) = mod_energy(module) + energy  ! add energy
      mod_energy(0) = mod_energy(0) + energy    ! and total energy
      IF ( module .LE. LAST_LYRD_MOD ) THEN
        energy_layer(module,layer) = energy_layer(module,layer) +
     &      energy
        IF ( energy .GT. max_energy_layer(module,layer,1) ) THEN
          max_energy_layer(module,layer,1) = energy
          max_energy_layer(module,layer,2) = eta
          max_energy_layer(module,layer,3) = phi
        ENDIF
        IF ( energy .GT. maximum_energy(3) ) THEN
          maximum_energy(1) = float(module)
          maximum_energy(2) = float(slayer)
          maximum_energy(3) = energy
          maximum_energy(4) = eta
          maximum_energy(5) = phi
        ENDIF
      ENDIF
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_gen_fill
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills histograms
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
C
C ****  fill the histograms in the module order listed in tb90l2_modules.def
C ****  For this to work the histograms must be booked in the same order.
C ****  That is the ordering of hist in tb9l2_calor_hist_rcp must be the same
C ****  as in tb90l2_modules.def
C
      CALL hf1(1001,mod_energy(0),1.)
      CALL hf1(1101,float(mod_channels(0)),1.)
      DO i = 1 , NUM_OF_MODULES
        CALL hf1(2000+i,mod_energy(i),1.)
        CALL hf1(2100+i,float(mod_channels(i)),1.)
      ENDDO
      CALL hf1(3001,maximum_energy(2),1.)
      CALL hf1(3002,maximum_energy(1),1.)
      CALL hf1(3003,maximum_energy(1)*100.+maximum_energy(2),1.)
      CALL hf1(3004,maximum_energy(3),1.)
      CALL hf1(3005,maximum_energy(4),1.)
      CALL hf1(3006,maximum_energy(5),1.)
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_gen_chan(number_of_channels)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fills the number of channels returned from
C-   gtcaep_header. Have entry here to logic simple. All histos that are part
C-   of the main group are filled here.
C-
C-   Inputs  : number of channels from gtcaep_header
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL hf1(1102,float(number_of_channels),1.)
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_gen_init()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : resets counter to 0.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL vzero_i(mod_channels(0),NUM_OF_MODULES+1)      ! +1 get (0)
      CALL vzero(mod_energy(0),NUM_OF_MODULES+1)        ! energy counts
      CALL vzero(energy_layer(1,1),NUM_OF_MODULES*LAST_LYRD_MOD)!lyrs
      CALL vzero(max_energy_layer(1,1,1),LAST_LYRD_MOD*MAX_LYR_NUM*3)
      CALL vzero(maximum_energy(1),4)
      RETURN
      END
