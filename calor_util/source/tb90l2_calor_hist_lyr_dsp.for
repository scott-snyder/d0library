      SUBROUTINE TB90L2_CALOR_HIST_LYR_DSP(module,layer,energy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Processing for Layer Display Histograms
C-
C-   Inputs  :
C-      module  -  module number
C-      layer   -  layer of module
C-      energy  -  energy left in channel
C-   Outputs :
C-   Controls:
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$params:tb90l2_modules.def'
      INTEGER module,layer
      REAL    energy
      INTEGER i
C
C ****  set up arrays for eta,phi cuts. The indicies are :
C ****          1 -  set of modules
C ****          2 -  sublayer of module
C
      REAL    mod_energy(LAST_LYRD_MOD,MAX_GRP_LAYER)
C----------------------------------------------------------------------
C
C ****  sum energy in calorimeter modules
C
      IF ( module .LE. LAST_LYRD_MOD ) THEN     ! only do layered mods
        mod_energy(module,layer) = mod_energy(module,layer) + energy
      ENDIF
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_lyr_dsp_fill()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the historgrams
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      DO i = 1,TB90L2EM_LYRS          ! fill EM layers
        CALL hf1(4000+i,mod_energy(TB90L2EM,i),1.)
      END DO
      DO i = 1, TB90L2FH_LYRS
        CALL hf1(4004+i,mod_energy(TB90L2FH,i),1.)
      END DO
      DO i = 1,TB90L2CH_LYRS
        CALL hf1(4007+i,mod_energy(TB90L2CH,i),1.)
      END DO
      DO i = 1,TB90L2MH_LYRS
        CALL hf1(4008+i,mod_energy(TB90L2MH,i),1.)
      END DO
      DO i = 1,TB90L2OH_LYRS
        CALL hf1(4013+i,mod_energy(TB90L2OH,i),1.)
      END DO
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_lyr_dsp_init()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set counters to 0
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL vzero(mod_energy(1,1),LAST_LYRD_MOD*MAX_GRP_LAYER)
      RETURN
      END
