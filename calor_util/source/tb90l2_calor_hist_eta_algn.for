      SUBROUTINE TB90L2_CALOR_HIST_ETA_ALGN(module,layer,eta,phi,energy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process info for filling alignment watch histograms.
C-
C-   Inputs  :
C-      module  -  calorimeter module
C-      layer   -  layer of module
C-      eta,phi -  address of module
C-      energy  -  energy in cell
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$params:tb90l2_modules.def'
      INTEGER module,layer
      REAL    eta,phi, energy
C
C ****  max_energy_layer contains the max energy for each layer of each module.
C ****  The indices are (module,item) item= 1 - energy, 2-eta, 3-phi, 4-layer
C
      REAL    max_energy_layer(1:LAST_LYRD_MOD,1:4)
      INTEGER i,j,k
      SAVE max_energy_layer
C----------------------------------------------------------------------
C
C ****  If the energy is greater update address
C
      IF ( module .LE. LAST_LYRD_MOD ) THEN
        IF ( energy .GT. max_energy_layer(module,1) ) THEN
          max_energy_layer(module,1) = energy
          max_energy_layer(module,2) = eta
          max_energy_layer(module,3) = phi
          max_energy_layer(module,4) = float(layer)
        ENDIF
      ENDIF
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_eta_algn_fill
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fills the eta align histograms
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      k = 1                             ! k counts over modules
      DO i = 8000 , 8000 + (LAST_LYRD_MOD-1) * 4 , 4     ! fill for each mod
        DO j = 1 , 4                      ! 4 histo for each mod
          CALL hf1(i+j,max_energy_layer(k,j),1.)
        ENDDO
        k = k + 1
      ENDDO
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_eta_algn_init
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : init histo counters for each event
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL vzero(max_energy_layer(1,1),LAST_LYRD_MOD*4)
      RETURN
      END
