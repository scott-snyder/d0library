      SUBROUTINE tb90l2_calor_hist_lego(module,layer,eta,phi,energy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Processes events for lego plots. eta,phi,energy for
C-   each layer of each module. The histos are filled here on a channel by
C-   channel basis instead of event by event because of the 2-d histos are
C-   filled. It is easier to fill them as you go rather than accumuating info
C-   and storing them at the end of the event.
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
      INTEGER module, layer
      REAL    eta, phi, energy
      INCLUDE 'd0$params:tb90l2_modules.def'
c----------------------------------------------------------------------
      IF ( module .EQ. TB90L2EM ) THEN
        CALL hf2(9000+layer,eta,phi,energy)
      ELSEIF ( module .EQ. TB90L2FH ) THEN
        CALL hf2(9004+layer,eta,phi,energy)
      ELSEIF ( module .EQ. TB90L2CH ) THEN
        CALL hf2(9008,eta,phi,energy)
      ELSEIF ( module .EQ. TB90L2MH ) THEN
        CALL hf2(9008+layer,eta,phi,energy)
      ELSEIF ( module .EQ. TB90L2OH ) THEN
        CALL hf2(9013+layer,eta,phi,energy)
      ELSEIF ( module .EQ. TB90L2ICD ) THEN
        CALL hf2(9017,eta,phi,energy)
      ELSEIF ( module .EQ. TB90L2CCMG ) THEN
        CALL hf2(9018,eta,phi,energy)
      ELSEIF ( module .EQ. TB90L2ECMG ) THEN
        CALL hf2(9019,eta,phi,energy)
      ELSEIF ( module .EQ. TB90L2ICDMG ) THEN
        CALL hf2(9020,eta,phi,energy)
      ENDIF
      RETURN
      END
