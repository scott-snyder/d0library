      SUBROUTINE CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALCULATES ENERGY DEPOSITION BY DEPTH
C-                         AND RING FOR CASH BANKS.
C-
C-   Inputs  : LCASH Link to CASH bank of interest
C-   Outputs : IETA_HOT I(5) Calorimeter eta for hottest tower
C-             IPHI_HOT I(5) Calorimeter phi for hottest tower
C-             EDPTH    R(5) Energy in EM layers + FH1
C-             PDPTH    R(5) Fractional energy in 5 layers
C-           ENERGY_HOT R(5) Energy in hottest cell of each layer
C-
C-   Controls: none
C-
C-   Created  12-JUN-1992   Norman A. Graf
C-   Updated  10-JUN-1995   Norman A. Graf  added LAYER_HOT as output 
C-                          argument (for CM3POS use)
C-   Updated   5-AUG-1995   Meenakshi Narain Add CEMDPTH_LAYER entry point
C-                          instead of changing argument list, as this routine
C-                          widely used in various analysis routines.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCASH
      INTEGER I,NCELLS,PAKADR,POINTER,IETA,IPHI,LAYER,FLOOR
      INTEGER IETA_HOT(5)
      INTEGER IPHI_HOT(5),LAYER_HOT(5),LAYER_HOTCELL(5)
      REAL EDPTH(5),PDPTH(5),ENERGY_HOT(5)
      REAL ENERGY,ETOT
C
C----------------------------------------------------------------------
C
      ETOT = 0
      DO I = 1,5
        EDPTH(I) = 0
        PDPTH(I) = 0
        ENERGY_HOT(I) = 0
      ENDDO
C
      ncells = iq(lcash+2)
      pointer=1
C
C ****  loop over cells..
C
      DO i = 1,ncells
        pointer = pointer+2
        pakadr = iq(lcash+pointer)
        energy = q(lcash+pointer+1)
        CALL CAEP_INDICES(PAKADR,IETA,IPHI,LAYER)
C
C ****  Which floor? Condense EM3, skip if not EM.
C
        IF(layer.LT.lyem3a) THEN
          floor = layer
        ELSEIF(layer.GE.lyem3a .AND. layer.LE.lyem3d) THEN
          floor = 3
        ELSEIF(layer.EQ.mxlyem) THEN
          floor = 4
        ELSE
          FLOOR = 5
        ENDIF
        IF(energy .GT. energy_hot(floor)) THEN
          energy_hot(floor) = energy
          ieta_hot(floor)   = ieta
          iphi_hot(floor)   = iphi
          layer_hot(floor)  = layer
        ENDIF
        EDPTH(FLOOR)  = EDPTH(FLOOR)+ENERGY
        ETOT = ETOT+ENERGY
C
      ENDDO
      IF(ETOT.GT.0) THEN
        DO I = 1,5
          PDPTH(I) = EDPTH(I)/ETOT
        ENDDO
      ENDIF
C
  999 RETURN
C----------------------------------------------------------------------
      ENTRY CEMDPTH_LAYER(LAYER_HOTCELL)
C-
C-   Added 5-AUG-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      DO I=1,5
        LAYER_HOTCELL(I) = LAYER_HOT(I)
      END DO
      RETURN
      END
