      SUBROUTINE PU_CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,
     &  ENERGY_HOT)
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
C-   Modified 28-MAY-1993   Nobuaki Oshima 
C-                          Clear all output buffers at first.
C-   Modified 23-MAR-1993   Sailesh Chopra
C-   Created  12-JUN-1992   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCASH
      INTEGER I,NCELLS,PAKADR,POINTER,IETA,IPHI,LAYER,FLOOR
      INTEGER IETA_HOT(8),IPHI_HOT(8)
      INTEGER LAYER_HOT(8)
      REAL EDPTH(8),PDPTH(8),ENERGY_HOT(8)
      REAL ENERGY,ETOT
C
C----------------------------------------------------------------------
C
      ETOT = 0
      DO I = 1,8
        IETA_HOT(I) = 0
        IPHI_HOT(I) = 0
        EDPTH(I) = 0.
        PDPTH(I) = 0.
        ENERGY_HOT(I) = 0.
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
          floor = layer
        ELSEIF(layer.EQ.mxlyem) THEN
          floor = 7
        ELSE
          FLOOR = 8
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
        DO I = 1,8
          PDPTH(I) = EDPTH(I)/ETOT
        ENDDO
      ENDIF
C
  999 RETURN
      END
