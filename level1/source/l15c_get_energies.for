      SUBROUTINE L15C_GET_ENERGIES ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieve the Level 1.5 Calorimeter Trigger Input
C-                         energy quantities derived from from the Level 1
C-                         Calorimter Trigger Input data. 
C-                         The code in this routine is only relevant within 
C-                         L1SIM, and after the call to L1_CALTRIG_TOWERS
C-
C-   Inputs  : None
C-   Outputs : fill the array of raw data bytes input to the L1.5 CT
C-              and derive the offset corrected energies in GeV.
C-   Controls: None
C-
C-   Created   5-MAY-1994   Philippe Laurens - MSU L1 Trigger
C-   Updated   6-MAY-1994   Dan Owen  Changed dimensions of energy arrays and
C-                                    equivalenced to EMET and TOTET arrays 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_LOCAL_DSP.PARAMS'
      INCLUDE 'D0$INC:L15_LOCAL_DSP.INC'
C
C
C   Local Variables
      INTEGER      PHI, ETA, ETA_SIGN, SETA
      INTEGER      PROM_BYTE(EM_ET_QUANT:PY_QUANT)
C
C-----------------------------------------------------------------------
C
      DO PHI = PHI_MIN, PHI_MAX
        DO ETA = ETA_MIN, ETA_MAX
          DO ETA_SIGN = POS_ETA, NEG_ETA
C generate signed value of eta
            IF ( ETA_SIGN .EQ. POS_ETA ) THEN
              SETA = ETA
            ELSE
              SETA = -ETA
            ENDIF
C
C   get ther response of the Lookup PROMs to the raw ADC input
C
            CALL PROM_RESPONSES( ETA_SIGN, ETA, PHI,
     &                           LEVEL_0_NZ,
     &                           FADC_BYTE (ETA_SIGN,ETA,PHI,EM_TOWER),
     &                           FADC_BYTE (ETA_SIGN,ETA,PHI,HD_TOWER),
     &                           PROM_BYTE )
C
C   Tot Et is obtained as the 9-bit sum of the EM and HD Et
C   Note that EM and HD Et are both subject to the low energy cut
C
            L15CT_TT_RAW ( SETA, PHI, L15C_TOT_TWR)
     &        = ( PROM_BYTE(EM_ET_QUANT) + PROM_BYTE(HD_ET_QUANT) )
C
C   The 9-bit sum is truncated down to 8-bit with saturation at 255 counts
C
            IF ( L15CT_TT_RAW ( SETA, PHI, L15C_TOT_TWR)
     &          .GT. 255 )
     &           L15CT_TT_RAW ( SETA, PHI, L15C_TOT_TWR) = 255
C
C   The Scale is 1/4 GeV per count and there is a Zero Energy Offset
C
            L15CT_TT_ENERGY ( SETA, PHI, L15C_TOT_TWR)
     &        = ( L15CT_TT_RAW ( SETA, PHI, L15C_TOT_TWR)
     &          - LOOKUP_ZERESP(ETA_SIGN, ETA, PHI, EM_ET_QUANT)
     &          - LOOKUP_ZERESP(ETA_SIGN, ETA, PHI, HD_ET_QUANT) )
     &          * GLOBAL_ENERGY_SCALE (EM_ET_QUANT)
C
C   EM Et is obtained during the second lookup.
C   It is still the sum of the EM and HD Lookup PROM outputs
C   but the HD contribution is a constant (that is only eta-dependent)
C
            L15CT_TT_RAW ( SETA, PHI, L15C_EM_TWR)
     &        = PROM_BYTE(EM_L2_QUANT) + PROM_BYTE(HD_L2_QUANT)
C
C
C   The 9-bit sum is truncated down to 8-bit with saturation at 255 counts
C   The saturation can be reached because the HD contribution is not zero
C
            IF ( L15CT_TT_RAW ( SETA, PHI, L15C_EM_TWR)
     &          .GT. 255 )
     &           L15CT_TT_RAW ( SETA, PHI, L15C_EM_TWR) = 255
C
C   The Scale is 1/4 GeV per count and there is a Zero Energy Offset
C
            L15CT_TT_ENERGY ( SETA, PHI, L15C_EM_TWR )
     &        = ( L15CT_TT_RAW ( SETA, PHI, L15C_EM_TWR)
     &          - LOOKUP_ZERESP(ETA_SIGN, ETA, PHI, EM_L2_QUANT)
     &          - LOOKUP_ZERESP(ETA_SIGN, ETA, PHI, HD_L2_QUANT) )
     &          * GLOBAL_ENERGY_SCALE (EM_ET_QUANT)
C
          END DO
        END DO
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
