      SUBROUTINE ADC_CONVERSIONS (TRIGGER_TOWER_ENERGY, ADC_BYTE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulates the analog to digital conversions 
C-                         performed by the Calorimeter Trigger for the
C-                         specified set of energy deposits over the whole
C-                         detector. 
C-   Inputs  : 
C-    TRIGGER_TOWER_ENERGY is the energy deposited in each Electro-Magnetic 
C-                         and each Hadronic Trigger Tower.
C-                         This is a FORTRAN array variable 
C-                         See the parameter file for translation of dimensions
C-                         Units : GeV
C-
C-   Outputs : 
C-     ADC_BYTE            is the set of simulated ADC output bytes for the
C-                         specified set of input energy deposit TT_ENERGY.
C-                         This is a FORTRAN array variable 
C-                         See the parameter file for translation of dimensions
C-
C-   Controls: None
C-
C-   Comments :
C-                         A call to INIT_LOOKUP_ROUTINES must have been made
C-                         before this routine can be called.
C-                         
C-                         The routine will fill with zeros all towers whose
C-                         description does not appear in the lookup tables.
C-                         The existence test is made on the downloaded DAC
C-                         byte that by definition is non-zero only for
C-                         existing Trigger Towers. 
C-                         
C-                         This simulation does not take the electronics noise
C-                         into consideration.
C-                         This simulation does not take the Analog to Digital
C-                         Conversion Linearity Errors into consideration.
C-
C-                         This simulation will take into account the analog
C-                         scaling of the signal coming from the BLS card at
C-                         the input of the CTFE cards. This scaling typically
C-                         transforms the raw energy deposit into its
C-                         transverse component assuming the interaction vertex
C-                         was at the center of the detector. There might be
C-                         some sub-range of the detector where the energy is 
C-                         only scaled to a multiple of this z-uncorrected 
C-                         transverse component.
C-
C-                         This simulation will take into account any output 
C-                         offset performed by the ADC.
C-                         
C-                         This simulation can also reproduce the action 
C-                         of the biases due to improper amplitude or timing of
C-                         the trigger pickoff signals. 
C-                         
C-   Defined 29-JAN-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Updated  2-AUG-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Updated  31-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                     - Made IMPLICIT NONE statement recognizable by D0FLAVOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE 
C
      INCLUDE   'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE   'D0$INC:LEVEL1_LOOKUP.INC'
C
      REAL       TRIGGER_TOWER_ENERGY ( POS_ETA:NEG_ETA,
     &                                  ETA_MIN:ETA_MAX,
     &                                  PHI_MIN:PHI_MAX,
     &                                  EM_TOWER:HD_TOWER )
      INTEGER    ADC_BYTE             ( POS_ETA:NEG_ETA,
     &                                  ETA_MIN:ETA_MAX,
     &                                  PHI_MIN:PHI_MAX,
     &                                  EM_TOWER:HD_TOWER )
C
      INTEGER    SIGN_ETA, MAGN_ETA, PHI
      REAL       TEMP
C
C----------------------------------------------------------------------
C
      DO PHI = PHI_MIN, PHI_MAX
       DO MAGN_ETA = ETA_MIN, ETA_MAX
        DO SIGN_ETA = POS_ETA, NEG_ETA
C
          IF ( DAC_BYTE (SIGN_ETA,MAGN_ETA,PHI,EM_TOWER) .EQ. 0 ) THEN
            ADC_BYTE (SIGN_ETA,MAGN_ETA,PHI,EM_TOWER) = 0
          ELSE 
C
            TEMP = TRIGGER_TOWER_ENERGY (SIGN_ETA,MAGN_ETA,PHI,EM_TOWER)
     &           * ADC_CNT_VS_RAW_E (SIGN_ETA,MAGN_ETA,PHI,EM_TOWER)
     &           + ADC_ZERESP (SIGN_ETA,MAGN_ETA,PHI,EM_TOWER) 
C
            IF ( TEMP .LT. 0.5 ) THEN 
              ADC_BYTE (SIGN_ETA,MAGN_ETA,PHI,EM_TOWER) = 0
            ELSE IF ( TEMP .GE. 254.5 ) THEN
              ADC_BYTE (SIGN_ETA,MAGN_ETA,PHI,EM_TOWER) = 255
            ELSE
              ADC_BYTE (SIGN_ETA,MAGN_ETA,PHI,EM_TOWER) = NINT(TEMP)
            END IF
C
          END IF
C
          IF ( DAC_BYTE (SIGN_ETA,MAGN_ETA,PHI,HD_TOWER) .EQ. 0 ) THEN
            ADC_BYTE (SIGN_ETA,MAGN_ETA,PHI,HD_TOWER) = 0
          ELSE 
C
            TEMP = TRIGGER_TOWER_ENERGY (SIGN_ETA,MAGN_ETA,PHI,HD_TOWER)
     &           * ADC_CNT_VS_RAW_E (SIGN_ETA,MAGN_ETA,PHI,HD_TOWER)
     &           + ADC_ZERESP (SIGN_ETA,MAGN_ETA,PHI,HD_TOWER) 
C
            IF ( TEMP .LT. 0.5 ) THEN 
              ADC_BYTE (SIGN_ETA,MAGN_ETA,PHI,HD_TOWER) = 0
            ELSE IF ( TEMP .GE. 254.5 ) THEN
              ADC_BYTE (SIGN_ETA,MAGN_ETA,PHI,HD_TOWER) = 255
            ELSE
              ADC_BYTE (SIGN_ETA,MAGN_ETA,PHI,HD_TOWER) = NINT(TEMP)
            END IF
C
          END IF
C
        END DO
       END DO
      END DO
C----------------------------------------------------------------------
  999 RETURN        
      END
