      SUBROUTINE L1C_FIND_CARDS ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initializes a part of the common :
C-                         L1C_CARDS_USE.INC
C-
C-   Inputs  : None.
C-
C-   Outputs : None
C-
C-   Controls: None.
C-
C-   Created   2-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Modified to use outside the context of Tisserant's
C-                          configuration file interpreter. 
C-                        - Changed name of routine from
C-                          LEVEL1_CAL_TRIG_SETUP_INIT to L1C_FIND_CARDS. 
C-                        - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                          D0$PARAMS:L1_CALTRIG.PARAMS 
C-                        - Replaced D0$INC:LEVEL1_CAL_TRIG_SETUP.INC with
C-                          D0$INC:L1C_CARDS_USE.INC 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$INC:L1C_CARDS_USE.INC'
C
      INTEGER ERR
C
      INTEGER CHANNEL, PHI, ETA_SIGN
C
C----------------------------------------------------------------------
C
      DO ETA_USE = ETA_MAX, ETA_MIN, -1
        DO CHANNEL = EM_TOWER, HD_TOWER
          DO PHI = PHI_MIN, PHI_MAX
            DO ETA_SIGN = POS_ETA, NEG_ETA
              IF(DAC_BYTE(ETA_SIGN,ETA_USE,PHI,CHANNEL).NE.0) GOTO 10
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      ETA_USE = ETA_MAX
C
   10 TOWER_USE       = 2 * ETA_USE * (PHI_MAX - PHI_MIN + 1)
      CTFE_USE        = (TOWER_USE        + 3) / 4
      CHTCR_USE       = (CTFE_USE         + 7) / 8
      SCND_HOT_USE    = (CHTCR_USE        + 7) / 8
      FRST_SCALAR_USE = (CTFE_USE         + 7) / 8
      SCND_SCALAR_USE = (FRST_SCALAR_USE  + 7) / 8
      FRST_MPT_USE    = (((CTFE_USE+1)/2) + 7) / 8
      SCND_MPT_USE    = (FRST_MPT_USE     + 7) / 8
      RETURN
C
      END
