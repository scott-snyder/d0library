C----------------------------------------------------------------------
C-
C-                      SPECIFIC_TRIGGER
C-                      ================
C-
C-   Provisonnal common containing the Specific Trigger related tables.
C-
C-   Created   5-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              SPECIFIC_TRIGGER to L1_SPECIFIC_TRIGGER. 
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek  
C-                            Removed variables no longer used by L1SIM:
C-                              GLOBAL_ENERGY_FREE
C-                              GLOBAL_ENERGY_STATE
C-                              TOTAL_MPT_FREE
C-                              TOTAL_MPT_STATE
C-                              HOT_TOWER_FREE
C-                              HOT_TOWER_STATE
C-   Updated  16-JAN-1992   Philippe Laurens, Steven Klocek  
C-                              Added flags to indicate if scalers were
C-                              incremented in the current event. 
C-   Updated   8-MAR-1992   Philippe Laurens, Steven Klocek  
C-                              replace single variable FIRED_NUMBER with
C-                              several variables counting number of specific 
C-                              triggers firing at different stages.
C-   Updated  10-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                              extend PROGRAMMING_MASK for Large Tiles 
C-
C----------------------------------------------------------------------
C
C     Programmed and Fired Specific Trigger List
C     ------------------------------------------
C
      LOGICAL PROGRAMMED_TRIGGER (TRG_NUM_MIN:TRG_NUM_MAX)
C
      LOGICAL FIRED_TRIGGER      (TRG_NUM_MIN:TRG_NUM_MAX)
C
      INTEGER NUM_SPTRG_PASS_L1SIM
C
      INTEGER NUM_SPTRG_PASS_L1
      INTEGER NUM_PURE_L1_SPTRG_PASS_L1
      INTEGER NUM_L15_SPTRG_PASS_L1
      INTEGER NUM_L15_SPTRG_SENT_L15
      INTEGER NUM_L15_SPTRG_PASS_L15
C
C     Trigger Scalers
C     ===============
C
      INTEGER ENABLE_SCALERS      (2,TRG_NUM_MIN:TRG_NUM_MAX)
      INTEGER ENABLE_SCALERS_INCREMENTED(TRG_NUM_MIN:TRG_NUM_MAX)
C
      INTEGER FIRED_SCALERS       (2,TRG_NUM_MIN:TRG_NUM_MAX)
      INTEGER FIRED_SCALERS_INCREMENTED(TRG_NUM_MIN:TRG_NUM_MAX)
C
      INTEGER TRIGGER_SCALER      (2)
      INTEGER TRIGGER_SCALER_INCREMENTED
C
      INTEGER BEAM_SCALER         (2)
      INTEGER BEAM_SCALER_INCREMENTED
C
C     Programmed and Fired Trigger Masks
C     ==================================
C
      INTEGER PROGRAMMING_MASK    (EM_ET_REF_MIN:LT_REF_MAX)
C
      INTEGER FIRED_MASK
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1_SPECIFIC_TRIGGER/ PROGRAMMED_TRIGGER,
     +                          FIRED_TRIGGER,
     +                          NUM_SPTRG_PASS_L1SIM,
     +                          ENABLE_SCALERS,
     +                          FIRED_SCALERS,
     +                          TRIGGER_SCALER,
     +                          BEAM_SCALER,
     +                          PROGRAMMING_MASK,
     +                          FIRED_MASK,
     +                          ENABLE_SCALERS_INCREMENTED,
     +                          FIRED_SCALERS_INCREMENTED,
     +                          TRIGGER_SCALER_INCREMENTED,
     +                          BEAM_SCALER_INCREMENTED,
     +                          NUM_SPTRG_PASS_L1,
     +                          NUM_PURE_L1_SPTRG_PASS_L1,
     +                          NUM_L15_SPTRG_PASS_L1,
     +                          NUM_L15_SPTRG_SENT_L15,
     +                          NUM_L15_SPTRG_PASS_L15
C
