      SUBROUTINE L1_CALTRIG_ANAL ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : THIS IS THE ANALYSIS ROUTINE
C-
C-   Inputs  :  none 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-SEP-1990   MARIS ABOLINS
C-   Updated  16-AUG-1991   DC  Return at top HBOOK Directory
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Computation now takes ADC_ZERESP into account
C-                          - Changed name of routine from L1C_ANAL to
C-                            L1_FW_AND_CT_ANAL. 
C-                          - Replaced D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS with
C-                            D0$PARAMS:L1_FRAMEWORK.PARAMS 
C-                          - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                            D0$PARAMS:L1_CALTRIG.PARAMS 
C-                          - Replaced D0$INC:SPECIFIC_TRIGGER.INC with
C-                            D0$INC:L1_SPECIFIC_TRIGGER.INC 
C-                          - Replaced D0$INC:TRG_SIMUL_EVENT.INC with
C-                            D0$INC:L1C_EVENT.INC 
C-                          - Replaced D0$INC:TRG_SIMUL_RAW_EVENT.INC with
C-                            D0$INC:L1C_EVENT_RAW.INC 
C-   Updated   7-NOV-1991   Philippe Laurens, Steven Klocek   
C-              Change histogram name "TRIGGER EFFIC." to "TRIGGER HITS".
C-              Fix numbering of sptrg. 0..31 and book only 32 channels.
C-   
C-   Updated   2-DEC-1991   Philippe Laurens, Steven Klocek   
C-              Rename hbook sub-directory from L1C to L1SIM
C-              "TOT" histograms were using only HD, now use EM+HD 
C-              Energy histogram were in units of ADC counts, now GeV
C-              Add HD tower count and HD energy histograms, 
C-              Reorder to display trigger hit histogram first.
C-   Updated   9-DEC-1991   Philippe Laurens, Steven Klocek   
C-              Increment number of events processed, number of events passed,
C-              number of pure Level 1 events passed, and number of events
C-              passed by Specific Trigger.
C-              Book the 'Level 1.5 Submitted' histogram here so they will be
C-              printed out in the desired order.
C-   Updated  13-DEC-1991   Philippe Laurens, Steven Klocek  
C-              Renamed this routine from L1_FW_AND_CT_ANAL to L1_CALTRIG_ANAL.
C-              Moved creation of HBOOK directory L1SIM to L1_AND_L15_FW_ANAL.
C-              Moved booking of Framework histograms to L1_AND_L15_FW_ANAL.
C-              Moved recording of Level 1 Trigger firing scalers to
C-                L1_AND_L15_FW_ANAL. 
C-              Removed unused variables and include files.
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek   
C-                      All Specific Trigger variables use [0,31] for indices
C-   Updated   8-MAR-1992   Philippe Laurens, Steven Klocek  
C-                      update var name FIRED_NUMBER -> NUM_SPTRG_PASS_L1SIM
C-   Updated   3-FEB-1993   Philippe Laurens
C-              fill energy histograms using quantities after PROM lookup,  
C-              instead of raw ADC counts.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER I ,J ,K
C
      INTEGER EM_ADC, HD_ADC, TOT_ADC   ! TEMPORARY BYTES
      INTEGER TOT_EMTWRS, TOT_HDTWRS, TOT_TOTWRS    ! NO. OF TOWERS
      REAL    TOT_EM_ET, TOT_HD_ET, TOT_TOT_ET ! TRANSVERSE ENERGIES
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
      INCLUDE 'D0$INC:L1C_Z_CORRECTED_ET.INC'
C
      REAL W2 ,TOT_WEIGHT, TOT_W2
      REAL TOTAL, TOT_CROSS, CROSS
      REAL GLOBAL_EFFICIENCY , GLOBAL_EFF_W2
      REAL EFFICIENCY(TRG_NUM_MIN:TRG_NUM_MAX) 
      REAL EFF_W2(TRG_NUM_MIN:TRG_NUM_MAX)
      INTEGER TRIGGER
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      CALL HCDIR('//PAWC/L1SIM',' ')  ! go to CTTR directory
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        CALL HBOOK1 (1011, ' NO OF EM TOWERS$',  100, 0., 1000., 0) 
        CALL HBOOK1 (1012, ' NO OF HD TOWERS$',  100, 0., 1000., 0) 
        CALL HBOOK1 (1013, ' NO OF TOT TOWERS$', 100, 0., 1000., 0) 
C
        CALL HBOOK1 (1021, ' TOT EM ET (GEV)$',  100, 0.,  500., 0)
        CALL HBOOK1 (1022, ' TOT HD ET (GEV)$',  100, 0.,  500., 0)
        CALL HBOOK1 (1023, ' TOT ET (GEV)$',     100, 0.,  500., 0)
C
        CALL HBOOK2 (2001, ' EM_ET VS HD_ET$',
     &                    100, 0., 500., 100, 0., 500.)
C
      ENDIF
C
C       compute trigger efficiencies
C
      IF(ISAJET_WG.EQ.0.) ISAJET_WG = 1.
      W2 = ISAJET_WG**2
      TOT_WEIGHT = TOT_WEIGHT +ISAJET_WG
      TOT_W2 = TOT_W2  +  W2
      TOT_CROSS = TOT_CROSS + ISAJET_CS
      TOTAL = TOTAL + 1.
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX 
        IF(FIRED_TRIGGER(TRIGGER)) THEN
          EFFICIENCY(TRIGGER) = EFFICIENCY(TRIGGER) + ISAJET_WG
          EFF_W2(TRIGGER)     = EFF_W2(TRIGGER)     +  W2
        ENDIF
      ENDDO
      IF ( NUM_SPTRG_PASS_L1SIM .NE. 0 ) THEN
        GLOBAL_EFFICIENCY = GLOBAL_EFFICIENCY + ISAJET_WG
        GLOBAL_EFF_W2     = GLOBAL_EFF_W2     + W2
      ENDIF
C
C       COUNT NUMBER OF TOWERS NOT ZERO
C
      TOT_EMTWRS = 0
      TOT_HDTWRS = 0
      TOT_TOTWRS = 0
      TOT_EM_ET  = 0.
      TOT_HD_ET  = 0.
      TOT_TOT_ET = 0.
      DO K = POS_ETA , NEG_ETA
        DO J = ETA_MIN , ETA_MAX
          DO I = PHI_MIN , PHI_MAX
C
            EM_ADC  = Z_CORRECTED_ET(K,J,I,EM_TOWER) 
            HD_ADC  = Z_CORRECTED_ET(K,J,I,HD_TOWER) 
            TOT_ADC = EM_ADC + HD_ADC
C
            IF ( EM_ADC .NE. 0 ) THEN
              TOT_EMTWRS = TOT_EMTWRS + 1
              TOT_EM_ET  = TOT_EM_ET + EM_ADC
            ENDIF
            IF ( HD_ADC .NE. 0 ) THEN
              TOT_HDTWRS = TOT_HDTWRS + 1
              TOT_HD_ET  = TOT_HD_ET + HD_ADC
            ENDIF
            IF ( TOT_ADC .NE. 0 ) THEN
              TOT_TOTWRS = TOT_TOTWRS + 1
              TOT_TOT_ET = TOT_TOT_ET + TOT_ADC
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      CALL HF1 ( 1011, FLOAT(TOT_EMTWRS), 1. )     ! TOTAL # EM TOWERS
      CALL HF1 ( 1012, FLOAT(TOT_HDTWRS), 1. )     ! TOTAL # HD TOWERS
      CALL HF1 ( 1013, FLOAT(TOT_TOTWRS), 1. )     ! TOTAL # TOT TOWERS
C
      CALL HF1 ( 1021, TOT_EM_ET*GLOBAL_ADC_SCALE,  1. )     ! EM_ET IN GEV
      CALL HF1 ( 1022, TOT_HD_ET*GLOBAL_ADC_SCALE,  1. )     ! HD_ET IN GEV
      CALL HF1 ( 1023, TOT_TOT_ET*GLOBAL_ADC_SCALE, 1. )     ! TOT_ET IN GEV
C
      CALL HF2 ( 2001, TOT_EM_ET*GLOBAL_ADC_SCALE, 
     &                 TOT_HD_ET*GLOBAL_ADC_SCALE, 1. ) ! SCATTER PLOT
C
      CALL HCDIR ('//PAWC',' ')       ! LEAVE HBOOK IN TOP DIRECTORY
  999 RETURN
      END
