      FUNCTION L1DMP_L1EXTRACT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call and display the results of many ZEBRA_UTIL 
C-                         L1EXTRACT_* routines. This routine is typically used
C-                         for testing these routines.
C-
C-   Returned value  : Success value
C-   Inputs  : ZEBRA input
C-   Outputs : Dump File output
C-   Controls: none
C-
C-   Created   9-SEP-1992   Philippe Laurens, Steven Klocek
C-   Updated  13-MAY-1993   Philippe Laurens - MSU L1 Trigger  
C-                        update call to L1EXTRACT_L15_STATUS to match the 
C-                        routine arguments from ZEBRA_UTIL
C-                        Add entry L1DMP_L1EXTRACT_DEFDUMP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L1DMP_L1EXTRACT
      LOGICAL L1DMP_L1EXTRACT_DEFDUMP
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LTRGR_LEVEL1, GZTRGR, GZFIND_CRATE
C
      INTEGER LUN
      INTEGER        SPTRG_FIRED_MASK
      INTEGER        LOW, HIGH 
      PARAMETER    ( LOW  = 1, HIGH = 2 )
      INTEGER        TRIGGER_NUMBER (LOW:HIGH)
      REAL EM_ET, HD_ET, TOT_ET, MIS_PT, EM_L2, HD_L2, TOT_L2
      REAL PX, PY, MIS_PT2
      INTEGER AO_STATE(0:255)
      LOGICAL STATE
      INTEGER SPTRG_SCALER(2, 0:31)
      INTEGER EXPOS_SCALER(2, 0:31)
      INTEGER BEAMX_SCALER(2)
      INTEGER GATED_BEAMX_SCALER(2)
      INTEGER L0GOOD_SCALER(2)
      INTEGER L0_PER_BUNCH_SCALERS(2, 6)
      LOGICAL L0GOOD
      INTEGER L0BIN
      INTEGER EVENT_TRANSFER_SCALER(2)
      INTEGER L1_PER_BUNCH_SCALERS(2, 6)
      INTEGER EM_TOWERS(4), TOT_TOWERS(4)
      REAL EM_GEV, TOT_GEV
      INTEGER ACNET_DATE, ACNET_TIME
      INTEGER COUNT, COUNT2, BIT
      INTEGER PHI, MAGN_ETA
      REAL ADC_EM(-2:2, 1:32), ADC_TOT(-2:2, 1:32)
C
      LOGICAL JET_COMPLETE
      INTEGER JET_LENGTH
      INTEGER ENTRY_LIST(1:16, 0:1)
C
      INTEGER L15_CYCLE_SCALER(2)
      INTEGER L15_POTENTIAL_SCALER(2)
      INTEGER L15_SKIP_SCALER(2)
      INTEGER AFTER_L15_DEAD_BEAMX_SCALER(2)
      INTEGER AFTER_L15_PASS_SCALER(2)
      INTEGER AFTER_L15_FAIL_SCALER(2)
      INTEGER AFTER_L15_TIMEOUT_SCALER(2)
      INTEGER BEFORE_L15_DEAD_BEAMX_SCALER(2)
      INTEGER BEFORE_L15_PASS_SCALER(2)
      INTEGER BEFORE_L15_FAIL_SCALER(2)
      INTEGER BEFORE_L15_TIMEOUT_SCALER(2)
C
      LOGICAL USED_L15, SKIPPED_L15
      LOGICAL TIMED_OUT
      LOGICAL ST_USED_L15(0:15)
      LOGICAL ST_L15_ANSWER(0:15)
      LOGICAL L15_TERM_DONE(0:31)
      LOGICAL L15_TERM_ANSWER(0:31)
C
      INTEGER  DMPUNI
      EXTERNAL DMPUNI
C
      LOGICAL  YES
      SAVE     YES
      DATA     YES/.TRUE./
C-----------------------------------------------------------------------
      L1DMP_L1EXTRACT = .FALSE.
C
C   See Entry L1DMP_L1EXTRACT_DEFDUMP
C
      IF ( YES .NEQV. .TRUE. ) GOTO 999
C
      LTRGR_LEVEL1 = GZFIND_CRATE( 'TRGR', GZTRGR(), 11)
      IF (LTRGR_LEVEL1 .EQ. 0) THEN
        CALL ERRMSG('No TRGR Bank', 'L1DMP_L1EXTRACT',
     &    'No TRGR bank found', 'W')
        GOTO 999
      ENDIF
C
      LUN = DMPUNI()
      WRITE ( LUN, 1 ) 
    1 FORMAT ( ' ', 80('-') / 
     &         ' Dump from L1EXTRACT* routines' /
     &         ' =============================' )
C
      CALL L1_DATA_FOR_HEAD_BANK ( IQ(LTRGR_LEVEL1), 
     &                             SPTRG_FIRED_MASK, 
     &                             TRIGGER_NUMBER )
C
      CALL       L1EXTRACT_L15_SCALERS(IQ(LTRGR_LEVEL1),
     &                                 L15_CYCLE_SCALER,
     &                                 L15_POTENTIAL_SCALER,
     &                                 L15_SKIP_SCALER,
     &                                 AFTER_L15_DEAD_BEAMX_SCALER,
     &                                 AFTER_L15_PASS_SCALER,
     &                                 AFTER_L15_FAIL_SCALER,
     &                                 AFTER_L15_TIMEOUT_SCALER,
     &                                 BEFORE_L15_DEAD_BEAMX_SCALER,
     &                                 BEFORE_L15_PASS_SCALER,
     &                                 BEFORE_L15_FAIL_SCALER,
     &                                 BEFORE_L15_TIMEOUT_SCALER)
C
      CALL L1EXTRACT_L15_STATUS(IQ(LTRGR_LEVEL1), 
     &                          USED_L15, SKIPPED_L15, TIMED_OUT, 
     &                          ST_USED_L15, ST_L15_ANSWER, 
     &                          L15_TERM_DONE, L15_TERM_ANSWER)
C
      CALL L1EXTRACT_TRANSV_ENERGIES(IQ(LTRGR_LEVEL1),
     &  EM_ET, HD_ET, TOT_ET, MIS_PT)
      CALL L1EXTRACT_2ND_LKP_ENERGIES(IQ(LTRGR_LEVEL1),
     &  EM_L2, HD_L2, TOT_L2)
      CALL L1EXTRACT_MOMENTUM(IQ(LTRGR_LEVEL1),
     &  PX, PY, MIS_PT2)
C
      DO COUNT = 0, 255
        CALL L1EXTRACT_ANDOR_TERM(IQ(LTRGR_LEVEL1), COUNT, STATE)
        IF (STATE .EQV. .TRUE.) THEN
          AO_STATE(COUNT) = 1
        ELSE
          AO_STATE(COUNT) = 0
        ENDIF
      END DO
C
      CALL L1EXTRACT_SPTRG_FIRED_SCALERS(IQ(LTRGR_LEVEL1), SPTRG_SCALER)
      CALL L1EXTRACT_SPTRG_EXPOS_SCALERS(IQ(LTRGR_LEVEL1), EXPOS_SCALER)
      CALL L1EXTRACT_L0_FAST_Z_DATA(IQ(LTRGR_LEVEL1), L0BIN, L0GOOD)
      CALL L1EXTRACT_GLOBAL_TOWER_COUNTS(IQ(LTRGR_LEVEL1),
     &  EM_TOWERS, TOT_TOWERS)
      CALL L1EXTRACT_ACNET_TIME(IQ(LTRGR_LEVEL1), 
     &                          ACNET_DATE, ACNET_TIME)
      CALL L1EXTRACT_BEAMX_SCALER(IQ(LTRGR_LEVEL1), BEAMX_SCALER,
     &  GATED_BEAMX_SCALER)
      CALL L1EXTRACT_L0_FAST_Z_SCALERS(IQ(LTRGR_LEVEL1), L0GOOD_SCALER,
     &  L0_PER_BUNCH_SCALERS)
      CALL L1EXTRACT_L1_FIRED_SCALERS(IQ(LTRGR_LEVEL1), 
     &  EVENT_TRANSFER_SCALER, L1_PER_BUNCH_SCALERS)
C
C
      WRITE ( LUN, 50 ) SPTRG_FIRED_MASK, 
     &                  TRIGGER_NUMBER(HIGH), TRIGGER_NUMBER(LOW)
   50 FORMAT ( ' L1_DATA_FOR_HEAD_BANK (hex) Trigger Mask: ', Z8.8, 
     &         ' Trigger Number: ', Z4.4, Z6.6 ) 
C
      WRITE (LUN, 100) EM_ET, HD_ET, TOT_ET, MIS_PT
      WRITE (LUN, 200) EM_L2, HD_L2, TOT_L2
      WRITE (LUN, 210) PX, PY, MIS_PT2
C
  100 FORMAT (' ENERGIES: EM Et', F10.2, ' HD Et', F10.2,
     &  ' Tot Et', F10.2, ' Mis Pt', F10.2)
  200 FORMAT (' 2ND LOOKUP: 2nd Et', F10.2, ' 2nd HD', F10.2,
     &  ' 2nd Tot', F10.2)
  210 FORMAT (' MOMENTUM  : Px', F10.2, ' Py', F10.2, ' Mis Pt', F10.2)
  300 FORMAT (' ', A, I8, X, I8)
C
      WRITE (LUN, *) 'EM  Towers:', EM_TOWERS
      WRITE (LUN, *) 'TOT Towers:', TOT_TOWERS
C
      DO COUNT = 0, 31
        WRITE (LUN, *) 'ST Fired Scaler #', COUNT, ': ',
     &    SPTRG_SCALER(2, COUNT), '/', SPTRG_SCALER(1, COUNT)
      END DO
C
      DO COUNT = 0, 31
        WRITE (LUN, *) 'ST Expos Scaler #', COUNT, ': ',
     &    EXPOS_SCALER(2, COUNT), '/', EXPOS_SCALER(1, COUNT)
      END DO
C
      WRITE (LUN, *) 'Beam Crossing Scaler:', BEAMX_SCALER(2), '/',
     &  BEAMX_SCALER(1)
      WRITE (LUN, *) 'Gated BeamX Scaler:', GATED_BEAMX_SCALER(2), '/',
     &  GATED_BEAMX_SCALER(1)
C
      WRITE (LUN, *) 'Andor Terms:'
  220 FORMAT (' ', '(', I3, ':', I3, ')', T12, 4(8I1.1, ' '))
      DO COUNT = 0, 255, 32
        WRITE (LUN, 220) COUNT, COUNT+31,
     &    (AO_STATE(COUNT+COUNT2), COUNT2 = 0, 31)
      END DO
C
      WRITE (LUN, *) 'L0 Bin:', L0BIN, ' L0 Good: ', L0GOOD
C
      WRITE (LUN, *) 'Level 0 Good Scaler:', L0GOOD_SCALER(2), '/',
     &  L0GOOD_SCALER(1)
      DO COUNT = 1, 6
        WRITE (LUN, *) 'Level 0 Per Bunch #', COUNT, ':',
     &    L0_PER_BUNCH_SCALERS(2, COUNT), '/',
     &    L0_PER_BUNCH_SCALERS(1, COUNT)
      END DO
C
      WRITE (LUN, *) 'Event Transfer Scaler:', EVENT_TRANSFER_SCALER(2),
     &  '/', EVENT_TRANSFER_SCALER(1)
      DO COUNT = 1, 6
        WRITE (LUN, *) 'Level 1 Per Bunch #', COUNT, ':',
     &    L1_PER_BUNCH_SCALERS(2, COUNT), '/',
     &    L1_PER_BUNCH_SCALERS(1, COUNT)
      END DO
C
      WRITE (LUN, *) 'ACnet Time:', ACNET_DATE, ACNET_TIME
C
C
      CALL L1EXTRACT_JET_LIST(IQ(LTRGR_LEVEL1), 0, JET_COMPLETE, 
     &                        JET_LENGTH, ENTRY_LIST)
      WRITE (LUN, *) 'EM Jet List Complete ', JET_COMPLETE
      WRITE (LUN, *) 'EM Jet List Length ', JET_LENGTH
      DO COUNT = 1, JET_LENGTH
        WRITE (LUN, 400) COUNT, ENTRY_LIST(COUNT, 0),
     &                   ENTRY_LIST(COUNT, 1)
      END DO
C
      CALL L1EXTRACT_JET_LIST(IQ(LTRGR_LEVEL1), 1, JET_COMPLETE, 
     &                        JET_LENGTH, ENTRY_LIST)
      WRITE (LUN, *) 'TOT Jet List Complete ', JET_COMPLETE
      WRITE (LUN, *) 'TOT Jet List Length ', JET_LENGTH
      DO COUNT = 1, JET_LENGTH
        WRITE (LUN, 410) COUNT, ENTRY_LIST(COUNT, 0),
     &                   ENTRY_LIST(COUNT, 1)
      END DO
C
  400 FORMAT(' EM  Jet Entry #', I2, ' Eta ', I3, ' Phi ', I2)
  410 FORMAT(' TOT Jet Entry #', I2, ' Eta ', I3, ' Phi ', I2)
C
      WRITE (LUN, *)
      WRITE (LUN, *) 'Level 1.5 Information:    '
      WRITE (LUN, *) 'Cycle Scaler:             ',
     &               L15_CYCLE_SCALER(2), L15_CYCLE_SCALER(1)
      WRITE (LUN, *) 'Potential Scaler:         ',
     &               L15_POTENTIAL_SCALER(2), L15_POTENTIAL_SCALER(1)
      WRITE (LUN, *) 'Skip Scaler:              ',
     &               L15_SKIP_SCALER(2), L15_SKIP_SCALER(1)
      WRITE (LUN, *) 'BEFORE Dead BeamX Scaler: ',
     &               BEFORE_L15_DEAD_BEAMX_SCALER(2),
     &               BEFORE_L15_DEAD_BEAMX_SCALER(1)
      WRITE (LUN, *) 'AFTER  Dead BeamX Scaler: ',
     &               AFTER_L15_DEAD_BEAMX_SCALER(2),
     &               AFTER_L15_DEAD_BEAMX_SCALER(1)
      WRITE (LUN, *) 'BEFORE Pass Scaler:       ',
     &               BEFORE_L15_PASS_SCALER(2),
     &               BEFORE_L15_PASS_SCALER(1)
      WRITE (LUN, *) 'AFTER  Pass Scaler:       ',
     &               AFTER_L15_PASS_SCALER(2),
     &               AFTER_L15_PASS_SCALER(1)
      WRITE (LUN, *) 'BEFORE Fail Scaler:       ',
     &               BEFORE_L15_FAIL_SCALER(2),
     &               BEFORE_L15_FAIL_SCALER(1)
      WRITE (LUN, *) 'AFTER  Fail Scaler:       ',
     &               AFTER_L15_FAIL_SCALER(2),
     &               AFTER_L15_FAIL_SCALER(1)
      WRITE (LUN, *) 'BEFORE Timeout Scaler:    ',
     &               BEFORE_L15_TIMEOUT_SCALER(2),
     &               BEFORE_L15_TIMEOUT_SCALER(1)
      WRITE (LUN, *) 'AFTER  Timeout Scaler:    ',
     &               AFTER_L15_TIMEOUT_SCALER(2),
     &               AFTER_L15_TIMEOUT_SCALER(1)
      WRITE (LUN, *)
C
      WRITE(LUN,*) 'Event Used L15    : ', USED_L15
      WRITE(LUN,*) 'Event Skipped L15 : ', SKIPPED_L15
      WRITE(LUN,*) 'L15 Timed Out     : ', TIMED_OUT
      WRITE(LUN, 310) 'Specific Trigger used L15', ST_USED_L15
      WRITE(LUN, 310) 'Specific Trigger Decision Complete',
     &  ST_L15_ANSWER
      WRITE(LUN, 320) 'L15 Term Done', L15_TERM_DONE
      WRITE(LUN, 320) 'L15 Term Answer', L15_TERM_ANSWER
  310 FORMAT(' ', A, T36, ':', 16L)
  320 FORMAT(' ', A, T15, ':', 32L)
C
  500 FORMAT(' ETA=', SP, I3, SS, '  TOT', 8(' ', F6.2))
  510 FORMAT(' PHI=', I2, ':', I2,  ' EM', 8(' ', F6.2))
      DO MAGN_ETA = 1,2
        DO PHI = 1, 32
          CALL L1EXTRACT_TRGTWR_ADC_ENERGY(IQ(LTRGR_LEVEL1), 
     &                                     -MAGN_ETA, PHI,
     &                                     ADC_EM(-MAGN_ETA, PHI),
     &                                     ADC_TOT(-MAGN_ETA, PHI))
          CALL L1EXTRACT_TRGTWR_ADC_ENERGY(IQ(LTRGR_LEVEL1), 
     &                                     MAGN_ETA, PHI,
     &                                     ADC_EM(MAGN_ETA, PHI),
     &                                     ADC_TOT(MAGN_ETA, PHI))
        END DO
      END DO
C
      DO MAGN_ETA = 2, 1, -1
        DO PHI = 1, 32, 8
          WRITE (LUN, 500) -MAGN_ETA, (ADC_TOT(-MAGN_ETA, PHI+COUNT2),
     &      COUNT2 = 0, 7)
          WRITE (LUN, 510) PHI, PHI+7, (ADC_EM(-MAGN_ETA, PHI+COUNT2),
     &      COUNT2 = 0, 7)
        END DO
      END DO
      DO MAGN_ETA = 1, 2
        DO PHI = 1, 32, 8
          WRITE (LUN, 500) MAGN_ETA, (ADC_TOT(MAGN_ETA, PHI+COUNT2),
     &      COUNT2 = 0, 7)
          WRITE (LUN, 510) PHI, PHI+7, (ADC_EM(MAGN_ETA, PHI+COUNT2),
     &      COUNT2 = 0, 7)
        END DO
      END DO
C
C----------------------------------------------------------------------
      L1DMP_L1EXTRACT = .TRUE.
  999 RETURN
C-----------------------------------------------------------------------
      ENTRY L1DMP_L1EXTRACT_DEFDUMP
C
      L1DMP_L1EXTRACT_DEFDUMP = .TRUE.
      YES = .TRUE.
      CALL GETPAR(1,' Dump from L1EXTRACT* Routines ?[Y]:', 'L',YES)
      CALL OUTMSG(' ')
      RETURN
      END
