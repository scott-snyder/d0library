      SUBROUTINE L1DMP_DBLOCK_UNPACK(LUN,SIMULATION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the items that make up the level 1 data block.
C-      If the TRGR bank is being used as input, it must first be unpacked
C-      into the simulator common blocks.
C-
C-   Inputs  : LUN      The unit number to print to
C-             SIMULATION Whether simulation was performed on the event.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  10-JUL-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  23-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek   
C-                      All Specific Trigger variables use [0,31] for indices
C-                      Added dump of Front End Busy, Front End Busy Disable,
C-                        Level 2 Disable.
C-                      Changed to new output format.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
C
      INTEGER  TRULEN
      EXTERNAL TRULEN
      INTEGER JBYT
      EXTERNAL JBYT
      INTEGER JBIT
      EXTERNAL JBIT
      CHARACTER*20 PRTRGR_SCALER_UNPACK, SPACES
      EXTERNAL PRTRGR_SCALER_UNPACK, SPACES
C
      INTEGER LUN
      LOGICAL SIMULATION
C
      CHARACTER*132 LINE, LINE2
      CHARACTER*20 FIRING_STRING, ENABLE_STRING
      INTEGER TRIGGER, INGROUP, POSITION, REFSET, ANDOR
      INTEGER SIGN_ETA, ETA, PHI, LUQ, BIT, GEO, SPEC_TRIG
      INTEGER ISTAT
C
      INTEGER AO_GROUP_PERLINE
      PARAMETER (AO_GROUP_PERLINE = 8)
C
      INTEGER JET
      INTEGER NUM_JET, JET_ADDRESS, JET_MASK, JET_ADDRESS_2, JET_MASK_2
      LOGICAL JET_COMPLETE
C
      INTEGER BIN_TO_INDEX
      BIN_TO_INDEX(LUQ, POSITION) = LUQ_PAGE_INDEX(LUQ, 
     &  LUQ_PAGE_NUMBER(LUQ, POSITION))
C
C       Specific Trigger Fired
C
C----------------------------------------------------------------------
C
      WRITE(LUN,*)
      WRITE(LUN,*)
      WRITE(LUN,*) 'Specific Trigger Information'
      WRITE(LUN,*) '----------------------------'
      WRITE(LUN,*)
      WRITE(LUN,*)'                 0          111111 11112222 22222233'
      WRITE(LUN,*)'Specific Trigger 01234567 89012345 67890123 45678901'
      WRITE(LUN,*)'                 -----------------------------------'
C
      LINE = ' '
      LINE2 = ' '
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        POSITION = TRIGGER + TRIGGER / BYTE_LENGTH + 1
        IF (ST_LEVEL1_STATE(TRIGGER) .EQV. .TRUE.) THEN
          LINE(POSITION:POSITION) = '1'
        ELSE
          LINE(POSITION:POSITION) = '0'
        ENDIF
        IF (FIRED_TRIGGER(TRIGGER) .EQV. .TRUE.) THEN
          LINE2(POSITION:POSITION) = '1'
        ELSE
          LINE2(POSITION:POSITION) = '0'
        ENDIF
      END DO
      WRITE(LUN,*) 'Level 1 Decision ', LINE(1:TRULEN(LINE))
      WRITE(LUN,*) 'Final Decision   ', LINE2(1:TRULEN(LINE))
C
C       Specific Trigger info table
C
      WRITE(LUN,*)
      LINE = '                                                     ' 
     &  // '    ST Fired         ST Enabled   Front End'
      WRITE(LUN,2110) LINE(1:TRULEN(LINE))
C
      LINE = 'Specific      Firing Count          Enable Count Lvl 1 '
     &  // '   Scaler    Andor   Scaler       Busy    Level 2   Final' 
      IF (SIMULATION .EQV. .TRUE.) THEN
        LINE = LINE(1:TRULEN(LINE)) // '    Is'
      ENDIF
      WRITE (LUN,2110) LINE(1:TRULEN(LINE))
C
      LINE = 'Trigger          Scaler                Scaler    Fired'
     &  // '  Incremented Fired Incremented   Disable  Disable'
     &  // '  Decision' 
      IF (SIMULATION .EQV. .TRUE.) THEN
        LINE = LINE(1:TRULEN(LINE)) // ' L1.5'
      ENDIF
      WRITE (LUN,2110) LINE(1:TRULEN(LINE))
      WRITE (LUN,2120)
C
 2100 FORMAT( I5, 2X, A20, X, A20, L4, SP, I15, L4, I14, L7, L9, L9)
 2110 FORMAT( ' ', A )
 2120 FORMAT(' ', 119('-'))
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        FIRING_STRING = SPACES(
     &    PRTRGR_SCALER_UNPACK(SP_TRG_SCALERS+2*SCALER_L*TRIGGER, 
     &                          LVL1_DATA_BLOCK),  0)
        ENABLE_STRING = SPACES( 
     &    PRTRGR_SCALER_UNPACK(SP_TRG_SCALERS+SCALER_L 
     &      +2*SCALER_L*TRIGGER, LVL1_DATA_BLOCK), 0)
C
        WRITE (LINE, 2100, IOSTAT=ISTAT) TRIGGER, 
     &    FIRING_STRING(1:TRULEN(FIRING_STRING)), 
     &    ENABLE_STRING(1:TRULEN(ENABLE_STRING)),
     &    ST_LEVEL1_STATE(TRIGGER),
     &    FIRED_SCALERS_INCREMENTED(TRIGGER),
     &    FSTD_ANDOR_FIRED(TRIGGER),
     &    ENABLE_SCALERS_INCREMENTED(TRIGGER),
     &    ST_FRONT_END_BUSY_DISABLE(TRIGGER),
     &    ST_L2_DISABLE(TRIGGER),
     &    FIRED_TRIGGER(TRIGGER)
C
        IF (SIMULATION .EQV. .TRUE.) THEN
          IF (ST_IS_L15(TRIGGER) .EQV. .TRUE.) THEN
            LINE = LINE(1:TRULEN(LINE)) // '      T'
          ELSE
            LINE = LINE(1:TRULEN(LINE)) // '      F'
          ENDIF
        ENDIF
C
        WRITE (LUN, 2110) LINE(1:TRULEN(LINE))
      END DO
C
C       Global Results
C
C----------------------------------------------------------------------
C
      WRITE (LUN,*)
      WRITE (LUN,*)
      WRITE (LUN,*) 'Global Results'
      WRITE (LUN,*) '--------------'
      WRITE (LUN,*)
 2400 FORMAT (' Level 0 Good: ', A, T26, SP, 'Level 0 Bin:', I6)
 2410 FORMAT (' Et Lookup Page:', SP, I4, '/', SS, I1, 
     &  T26, '2nd Lookup:', SP, I7, '/', SS, I1, 
     &  T49, 'Px/Py:', SP, I7, '/', SS, I1)
C
      IF (LEVEL_0_OK .EQV. .TRUE.) THEN
        WRITE (LUN, 2400, IOSTAT=ISTAT) 'Yes', LEVEL_0_NZ
      ELSE
        WRITE (LUN, 2400, IOSTAT=ISTAT) 'No', LEVEL_0_NZ      
      ENDIF
C
      WRITE (LUN, 2410, IOSTAT = ISTAT) 
     &  LUQ_PAGE_NUMBER(EM_ET_QUANT, LEVEL_0_NZ),
     &  BIN_TO_INDEX(EM_ET_QUANT, LEVEL_0_NZ),
     &  LUQ_PAGE_NUMBER(EM_L2_QUANT, LEVEL_0_NZ),
     &  BIN_TO_INDEX(EM_L2_QUANT, LEVEL_0_NZ),
     &  LUQ_PAGE_NUMBER(PX_QUANT, LEVEL_0_NZ),
     &  BIN_TO_INDEX(PX_QUANT, LEVEL_0_NZ)
C
C       Global Energies
C       
C----------------------------------------------------------------------
C
      WRITE (LUN,*)
      WRITE (LUN,*)
      WRITE (LUN,*) 'Global Energy Sums'
      WRITE (LUN,*) '------------------'
      WRITE (LUN,*)
      WRITE (LUN,*) '             counts        GeVs'
      WRITE (LUN,*) '-----------------------------------'
      WRITE (LUN,*)
 2500 FORMAT (' ', A, T9, I11, F12.2, ' GeV')
      WRITE (LUN,2500,IOSTAT=ISTAT) 'EM  Et',
     &            GLOBAL_ENERGY(GL_EMET_THRTYP),
     &            FLOAT(GLOBAL_ENERGY(GL_EMET_THRTYP))
     &              * GLOBAL_ENERGY_SCALE(EM_ET_QUANT)
      WRITE (LUN,2500,IOSTAT=ISTAT) 'HD  Et ',
     &            GLOBAL_ENERGY(GL_HDET_THRTYP),
     &            FLOAT(GLOBAL_ENERGY(GL_HDET_THRTYP))
     &              * GLOBAL_ENERGY_SCALE(HD_ET_QUANT)
      WRITE (LUN,2500,IOSTAT=ISTAT) 'TOT Et ',
     &            GLOBAL_ENERGY(GL_TOTET_THRTYP),
     &            FLOAT(GLOBAL_ENERGY(GL_TOTET_THRTYP))
     &              * GLOBAL_ENERGY_SCALE(TOT_ET_QUANT)
C
      WRITE (LUN,2500,IOSTAT=ISTAT) 'Px     ', TOTAL_PX,
     &            FLOAT(TOTAL_PX) * GLOBAL_ENERGY_SCALE(PX_QUANT)
      WRITE (LUN,2500,IOSTAT=ISTAT) 'Py     ', TOTAL_PY,
     &            FLOAT(TOTAL_PY) * GLOBAL_ENERGY_SCALE(PY_QUANT)
      WRITE (LUN,2500,IOSTAT=ISTAT) 'Mis Pt ', TOTAL_MPT,
     &            FLOAT(TOTAL_MPT) * GLOBAL_ENERGY_SCALE(PX_QUANT)
C
      WRITE (LUN,2500,IOSTAT=ISTAT) '2nd EM ',
     &            GLOBAL_ENERGY(GL_EML2_THRTYP),
     &            FLOAT(GLOBAL_ENERGY(GL_EML2_THRTYP))
     &              * GLOBAL_ENERGY_SCALE(EM_L2_QUANT)
      WRITE (LUN,2500,IOSTAT=ISTAT) '2nd HD ',
     &            GLOBAL_ENERGY(GL_HDL2_THRTYP),
     &            FLOAT(GLOBAL_ENERGY(GL_HDL2_THRTYP))
     &              * GLOBAL_ENERGY_SCALE(HD_L2_QUANT)
      WRITE (LUN,2500,IOSTAT=ISTAT) 'TOT 2nd',
     &            GLOBAL_ENERGY(GL_TOTL2_THRTYP),
     &            FLOAT(GLOBAL_ENERGY(GL_TOTL2_THRTYP))
     &              * GLOBAL_ENERGY_SCALE(TOT_L2_QUANT)
C
      WRITE (LUN,*)
      WRITE (LUN,*)
 2510 FORMAT( ' ', A6, ' Reference Set Final Count', 
     &               4('    #', I1, ':', I4) )
      WRITE (LUN, 2510, IOSTAT=ISTAT) 'EM  Et', 
     &  (REFSET-EM_ET_REF_MIN, HOT_TOWER_COUNT(REFSET),
     &     REFSET = EM_ET_REF_MIN,EM_ET_REF_MAX)
      WRITE (LUN, 2510, IOSTAT=ISTAT) 'TOT Et',
     &  (REFSET-TOT_ET_REF_MIN, HOT_TOWER_COUNT(REFSET),
     &     REFSET = TOT_ET_REF_MIN, TOT_ET_REF_MAX)
C
C
C----------------------------------------------------------------------
  999 RETURN
      END
