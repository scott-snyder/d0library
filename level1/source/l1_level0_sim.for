      SUBROUTINE L1_LEVEL0_SIM(NUM_ANDOR_USED, ANDOR_STATES, 
     &  ANDOR_INDICES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the state of the LEVEL_0_GOOD signal, and
C-   which ANDOR term it corresponds to. 
C-
C-   Andor Terms Returned:
C-
C-  Level 0 Fast Vertex Good            L0_FAST_Z_GOOD
C-  Level 0 Fast Vertex Segment 
C-    Vertex Within lookup page 0       L0_CENTER_1 or L1C_LKP_CENTER_1
C-    Vertex Within lookup page -1..+1  L1C_LKP_CENTER_3
C-    Vertex Within lookup page -2..+2  L1C_LKP_CENTER_5
C-    Vertex Within lookup page -3..+3  L1C_LKP_CENTER_7
C-  Level 0 Fast Vertex Sign 
C-    Vertex Within lookup page -3..-1  L1C_LKP_NEG_Z
C-  Level 0 Fast Vertex Center Bin      L0_FAST_Z_CENTER
C-
C-   Inputs  : none
C-   Outputs : NUM_ANDOR_USED   The number of Andor Terms being returned
C-             ANDOR_STATES     The states of the returned Andor Terms
C-             ANDOR_INDICES    The hardware index of each returned Andor
C-                                Term
C-   Controls: none
C-
C-   Created  14-AUG-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   7-NOV-1991   Philippe Laurens, Steven Klocek  
C-                      Added support for lookup page Andor Terms. 
C-   Updated   3-MAR-1992   Philippe Laurens, Steven Klocek   
C-           extract Level 0 Vertex from TRGR bank when chosen as input
C-           Set Level 0 Vertex position to 0 if FORCE_VERTEX_CENTER is set 
C-   Updated   2-DEC-1992   Philippe Laurens, Steven Klocek   
C-           Use new Andor Term names 'L0_FAST_Z_GOOD' and 'L0_CENTER_1' 
C-           New scaler Level 0 Good
C-           Return "center L0 bin" Andor Term
C-           Andor Term names L0_CENTER_1 and L1C_LKP_CENTER_1 interchangable
C-           Simulate new Andor Terms 'L0_SLOW_Z_GOOD' and 'L0_SLOW_Z_CENTER'.
C-             L0_SLOW_Z_GOOD   True if vertex |Z| < 100.5cm
C-             L0_SLOW_Z_CENTER True if vertex |Z| <  10.5cm
C-   Updated  27-APR-1993   Philippe LAURENS, MICHIGAN STATE UNIVERSITY   
C-           Fix bug in L1C_LKP_NEG_Z (was wrong for bin -1) 
C-                  and L0_FAST_Z_CENTER ( was always set TRUE)
C-   Updated  11-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-           Amber pointed out that the Andor Terms should not be forced to
C-           display a Vertex at z=0 when the Calorimeter Trigger Lookup System
C-           is told to ignore Level 0 Vertex information (FORCE_VERTEX_CENTER
C-           in L1SIM_RCP). 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      LOGICAL  L1UTIL_PICK_RESOURCE_RCP
      EXTERNAL L1UTIL_PICK_RESOURCE_RCP
C
      INTEGER NUM_ANDOR_USED
      LOGICAL ANDOR_STATES(1:NUM_ANDOR)
      INTEGER ANDOR_INDICES(1:NUM_ANDOR)
C
      INTEGER L0_FAST_Z_GOOD_TERM_NUM
      INTEGER L1C_LKP1_TERM_NUM
      INTEGER L1C_LKP3_TERM_NUM
      INTEGER L1C_LKP5_TERM_NUM
      INTEGER L1C_LKP7_TERM_NUM
      INTEGER L1C_LKPN_TERM_NUM
      INTEGER L0_FAST_Z_CENTER_NUM
C
      INTEGER L0_SLOW_Z_GOOD_TERM_NUM
      LOGICAL L0_SLOW_Z_GOOD_STATE
      INTEGER L0_SLOW_Z_CENTER_TERM_NUM
      LOGICAL L0_SLOW_Z_CENTER_STATE
C
      INTEGER   ZI_L0_VERTEX, GZTRGR, GZFIND_CRATE, LTRGR_LEVEL1
      INTEGER JBIT
      INTEGER L0_SIGN_BIT
      PARAMETER (L0_SIGN_BIT = 5)
      INTEGER L0_GOOD_BIT
      PARAMETER (L0_GOOD_BIT = 6)
C
      REAL L0_SLOW_Z_GOOD_WIDTH
      PARAMETER ( L0_SLOW_Z_GOOD_WIDTH = 100.5 )
      REAL L0_SLOW_Z_CENTER_WIDTH
      PARAMETER ( L0_SLOW_Z_CENTER_WIDTH = 10.5 )
C
      INTEGER TERM_NUM
      INTEGER PAGE
      INTEGER IER
      INTEGER RCP_OK
      PARAMETER (RCP_OK = 0)
      INTEGER OUT_OF_RANGE
      PARAMETER (OUT_OF_RANGE = 1000)
C
      LOGICAL FIRST
      SAVE FIRST, L0_FAST_Z_GOOD_TERM_NUM, L1C_LKP1_TERM_NUM,
     &  L1C_LKP3_TERM_NUM, L1C_LKP5_TERM_NUM, L1C_LKP7_TERM_NUM,
     &  L1C_LKPN_TERM_NUM, L0_SLOW_Z_GOOD_TERM_NUM, 
     &  L0_SLOW_Z_CENTER_TERM_NUM
      DATA FIRST / .TRUE. /
C
      NUM_ANDOR_USED = 0
      IF (FIRST .EQV. .TRUE.) THEN
        IF (L1UTIL_PICK_RESOURCE_RCP() .EQV. .FALSE.) GOTO 999
C       
C       Initialize the Andor Term numbers
C
        CALL EZGET('L0_FAST_Z_GOOD', L0_FAST_Z_GOOD_TERM_NUM, IER)
        IF (IER .NE. RCP_OK) THEN
          L0_FAST_Z_GOOD_TERM_NUM = OUT_OF_RANGE
        ENDIF
C
        CALL EZGET('L1C_LKP_CENTER_1', L1C_LKP1_TERM_NUM, IER)
        IF (IER .NE. RCP_OK) THEN
          L1C_LKP1_TERM_NUM = OUT_OF_RANGE
        ENDIF
        CALL EZGET('L0_CENTER_1', TERM_NUM, IER)
        IF (IER .EQ. RCP_OK) THEN
          L1C_LKP1_TERM_NUM = TERM_NUM
        ENDIF
C
        CALL EZGET('L1C_LKP_CENTER_3', L1C_LKP3_TERM_NUM, IER)
        IF (IER .NE. RCP_OK) THEN
          L1C_LKP3_TERM_NUM = OUT_OF_RANGE
        ENDIF
C
        CALL EZGET('L1C_LKP_CENTER_5', L1C_LKP5_TERM_NUM, IER)
        IF (IER .NE. RCP_OK) THEN
          L1C_LKP5_TERM_NUM = OUT_OF_RANGE
        ENDIF
C
        CALL EZGET('L1C_LKP_CENTER_7', L1C_LKP7_TERM_NUM, IER)
        IF (IER .NE. RCP_OK) THEN
          L1C_LKP7_TERM_NUM = OUT_OF_RANGE
        ENDIF
C
        CALL EZGET('L1C_LKP_NEG_Z', L1C_LKPN_TERM_NUM, IER)
        IF (IER .NE. RCP_OK) THEN
          L1C_LKPN_TERM_NUM = OUT_OF_RANGE
        ENDIF
C
        CALL EZGET('L0_FAST_Z_CENTER', L0_FAST_Z_CENTER_NUM, IER)
        IF (IER .NE. RCP_OK) THEN
          L0_FAST_Z_CENTER_NUM = OUT_OF_RANGE
        ENDIF
C
        CALL EZGET('L0_SLOW_Z_GOOD', L0_SLOW_Z_GOOD_TERM_NUM, IER)
        IF (IER .NE. RCP_OK) L0_SLOW_Z_GOOD_TERM_NUM = OUT_OF_RANGE
C
        CALL EZGET('L0_SLOW_Z_CENTER', L0_SLOW_Z_CENTER_TERM_NUM, IER)
        IF (IER .NE. RCP_OK) L0_SLOW_Z_CENTER_TERM_NUM = OUT_OF_RANGE
C
        CALL EZRSET()
        FIRST = .FALSE.
      ENDIF
C
C       now find level 0 vertex bin number
C
      IF ( L1SIM_INPUT_SOURCE .EQ. 'TRGR' ) THEN 
C
C       Find the Level 1 data in the TRGR bank
        LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), CRATE_ID ) 
        IF ( LTRGR_LEVEL1 .LE. 0 ) CALL ERRMSG ( 
     &    'No L1 data', 'L1_LEVEL0_SIM', 'cant find level 1 crate', 'F')
C
C       Prepare the index to the level 0 vertex 
        ZI_L0_VERTEX = LTRGR_LEVEL1 + TRGR_HEADER_LENGTH 
     &                  + (FAST_VERTEX+1)/2
C
C       Start with reading and extending the sign of the 5 bit number 
        LEVEL_0_NZ = - JBIT( IQ(ZI_L0_VERTEX), L0_SIGN_BIT ) 
C
C       Now read the magnitude
        CALL CBYT( IQ(ZI_L0_VERTEX), 1, LEVEL_0_NZ, 1, L0_SIGN_BIT-1 ) 
C
C       Now read the "good" bit
        IF ( JBIT( IQ(ZI_L0_VERTEX), L0_GOOD_BIT ) .EQ. 0 ) THEN 
          LEVEL_0_OK = .FALSE.
        ELSE 
          LEVEL_0_OK = .TRUE.
        ENDIF
C
C       Now read the state of the L0_SLOW_Z_GOOD Andor Term 
        IF (L0_SLOW_Z_GOOD_TERM_NUM .NE. OUT_OF_RANGE) THEN
          CALL L1EXTRACT_ANDOR_TERM(IQ(LTRGR_LEVEL1), 
     &      L0_SLOW_Z_GOOD_TERM_NUM, L0_SLOW_Z_GOOD_STATE)
        ENDIF
C
        IF (L0_SLOW_Z_CENTER_TERM_NUM .NE. OUT_OF_RANGE) THEN
          CALL L1EXTRACT_ANDOR_TERM(IQ(LTRGR_LEVEL1), 
     &      L0_SLOW_Z_CENTER_TERM_NUM, L0_SLOW_Z_CENTER_STATE)
        ENDIF
C
      ELSE 
C
C       use the isajet info to make simple geometric simulation of LEVEL 0
        CALL SIMUL_LEVEL0(Z_VERTEX, LEVEL_0_NZ, LEVEL_0_OK)
C
        L0_SLOW_Z_GOOD_STATE = .FALSE.
        IF (ABS(Z_VERTEX) .LT. L0_SLOW_Z_GOOD_WIDTH) THEN
          L0_SLOW_Z_GOOD_STATE = .TRUE.
        ENDIF
C
        L0_SLOW_Z_CENTER_STATE = .FALSE.
        IF (ABS(Z_VERTEX) .LT. L0_SLOW_Z_CENTER_WIDTH) THEN
          L0_SLOW_Z_CENTER_STATE = .TRUE.
        ENDIF
C
      END IF
C---------- Removed 12-JUL-1993: Do Not override the Andor Terms, 
C                                The Calorimete Trigger Lookup will still be
C                                forced to center page because LSM was
C                                initialized as showing that all Level 0 Bins
C                                point to the center page (cf. L1C_INIT_LSM)
C       Force Level 0 Vertex information to center bin if requested
C
C      IF ( FORCE_VERTEX_CENTER .EQV. .TRUE. ) THEN 
C        LEVEL_0_NZ = 0 
C        LEVEL_0_OK = .TRUE.
C        L0_SLOW_Z_GOOD_STATE = .TRUE.
C        L0_SLOW_Z_CENTER_STATE = .TRUE.
C      END IF 
C---------- 
C
C       Get Level 0 Good state and increment scaler
C
      IF (LEVEL_0_OK .EQV. .TRUE.) THEN
        L0_FASTZ_GOOD_SCALER(1) = L0_FASTZ_GOOD_SCALER(1) + 1
        L0_FASTZ_GOOD_INCREMENTED = 1
      ELSE
        L0_FASTZ_GOOD_INCREMENTED = 0
      ENDIF
C      
      IF (L0_FAST_Z_GOOD_TERM_NUM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = NUM_ANDOR_USED + 1
        ANDOR_STATES(NUM_ANDOR_USED) = LEVEL_0_OK
        ANDOR_INDICES(NUM_ANDOR_USED) = L0_FAST_Z_GOOD_TERM_NUM
      ENDIF
C
      PAGE = LUQ_PAGE_NUMBER( PX_QUANT, LEVEL_0_NZ)
C
C       Is it in the center page
      IF (L1C_LKP1_TERM_NUM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = NUM_ANDOR_USED + 1
        ANDOR_INDICES(NUM_ANDOR_USED) = L1C_LKP1_TERM_NUM
        IF (PAGE .EQ. 0) THEN
          ANDOR_STATES(NUM_ANDOR_USED) = .TRUE.
        ELSE
          ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
        ENDIF
      ENDIF
C
C       Is it in the 3 center pages
      IF (L1C_LKP3_TERM_NUM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = NUM_ANDOR_USED + 1
        ANDOR_INDICES(NUM_ANDOR_USED) = L1C_LKP3_TERM_NUM
        IF (ABS(PAGE) .LE. 1) THEN
          ANDOR_STATES(NUM_ANDOR_USED) = .TRUE.
        ELSE
          ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
        ENDIF
      ENDIF
C
C       Is it in the 5 center pages
      IF (L1C_LKP5_TERM_NUM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = NUM_ANDOR_USED + 1
        ANDOR_INDICES(NUM_ANDOR_USED) = L1C_LKP5_TERM_NUM
        IF (ABS(PAGE) .LE. 2) THEN
          ANDOR_STATES(NUM_ANDOR_USED) = .TRUE.
        ELSE
          ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
        ENDIF
      ENDIF
C
C       Is it in the 7 center pages
      IF (L1C_LKP7_TERM_NUM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = NUM_ANDOR_USED + 1
        ANDOR_INDICES(NUM_ANDOR_USED) = L1C_LKP7_TERM_NUM
        IF (ABS(PAGE) .LE. 3) THEN
          ANDOR_STATES(NUM_ANDOR_USED) = .TRUE.
        ELSE
          ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
        ENDIF
      ENDIF
C
C       Is the page number negative
      IF (L1C_LKPN_TERM_NUM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = NUM_ANDOR_USED + 1
        ANDOR_INDICES(NUM_ANDOR_USED) = L1C_LKPN_TERM_NUM
        IF ( LEVEL_0_NZ .LT. 0 ) THEN
          ANDOR_STATES(NUM_ANDOR_USED) = .TRUE.
        ELSE
          ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
        ENDIF
      ENDIF
C
C       Was the event in the center Level 0 bin
      IF (L0_FAST_Z_CENTER_NUM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = NUM_ANDOR_USED + 1
        ANDOR_INDICES(NUM_ANDOR_USED) = L0_FAST_Z_CENTER_NUM
        IF (LEVEL_0_NZ .EQ. 0) THEN
          ANDOR_STATES(NUM_ANDOR_USED) = .TRUE.
        ELSE
          ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
        ENDIF
      ENDIF
C
C       Does the event meet the requirements of the Slow Level 0
      IF (L0_SLOW_Z_GOOD_TERM_NUM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = NUM_ANDOR_USED + 1
        ANDOR_INDICES(NUM_ANDOR_USED) = L0_SLOW_Z_GOOD_TERM_NUM
        ANDOR_STATES(NUM_ANDOR_USED) =  L0_SLOW_Z_GOOD_STATE
      ENDIF
C
      IF (L0_SLOW_Z_CENTER_TERM_NUM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = NUM_ANDOR_USED + 1
        ANDOR_INDICES(NUM_ANDOR_USED) = L0_SLOW_Z_CENTER_TERM_NUM
        ANDOR_STATES(NUM_ANDOR_USED) =  L0_SLOW_Z_CENTER_STATE
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
