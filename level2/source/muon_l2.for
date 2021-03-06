      SUBROUTINE MUON_L2
     &         (PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RESULT_FLAG is .TRUE. if NTRACKS .GE.
C-                         MIN_MUON_TRACKS AND AT LEAST 1 OF THE TRACKS
C-                         HAS GREATER THAN MINIMUM MOMENTUM
C-
C-   Inputs  : PARAM_SET_NUMBER : Number in series of parameters to use.
C-             HARDWARE : Mask with bit set for Level-1 trigger which
C-                        started this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when TOOL wants event passed
C-             EXTRA_FLAG :  Not used
C-   Controls: None
C-
C-   Created   9-NOV-90   by the L2STATE program
C-            27-NOV-90   Present form by Diehl USED IN CRC I
C-            04-AUG-91   Form for CRC II or PBARP        HTD
C-            30-SEP-91   CHECKS IF ALREADY CALLED FOR EVENT.
C-            23-NOV-91   PARAMS NOW CONFORM WITH STANDARD (HTD)
C-            12-DEC-91   Put muons in FSUM bank (JTL)
C-             3-JAN-91   REQUIRE 2nd Flag word = 0 as per Hedin (JTL)
C-             7-JAN-92   Fix MUOT loop. also add ABS(MOM) (DH)
C-             8-JAN-92   James T. Linnemann   FSUM -> ESUM
C-             3-MAR-92   Add TRACK_IN_ROAD parameter (HTD)
C-            11-APR-92   Use header words 7+8 to check event num.(HTD)
C-             1-JUN-92   use IFW4 DH
C-            25-JUN-92   Add MUON_QUALITY parameter (HTD)
C-            25-JUN-92   ESUM stores ETAMU_PHYSICS and ETAMU_DETECTOR (HTD)
C-            01-JUL-92   Change TRACK_IN_ROAD from Logical to Char (HTD)
C-            21-SEP-92   Tight Cuts include cosmic id and IFW4=0 (HTD)
C-            25-SEP-92   COMMON cuts include cosmic id and IFW4<2 (HTD)
C-            08-DEC-92   Include SAMUS_L2 as subroutine and subtool (HTD)
C-            15-DEC-92   Eta cut now refers to "eta region"   (HTD)
C-            22-DEC-92   Increment track_quality based on plane count EF. (HTD)
C-            25-APR-93   SA-SB-WC Tracking available. Switchable.  (HTD)
C-            18-NOV-93   Modify to allow additional parameters (PZQ)
C-            18_NOV-93   Modify to check on calorimeter energy (Bald)
C-            25-APR-94   Modify and rewrite for V3.00_0.03 (RM)
C-	       8-JUN-94   Modify to restore single cal_confirm call (PZQ)
C-	      24-JUN-94   Modify to track for MUCTAG at the end (PZQ)
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:MUON_L2_PARAMS.INC'
      INCLUDE 'D0$INC:MUON_L2_PARAMS_C.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'

      INTEGER PARAM_SET_NUMBER,HARDWARE
      INTEGER LMUD1,GZMUD1,MUD1_SIZE,MAX_MUD1_L2
      INTEGER LMUOT, GZMUOT
      INTEGER EVENT_NUM_LOW,EVENT_NUM_HIGH
      INTEGER LAST_EVENT_NUM_LOW,LAST_EVENT_NUM_HIGH
      INTEGER SKIP_LEVEL, TRIG_LEVEL, DET_REGION, I
      INTEGER MIN_TRIG_LEVEL(4),NTRACKS,IER
      INTEGER NWAMUS,NSAMUS,QUAD,IFW1,IFW2,IFW3,IFW4                  !muot
      INTEGER COUNT,TRACK_QUALITY,REQ_QUALITY

      REAL ETAMU,ABSETAMU,PTMU,PX,PY,PZ,THETA,PHIMU
      REAL XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM         !muot
      REAL XCOSOM,YCOSOM,ZCOSOM,CHSNBV,CHSQBV,MOM,MOMER,ELCAL,ELFE !muot
      REAL SPARE1,SPARE2                                           !muot
      REAL DIR(3),VTX(3),L2_VERT,ROOTSUMSQ,L2CAL

      LOGICAL RESULT_FLAG,EXTRA_FLAG
      LOGICAL FIRST, DO_TRACKING, OVERLAP, WAMUS, SAMUS
      LOGICAL T0_CONFIRM, SCINT_CONFIRM, CAL_CONFIRM, CD_CONFIRM
      LOGICAL COSMIC_ID, CAL_OK
	
      INTEGER MAX_TRACKS
      PARAMETER (MAX_TRACKS=100)
      INTEGER CAL_CHECKED(MAX_TRACKS)

      SAVE LAST_EVENT_NUM_LOW, LAST_EVENT_NUM_HIGH
      SAVE SKIP_LEVEL, MUD1_SIZE, MAX_MUD1_L2
      SAVE CAL_CHECKED, MIN_TRIG_LEVEL

      DATA LAST_EVENT_NUM_LOW / 0 /, LAST_EVENT_NUM_HIGH / 0 /
      DATA FIRST / .TRUE. /

C----------------------------------------------------------------------

C---- GET RCP PARAMETER SKIP_LEVEL.  DO THIS ONLY ONCE

      IF (FIRST) THEN
        CALL EZPICK('MUON_UTIL_L2_PARAMS')
        CALL EZGET('SKIP_LEVEL',SKIP_LEVEL,IER)
        CALL EZRSET
        CALL EZPICK('MUON_L2_CONTROL')
        CALL EZGET('MAX_MUD1_L2',MAX_MUD1_L2,IER)
        IF (IER .NE. 0) MAX_MUD1_L2 = 99999
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF

      RESULT_FLAG = .FALSE.            ! TURNS TRUE IF EVENT PASSES
      EXTRA_FLAG = .FALSE.

C---- INITIALIZE MIN_TRIG_LEVEL AND GET MUD1 SIZE. DO ONCE PER EVENT

      EVENT_NUM_LOW = IQ(LHEAD+7)
      EVENT_NUM_HIGH = IQ(LHEAD+8)
      IF(EVENT_NUM_LOW.NE.LAST_EVENT_NUM_LOW.OR.
     &   EVENT_NUM_HIGH.NE.LAST_EVENT_NUM_HIGH) THEN
        LAST_EVENT_NUM_LOW = EVENT_NUM_LOW
        LAST_EVENT_NUM_HIGH = EVENT_NUM_HIGH
        DO I = 1, 4
          MIN_TRIG_LEVEL(I) = 999
        ENDDO
        DO I = 1, MAX_TRACKS
	  CAL_CHECKED(I) = 0
        ENDDO
C
        LMUD1 = GZMUD1()
        IF (LMUD1.GT.0) THEN
          MUD1_SIZE = IQ(LMUD1-1)
          IF (MUD1_SIZE .GT. MAX_MUD1_L2) THEN
            CALL ERRMSG('MAX_MUD1_L2 EXCEEDED',
     &        'L2_MUON','Filter Passed ','W')
            RESULT_FLAG = .TRUE.
            RETURN
          ENDIF
        ENDIF

      ENDIF

C---- CHECK MUD1 SIZE EACH TIME THIS ROUTINE IS CALLED

      IF (MUD1_SIZE.GT.MAX_MUD1_L2) THEN
        RESULT_FLAG = .TRUE.
        RETURN
      ENDIF

      IF(PARAM_SET_NUMBER.LE.0.OR.PARAM_SET_NUMBER.GT.NUMBER_OF_SETS)
     &  CALL ERRMSG('MUON_L2 PARAM SET NUMBER','L2_MUON',' ','F')

C---- BELOW ARE THE NEW MUON PARAMETERS INTRODUCED IN RUN 1B

      COSMIC_ID = COSMIC_REJECT(PARAM_SET_NUMBER)
      T0_CONFIRM = REFIT_T0(PARAM_SET_NUMBER)
      SCINT_CONFIRM = SCINT_ON_TRACK(PARAM_SET_NUMBER)
      CAL_CONFIRM = CAL_ON_TRACK(PARAM_SET_NUMBER)
      CD_CONFIRM = CD_ON_TRACK(PARAM_SET_NUMBER)

C---- BELOW ARE THE MUON_QUALITIES USED IN RUN 1A

      IF (MUON_QUALITY(PARAM_SET_NUMBER).EQ.'TIGHT') THEN
        REQ_QUALITY  = 1
        COSMIC_ID = .TRUE.
      ELSEIF(MUON_QUALITY(PARAM_SET_NUMBER).EQ.'COMMON') THEN
        REQ_QUALITY = 2
        COSMIC_ID = .TRUE.
      ELSEIF(MUON_QUALITY(PARAM_SET_NUMBER).EQ.'TYPICAL') THEN
        REQ_QUALITY = 1
        COSMIC_ID = .FALSE.
      ELSEIF(MUON_QUALITY(PARAM_SET_NUMBER).EQ.'LOOSE') THEN
        REQ_QUALITY = 2
        COSMIC_ID = .FALSE.
      ELSEIF(MUON_QUALITY(PARAM_SET_NUMBER).EQ.'MUOT') THEN
        REQ_QUALITY = 100
        COSMIC_ID = .FALSE.

C---- BELOW ARE THE MUON_QUALITIES USED IN RUN 1B

      ELSEIF(MUON_QUALITY(PARAM_SET_NUMBER).EQ.'BEST') THEN
        REQ_QUALITY = 1
      ELSEIF(MUON_QUALITY(PARAM_SET_NUMBER).EQ.'GOOD') THEN
        REQ_QUALITY = 2
      ELSEIF(MUON_QUALITY(PARAM_SET_NUMBER).EQ.'IGNORE') THEN
        REQ_QUALITY = 100
      ENDIF

      CALL MUON_L2_TIR_PARSE(PARAM_SET_NUMBER,TRIG_LEVEL,DET_REGION)

C---- TRACKING WILL BE DONE ONLY IF IT HAS NOT ALREADY BEEN DONE FOR THIS
C---- REGION AT AN EQUAL OR LOWER TRIGGER LEVEL

      DO_TRACKING = .FALSE.
      IF (DET_REGION .GT. 0) THEN
        DO I = 1, DET_REGION
          IF (TRIG_LEVEL .LT. MIN_TRIG_LEVEL(I)) THEN
            MIN_TRIG_LEVEL(I) = TRIG_LEVEL
            DO_TRACKING = .TRUE.
          ENDIF
        ENDDO
      ELSE
        DO I = -DET_REGION, 4
          IF (TRIG_LEVEL .LT. MIN_TRIG_LEVEL(I)) THEN
            MIN_TRIG_LEVEL(I) = TRIG_LEVEL
            DO_TRACKING = .TRUE.
          ENDIF
        ENDDO
      ENDIF

      IF (DO_TRACKING) THEN
        CALL EZPICK('MUON_UTIL_L2_PARAMS')
        CALL MUANLZ(IER,SKIP_LEVEL,TRIG_LEVEL,DET_REGION)
        CALL EZRSET
        IF (IER .LT. 0) THEN    ! MUANLZ FAILED; EVENT REJECTED
          CALL ERRMSG('BAD ERROR FROM MUANLZ',
     &        'L2_MUON','Filter Failed','W')
          RESULT_FLAG = .FALSE.
          RETURN
        ENDIF
      ENDIF

      CALL GTMTRH(NTRACKS)

C---- LOOP OVER TRACKS AND DECIDE RESULTS OF A FILTER

      COUNT = 0                 ! THIS IS THE IMPORTANT NUMBER

      DO 100 I = 1, NTRACKS

        CALL GTMUOT(I,NWAMUS,NSAMUS,QUAD,IFW1,IFW2,IFW3,IFW4,
     &    XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     &    YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,
     &    SPARE2)

        PX = ABS(MOM)*XCOSIM
        PY = ABS(MOM)*YCOSIM
        PZ = ABS(MOM)*ZCOSIM
        PTMU = SQRT(PX**2.+PY**2.)

C------ CUT ON  PT

        IF (PTMU .LT. PTMIN(PARAM_SET_NUMBER)) GOTO 100


C------ CUT ON  ETA

        THETA = ATAN2(PTMU,PZ)
        ETAMU = -ALOG(TAN(THETA/2.0))
        ABSETAMU = ABS(ETAMU)

        IF (ABSETAMU .GT. ABS_ETA_MAX(PARAM_SET_NUMBER)) GOTO 100

        PHIMU = ATAN2(PY,PX)
        IF (PHIMU.LT.0) PHIMU = PHIMU + TWOPI

C------ CUT ON TRACK QUALITY

        IF (IFW4 .EQ. 0) THEN
          TRACK_QUALITY = 1
        ELSEIF (IFW4 .EQ. 1) THEN
          TRACK_QUALITY = 2
        ELSE
          TRACK_QUALITY = 3
        ENDIF
        IF (TRACK_QUALITY .GT. REQ_QUALITY) GOTO 100

C------ VERIFY THAT TRACK IS IN THE DETECTOR REGION REQUESTED

	OVERLAP = (NWAMUS.GT.0) .AND. (NSAMUS.GT.0)
	WAMUS   = (QUAD.LE.12) .AND. (.NOT.OVERLAP)
	SAMUS   = (QUAD.EQ.13)  .OR.  (QUAD.EQ.14)

        IF (DET_REGION .GT. 0) THEN
          IF ((DET_REGION .EQ. 1 .AND. QUAD .GT. 4) .OR. 
     &        (DET_REGION .EQ. 2 .AND. .NOT.WAMUS)  .OR.
     &        (DET_REGION .EQ. 3 .AND. SAMUS))
     &      GOTO 100
        ELSE
          IF ((DET_REGION .EQ. -1 .AND. QUAD .LE. 4) .OR.
     &        (DET_REGION .EQ. -2 .AND. WAMUS)       .OR.
     &        (DET_REGION .EQ. -3 .AND. .NOT.SAMUS))
     &      GOTO 100
        ENDIF

C------ VERIFY THAT TRACK IS CONSISTENT WITH TRIGGER LEVEL

        IF ((TRIG_LEVEL .EQ. 1 .AND. .NOT. BTEST(IFW3,16)) .OR.
     &    (TRIG_LEVEL .EQ. 2 .AND. .NOT. BTEST(IFW3,18)) .OR.
     &    (TRIG_LEVEL .EQ. 3 .AND. .NOT. BTEST(IFW3,19))) GOTO 100

C------ COSMIC REJECTION USING SCINTILLATOR BITS

        IF (SCINT_CONFIRM .AND. QUAD.LE.4) THEN

C------ REJECT IF TRACK POINTS TO SCINTILLATOR AND NO SCINT. HIT FOUND 

          IF (BTEST(IFW2,16) .AND. .NOT. BTEST(IFW2,17)) GOTO 100

	ENDIF

C------ COSMIC REJECTION USING MUCTAG BITS

        IF (COSMIC_ID .AND. QUAD.LE.4) THEN

C------ DO TRACKING IN CENTRAL REGION IN ORDER FOR MUCTAG TO WORK

	  IF (MIN_TRIG_LEVEL(1).GT.0) THEN
	    MIN_TRIG_LEVEL(1) = 0
	    CALL EZPICK('MUON_UTIL_L2_PARAMS')
	    CALL MUANLZ(IER,SKIP_LEVEL,0,1)
	    CALL EZRSET
	  ENDIF
	  CALL MUCTAG(I)
	  LMUOT = GZMUOT(I)
	  IF (LMUOT.GT.0) THEN
	    IFW2 = IQ(LMUOT+5)
	  ENDIF

C------ APPLY BACK-TO-BACK AND OCTANT CROSSING CUTS

          IF (BTEST(IFW2,6) .OR. BTEST(IFW2,7) .OR. BTEST(IFW2,8))
     &      GOTO 100

        ENDIF

C------ DO CALORIMETERY CONFIRMATION IF DESIRED

        IF (CAL_CONFIRM) THEN

	 IF (CAL_CHECKED(I).EQ.0) THEN			! 0 means not checked

C-------- THE FOLLOWING IS FOR WAMUS AND OVERLAP TRACKS

          IF (QUAD .NE. 13 .AND. QUAD .NE. 14) THEN
            VTX(1) = 0
            VTX(2) = 0
            VTX(3) = L2_VERT()
            ROOTSUMSQ = SQRT(XMAGC*XMAGC+YMAGC*YMAGC+
     &        (ZMAGC-VTX(3))*(ZMAGC-VTX(3)))
            IF (ROOTSUMSQ .GT. 0) THEN
              DIR(1) = XMAGC/ROOTSUMSQ
              DIR(2) = YMAGC/ROOTSUMSQ
              DIR(3) = (ZMAGC-VTX(3))/ROOTSUMSQ
            ELSE
              DIR(1) = XCOSIM
              DIR(2) = YCOSIM
              DIR(3) = ZCOSIM
            ENDIF

            CALL WAM_CAL_CONFIRM(VTX,DIR,CAL_OK,L2CAL,1)

C-------- THE FOLLOW IS FOR SAMUS TRACKS

          ELSE
            CALL SAM_CAL_CONFIRM(ETAMU,PHIMU,CAL_OK,L2CAL,2)
          ENDIF

	  IF (CAL_OK) THEN
	   CAL_CHECKED(I) = 1
	  ELSE
	   CAL_CHECKED(I) = -1
	  ENDIF
	ELSE						! already checked
	  CAL_OK = CAL_CHECKED(I).GT.0
	ENDIF

          IF (CAL_OK) THEN

C---------- PASSES CAL_CONFIRM; A GOOD MUON

            COUNT = COUNT + 1
            CALL ESUMFL('FILT',ID_MUON,PTMU,ETAMU,ETAMU,PHIMU,
     &        TRACK_QUALITY)
          ENDIF

        ELSE

C-------- CAL_CONFIRM NOT REQUIRED; A GOOD MUON

          COUNT = COUNT + 1
          CALL ESUMFL('FILT',ID_MUON,PTMU,ETAMU,ETAMU,PHIMU,
     &      TRACK_QUALITY)
        ENDIF

C------ PASS EVENT IF THE REQUIRED NUMBER OF GOOD MUONS HAS BEEN FOUND

        IF (COUNT .GE. NUM_MUONS(PARAM_SET_NUMBER)) THEN
          RESULT_FLAG = .TRUE.
          RETURN
        ENDIF

  100 CONTINUE                   ! LOOP OVER TRACKS

      RETURN
      END
