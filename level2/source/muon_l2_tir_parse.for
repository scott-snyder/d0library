      SUBROUTINE MUON_L2_TIR_PARSE(PSN,TRIG_LEVEL,DET_REGION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRACK_IN_ROAD(PSN) is passed through common
C-            to this routine which then determines the trigger levels
C-            and detector regions required for muon tracking. Backwards-
C-            compatible with pre-muon_util-V3.00_02 configuration files.
C-
C-   Inputs  : Parameter_Set_Number PSN
C-   Outputs :
C-             TRIG_LEVEL      Trigger levels for tracking
C-             DET_REGION      Detector regions for tracking
C-
C-   Controls: None
C-
C-   Created   6-SEP-1992   Tom Diehl
C-   Modified  11-MAR-94    Rewritten for muon_util V3.00_02 by R. Markeloff
C-             15-APR-94    Paul Quintas. Fixed parse of old style TIR.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MUON_L2_PARAMS.INC'
      INCLUDE 'D0$INC:MUON_L2_PARAMS_C.INC'
      INTEGER PSN, TRIG_LEVEL, DET_REGION
      CHARACTER*1 DET_REGION_NUMBER(4)
      CHARACTER*2 DET_STRING, DUMMY2
      CHARACTER*3 TRIG_STRING, ANSWER(4)
      CHARACTER*8  DUMMY8
      CHARACTER*14 DUMMY14
      CHARACTER*19 DUMMY19

      DATA DET_REGION_NUMBER / '1', '2', '3', '4' /
C----------------------------------------------------------------------

C---- IF FOURTH CHARACTER IN STRING IS AN UNDERSCORE, THEN WE HAVE V3.00_02
C---- OR LATER CONFIGURATION FILE

      IF (TRACK_IN_ROAD(PSN)(4:4) .EQ. '_') THEN
        TRIG_STRING = TRACK_IN_ROAD(PSN)(1:3)
        DET_STRING = TRACK_IN_ROAD(PSN)(6:7)

C------ FIRST DECIDE TRIG_LEVEL

        IF (TRIG_STRING .EQ. 'ALL' .OR. TRIG_STRING .EQ. 'all') THEN

C-------- IGNORE MUON TRIGGER

          TRIG_LEVEL = 0

        ELSEIF (TRIG_STRING(2:3) .EQ. '10') THEN

C-------- REQUIRE LEVEL 1

          TRIG_LEVEL = 1

        ELSEIF (TRIG_STRING(2:3) .EQ. '15') THEN

C-------- REQUIRE LEVEL 1.5

          IF (TRACK_IN_ROAD(PSN)(9:10) .EQ. 'LO' .OR.
     &      TRACK_IN_ROAD(PSN)(9:10) .EQ. 'lo') THEN
            TRIG_LEVEL = 2
          ELSEIF (TRACK_IN_ROAD(PSN)(9:10) .EQ. 'HI' .OR.
     &      TRACK_IN_ROAD(PSN)(9:10) .EQ. 'hi') THEN
            TRIG_LEVEL = 3
          ELSE
            CALL ERRMSG('L15 LOW OR HIGH NOT SPECIFIED',
     &        'MUON_L2_TIR_PARSE',' LOW is assumed ','W')
            TRIG_LEVEL = 2
          ENDIF

        ENDIF

C------ NOW DECIDE DETECTOR REGION

        DET_REGION = 1
        DO WHILE (DET_REGION .LT. 4 .AND. DET_STRING(2:2) .NE.
     &    DET_REGION_NUMBER(DET_REGION))
          DET_REGION = DET_REGION + 1
        ENDDO

        IF (DET_STRING(1:1) .EQ. 'X' .OR. DET_STRING(1:1) .EQ. 'x')
     &    DET_REGION = -DET_REGION

      ELSE

C------ OLD STYLE CONFIGURATION FILES

C------ GET SUBSTRINGS INDICATING TRIGGER LEVELS

        READ(TRACK_IN_ROAD(PSN),101)DUMMY2,ANSWER(1)
        READ(TRACK_IN_ROAD(PSN),102)DUMMY8,ANSWER(2)
        READ(TRACK_IN_ROAD(PSN),103)DUMMY14,ANSWER(3)
        READ(TRACK_IN_ROAD(PSN),104)DUMMY19,ANSWER(4)

  101   FORMAT(A2,A3)
  102   FORMAT(A8,A3)
  103   FORMAT(A14,A3)
  104   FORMAT(A19,A3)

C------ ONLY FOUR STRINGS EVER USED WITH OLD STYLE

C	CFALL_EFL10_WSL10_SXXX ==> L10_(y3)
C	CFALL_EFL10_WSL1A_SXXX ==> L10_(y3)
C	CFALL_EFL10_WSL10_SL10 ==> L10_(y4)
C	CFALL_EFL10_WSL1A_SL10 ==> L10_(y4)

	TRIG_LEVEL = 1
	IF (ANSWER(4) .EQ. 'XXX' .OR.
     x      ANSWER(4) .EQ. 'xxx') THEN
	  DET_REGION = 3
	ELSE
	  DET_REGION = 4
	ENDIF

      ENDIF
      RETURN
      END
