      SUBROUTINE L1C_ART_EVENT_BUILDER (LUN, LINE_NUM, LISTING, 
     &  NOISE, ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads a new event from an ASCII file.
C-
C-   Inputs  : LUN :      Logical Unit Number to be read;
C-             LINE_NUM : line counter in the input file.
C-             LISTING :  Logical Unit Number to be used for event
C-                        printing out (no print if nil).
C-             NOISE :    In order to request electronic noise generation.
C-
C-   Outputs : LINE_NUM : incremented line counter;
C-             ERR :      error code.
C-
C-   Controls: None.
C-
C-   Created   5-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  19-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                    - This routine is now only used for artificial events
C-                    - Removed extra RETURN statements to meet D0
C-                      standards.
C-                    - Removed reference to NORMAL_VARIABLE
C-                    - Changed name of routine from EVENT_BUILDER to
C-                      L1C_ART_EVENT_BUILDER. 
C-                    - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                      D0$PARAMS:L1_CALTRIG.PARAMS 
C-                    - Replaced D0$INC:TRG_SIMUL_EVENT.INC with
C-                      D0$INC:L1C_EVENT.INC 
C-                    - Replaced D0$INC:TRG_SIMUL_RAW_EVENT.INC with
C-                      D0$INC:L1C_EVENT_RAW.INC 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:INTERPRETER_CODES.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
C
      INTEGER    BOTH_TOWER,                VERTEX
      PARAMETER (BOTH_TOWER = HD_TOWER + 1, VERTEX = BOTH_TOWER + 1)
C
      INTEGER    LUN, LINE_NUM, LISTING, ERR
      LOGICAL    NOISE
C
      INTEGER      NB_KEY
      PARAMETER   (NB_KEY = VERTEX - EM_TOWER + 1)
      CHARACTER*20 KEY_LIST(NB_KEY)
      INTEGER      KEY_LEN(NB_KEY)
      LOGICAL      DEFINE(EM_TOWER:HD_TOWER,EM_TOWER:BOTH_TOWER)
C
      CHARACTER LINE*80, OUT*55
      REAL      ENERGY(EM_TOWER:HD_TOWER), EM, HD
      INTEGER   CHANNEL, PHI, ETA, ETA_SIGN, ETA1, ETA2, PHI1, PHI2
      INTEGER   INDEX, LENGTH, PNTR, TYPE, N, SIGN(POS_ETA:NEG_ETA)
C
      DATA KEY_LIST / 'EM_TRIGGER_TOWER', 'HD_TRIGGER_TOWER',
     +                'TRIGGER_TOWER',    'VERTEX'            /
      DATA SIGN     / +1, -1 /
      DATA KEY_LEN  / 16, 16, 13, 6 /
      DATA DEFINE   / .TRUE.,  .FALSE.,
     +                .FALSE., .TRUE.,
     +                .TRUE.,  .TRUE.  /
C
C----------------------------------------------------------------------
C
C     New line interpretation
C     =======================
C
      INDEX = 0
   10 CALL WHAT_TO_DO (LUN, LINE_NUM,
     +                 NB_KEY, KEY_LIST, KEY_LEN, INDEX,
     +                 LINE, LENGTH, PNTR, ERR)
      IF( (ERR.EQ.PARSER_UNKNOWN_ACTION).OR.
     +    (ERR.EQ.PARSER_END_OF_FILE)       ) GOTO 200
      IF(ERR.NE.0) GOTO 999
      INDEX = INDEX + EM_TOWER - 1
      IF(INDEX.EQ.VERTEX) GOTO 100
C
C     Trigger Tower energy definition
C     ===============================
C
      OUT = ' '
      IF(INDEX.EQ.EM_TOWER) THEN
        WRITE(OUT(1:18),9000)
      ELSE IF(INDEX.EQ.HD_TOWER) THEN
        WRITE(OUT(1:18),9001)
      ELSE
        WRITE(OUT(1:18),9002)
      ENDIF
   20 CALL DOUBLE_RANGE (LINE,LENGTH,PNTR,ETA1,ETA2,PHI1,PHI2,ERR)
      IF(ERR.NE.0) GOTO 999
      CALL RANGE_CHECK (ETA1, ETA2, ETA_SIGN, PHI1, PHI2, ERR)
      IF(ERR.NE.0) GOTO 999
      IF (ETA1.EQ.ETA2) THEN
        WRITE(OUT(19:25),9020) SIGN(ETA_SIGN)*ETA1
      ELSE
        WRITE(OUT(19:25),9021) SIGN(ETA_SIGN)*ETA1, SIGN(ETA_SIGN)*ETA2
      ENDIF
      OUT(26:26) = ','
      IF (PHI1.EQ.PHI2) THEN
        WRITE(OUT(27:33),9020) PHI1
      ELSE
        WRITE(OUT(27:33),9021) PHI1, PHI2
      ENDIF
      IF(INDEX.EQ.BOTH_TOWER) THEN
        N = 2
        CALL FIND_REAL_LIST (LINE, LENGTH, PNTR, N, ENERGY, ERR)
        IF(ERR.NE.0) GOTO 999
        IF(N.NE.2) THEN
          ERR = PARSER_SYNTAX_ERROR
          GOTO 999
        ENDIF
        WRITE(OUT(34:55),9022) ENERGY
      ELSE
        CALL FIND_ONE_VALUE (LINE, LENGTH, PNTR, ENERGY(INDEX), TYPE,
     +                        ERR)
        IF(ERR.NE.0) GOTO 999
        WRITE(OUT(34:45),9023) ENERGY(INDEX)
      ENDIF
      DO CHANNEL = EM_TOWER, HD_TOWER
        DO PHI = PHI1, PHI2
          DO ETA = ETA1, ETA2
            IF(DEFINE(CHANNEL,INDEX))
     +         TT_ENERGY(ETA_SIGN,ETA,PHI,CHANNEL) = 
     +            TT_ENERGY(ETA_SIGN,ETA,PHI,CHANNEL) + ENERGY(CHANNEL)
          ENDDO
        ENDDO
      ENDDO
      IF(PNTR.LE.LENGTH) GOTO 20
      GOTO 10
C
C     Vertex definition
C     =================
C
  100 CALL FIND_ONE_VALUE (LINE, LENGTH, PNTR, Z_VERTEX, TYPE, ERR)
      IF(ERR.NE.0) GOTO 999
      GOTO 10
C
  200 CONTINUE
C
  999 RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
 9000 FORMAT ('EM_TRIGGER_TOWER (')
 9001 FORMAT ('HD_TRIGGER_TOWER (')
 9002 FORMAT ('TRIGGER_TOWER    (')
 9020 FORMAT (I7)
 9021 FORMAT (I3,':',I3)
 9022 FORMAT (')  (',F8.2,',',F8.2,')')
 9023 FORMAT (')   ',F8.2)
 9024 FORMAT (1X,A55)
 9100 FORMAT (' VERTEX',F7.0)
C
      END
