      SUBROUTINE L1C_ART_EVENTS (LUN, EVENT_FILE, RUN,
     +                                        OUTPUT,
     +                                        LISTING, TRGR_DUMP, MODE,
     +                                        NOISE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads events from an ASCII file, decodes them
C-
C-   Inputs  : LUN :        Logical Unit Number to be use to read in the
C-                          event input file;
C-             EVENT_FILE : Name of the event input file;
C-             RUN :        Run number : Fake Event file extension number;
C-             OUTPUT :     Logical Unit Number to be used to write out
C-                          events in Zebra format;
C-             LISTING :    Logical Unit Number to be used to produce an
C-                          output listing.
C-             TRGR_DUMP :  | 1 -> TRGR bank dump;
C-                          | 2 -> TRGR bank print;
C-                          | 3 -> TRGR bank dump and print;
C-                          | 4 -> Nothing.
C-             MODE :       TRGR Dump type.
C-             NOISE :      In order to request electronic noise generation.
C-
C-   Outputs : None.
C-   Controls: None.
C-
C-   Created   6-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  19-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Patch it so it can be used to load debugging
C-                          events.
C-                        - Now uses D0OPEN 
C-                        - rename routine from TRG_SIMUL_ARTIFICIAL_EVENTS
C-                          to L1C_ART_EVENTS
C-                        - Changed all occurances of EVENT_BUILDER to
C-                          L1C_ART_EVENT_BUILDER. 
C-                        - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                          D0$PARAMS:L1_CALTRIG.PARAMS 
C-                        - Replaced D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS with
C-                          D0$PARAMS:L1_FRAMEWORK.PARAMS 
C-                        - Replaced D0$INC:SPECIFIC_TRIGGER.INC with
C-                          D0$INC:L1_SPECIFIC_TRIGGER.INC 
C-                        - Replaced D0$INC:TRG_SIMUL_RAW_EVENT.INC with
C-                          D0$INC:L1C_EVENT_RAW.INC 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:INTERPRETER_CODES.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
C
      INTEGER READPF
C
      INTEGER       LUN, RUN, OUTPUT, LISTING, TRGR_DUMP
      CHARACTER*(*) EVENT_FILE, MODE
      LOGICAL       NOISE
C
      CHARACTER*80 LINE
      INTEGER      LINE_NUM, LENGTH, ERR, SECTION_ID, DUMMY
      INTEGER      I
      LOGICAL      FIRST, OK
C
      DATA FIRST /.TRUE./
C
C----------------------------------------------------------------------
C
C     Input file opening
C     ==================
C
      CALL D0OPEN(LUN, EVENT_FILE, 'I', OK)
      IF (OK.NEQV. .TRUE.) GOTO 200
      LINE_NUM = 0
      CALL READ_NEW_LINE (LUN, LINE_NUM, LINE, LENGTH, ERR)
      IF(ERR.NE.0) GOTO 210
      RUN_NUMBER   = RUN
      EVENT_NUMBER = 0
      FIRST = .FALSE.
C
C     New event decoding
C     ==================
C
   10 CALL SECTION (LINE, LENGTH, SECTION_ID, DUMMY, ERR)
      IF(ERR.NE.0) GOTO 210
      IF(SECTION_ID.NE.EVENT_DEFINITION) THEN
        ERR = PARSER_BAD_SECTION_DEFINITION
        GOTO 210
      ENDIF
      EVENT_NUMBER = EVENT_NUMBER + 1
      CALL L1C_ART_EVENT_BUILDER (LUN, LINE_NUM, LISTING, NOISE, ERR)
      IF( (ERR.NE.PARSER_UNKNOWN_ACTION) .AND.
     +    (ERR.NE.PARSER_END_OF_FILE)          ) GOTO 210
      CLOSE (LUN)
      GOTO 999
  200 ERR = PARSER_OPEN_FAILURE
      GOTO 220
  210 CLOSE(LUN)
  220 WRITE (LINE,9200) ERR, LINE_NUM, CHAR(7)
      CALL OUTMSG (LINE)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
 9000 FORMAT ('1''Artificial'' event #',I3,/)
 9001 FORMAT ('1')
 9200 FORMAT (' Event read-out error #',I3,' line #',I5,A1)
C
  999 RETURN
      END
