      SUBROUTINE L1_FW_AND_CT_PROGR(LUN,PROGR_STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the programming of the L1 calorimeter
C-      trigger simulator.
C-
C-   Inputs  : LUN      The unit number of the input file
C-   Outputs : PROGR_STATUS Error status. 0 if no error.
C-   Controls: none
C-
C-   Created  25-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  18-OCT-1991   Philippe Laurens, Steven Klocek   
C-                          COOR_DGTZOFF message is now a no-op.
C-   Updated   5-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      COOR_PRESCALER is no longer a no-op.
C-   Updated   7-NOV-1991   Philippe Laurens, Steven Klocek  
C-                      Unrecognized message on first line is no longer fatal. 
C-                      No recognized messages in the input file is a
C-                      fatal error.
C-   Updated  19-NOV-1991   Philippe Laurens, Steven Klocek  
C-                      Modified COOR parser to take its input from a string
C-                      rather than a file. This routine now reads lines from
C-                      the input file and passes them on to the parser.
C-   Updated   3-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      Now recognizes Level 1.5 COOR Messages 
C-                      Clear the list before anything is put on it.
C-   Updated  16-SEP-1992   Philippe Laurens, Steven Klocek  
C-                      Unknown COOR messages now produce warning messages
C-                        rather than fatal errors.
C-                      Take action on additional COOR messages.
C-                      Ignore rest of COOR message after keywords if no 
C-                        action is taken on COOR message.
C-   Updated  28-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Add messages for Large Tiles 
C-   Updated  06-JAN-1994   Zhengzhi Zhang, included L1.5 COOR messages
C-            28-JUN-1994   Philippe LAURENS - re-update using latest code
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      CHARACTER*1 L1COOR_INIT_CHAR
      EXTERNAL L1COOR_INIT_CHAR
      INTEGER TRULEN
      EXTERNAL TRULEN
C
      INTEGER LUN, PROGR_STATUS
C
      LOGICAL GOOD, ERROR
      INTEGER COMMAND,COUNT
      INTEGER LAST_OBJECT, OBJECT
      CHARACTER*1 CHAR
      CHARACTER*80 STRING
      CHARACTER*1122 LINE
C
C       Get ready to read in the programming file
      CALL L1C_FIND_CARDS()
      CALL L1COOR_INIT_KEYWORD_TABLE()
      CALL L1COOR_INIT_COMMAND_TYPE()
      CALL ERRMAX('L1C UNKNOWN PROGR FILE LINE', -1, -1)
      PARSE_STATUS = PARSE_SUCCESS
      PROGR_STATUS = 0
      PARSE_LINE_NUM = 0
C
C
      LIST_TOP = LIST_SIZE
      DO WHILE (.TRUE.)
  100   CONTINUE
C
C       Clear the list. Probably not needed, but do it just in case.
C
        DO COUNT = 1, LIST_TOP
          LIST(COUNT, OBJ_INDEX) = 0
          LIST(COUNT, OBJ_T_INDEX) = PARSE_ASSERTED
          LIST(COUNT, ITEM_INDEX) = 0
          LIST(COUNT, ITEM_T_INDEX) = PARSE_ASSERTED
        END DO
C
C       Get lines from the programming file until end of file is found
C       First, decode the first 20 characters
C
        READ(LUN,110,END=200) LINE
  110   FORMAT( A )
        PARSE_LINE_NUM = PARSE_LINE_NUM + 1
        IF ((LINE(1:1) .EQ. '!') .OR. (LINE .EQ. ' ')) GOTO 100
        CALL UPCASE(LINE, LINE)
C
        CALL L1COOR_GET_COMMAND_TYPE(LINE, COMMAND, GOOD)
C
        IF (DEBUG_COOR_PARSER .EQV. .TRUE.) THEN
          WRITE (6,*) 'Command:', COMMAND, ' Good:',GOOD
        ENDIF
        IF (GOOD .NEQV. .TRUE.) THEN
C       look for unknown command line (not commented out)
          IF ( (PARSE_STATUS .EQ. PARSE_SUCCESS) 
C           Allow nonsense on first line 
C           This is to accept TRIGGER.INFO from COOR_sim 
     &          .AND. ( PARSE_LINE_NUM .NE. 1 ) ) THEN   
            STRING = ' '
            WRITE(STRING,150) PARSE_LINE_NUM
  150       FORMAT(I10)
            CALL ERRMSG('L1C UNKNOWN PROGR FILE LINE',
     &          'L1_FW_AND_CT_PROGR',
     &          'Unknown keyword in the programming file line ' //
     &          STRING(1:TRULEN(STRING)) //
     &          ':' // LINE(1:MAX_COOR_COMMAND_LENGTH), 'W')
          ENDIF
          GOTO 100
        ENDIF
C
C       Switch on the command type
C       First, list all the NO-OP messages
        IF (COMMAND .EQ. COOR_EXIT) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_ACQBANDW) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_AUTODIS) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_AUTOTUNE) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_INITIAL) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_PAUSE) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_RD_TIME) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_REENABLE) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_RESUME) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_DGTZOFF) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_STOP) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_XCEMTOWER) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. COOR_XCHDTOWER) THEN
          GOTO 100           
        ELSE IF (COMMAND .EQ. COOR_WRTHOST) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. L15COOR_SYLOAD) THEN
          GOTO 100
        ELSE IF (COMMAND .EQ. L15COOR_SYSTRT) THEN
          GOTO 100
        ENDIF
C
C       Parse the rest of the line
        CALL L1COOR_GET_LIST(
     &    LINE(MAX_COOR_COMMAND_LENGTH+1:LEN(LINE)), ERROR )
C
        IF (DEBUG_COOR_PARSER .EQV. .TRUE.) THEN
          WRITE (6,*) 'Parse status', PARSE_STATUS
          WRITE (6,*) 'List top', LIST_TOP
          DO COUNT = 1, LIST_TOP
            WRITE (6,300) COUNT, LIST(COUNT, OBJ_INDEX), 
     &        LIST(COUNT, OBJ_T_INDEX), LIST(COUNT, ITEM_INDEX), 
     &        LIST(COUNT, ITEM_T_INDEX)
          END DO
  300     FORMAT( ' #',I3,' ', I10, ' ', I2, ' ', I10, ' ', I2)
        ENDIF
C
        IF (PARSE_STATUS .NE. PARSE_SUCCESS) THEN
          PROGR_STATUS = PARSE_STATUS
          GOTO 999
        ENDIF
C
C       If it is not a no-op, then the list better have elements
        IF (LIST_TOP .EQ. 0) THEN
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) THEN
            PARSE_STATUS = PARSE_BAD_FORMAT
          ENDIF
          PROGR_STATUS = PARSE_STATUS
          GOTO 999
        ENDIF
C
C       Assign AND-OR terms to specific triggers
        IF (COMMAND .EQ. COOR_ANDORREQ) THEN
          CALL L1COOR_ANDOR_INIT()
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) THEN
            PROGR_STATUS = PARSE_STATUS
            GOTO 999
          ENDIF
C
C       Assign global sum thresholds
        ELSEIF (COMMAND .EQ. COOR_EMETSUM) THEN
          CALL L1COOR_GLOBSUM_INIT(GL_EMET_THRTYP)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_HDETSUM) THEN
          CALL L1COOR_GLOBSUM_INIT(GL_HDET_THRTYP)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_TOTETSUM) THEN
          CALL L1COOR_GLOBSUM_INIT(GL_TOTET_THRTYP)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_EML2SUM) THEN
          CALL L1COOR_GLOBSUM_INIT(GL_EML2_THRTYP)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_HDL2SUM) THEN
          CALL L1COOR_GLOBSUM_INIT(GL_HDL2_THRTYP)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_TOTL2SUM) THEN
          CALL L1COOR_GLOBSUM_INIT(GL_TOTL2_THRTYP)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign Hot Tower Count thresholds
        ELSEIF (COMMAND .EQ. COOR_EMETCNT) THEN
          CALL L1COOR_COUNTREF_INIT(TT_EMET_THRTYP)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_TOTETCNT) THEN
          CALL L1COOR_COUNTREF_INIT(TT_TOTET_THRTYP)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Define reference sets
        ELSEIF (COMMAND .EQ. COOR_RSEMET) THEN
          CALL L1COOR_REFSET_INIT(COOR_RSEMET)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_RSHDVETO) THEN
          CALL L1COOR_REFSET_INIT(COOR_RSHDVETO)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_RSTOTET) THEN
          CALL L1COOR_REFSET_INIT(COOR_RSTOTET)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_RSLGTILE) THEN
          CALL L1COOR_REFSET_INIT(COOR_RSLGTILE)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Define which Reference Sets each Specific Trigger pays attention to
        ELSEIF (COMMAND .EQ. COOR_VSEMLIST) THEN
          CALL L1COOR_ST_VS_RS_INIT(EM_ET_REF_MIN)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_VSTOTLIST) THEN
          CALL L1COOR_ST_VS_RS_INIT(TOT_ET_REF_MIN)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSEIF (COMMAND .EQ. COOR_VSLGTILE) THEN
          CALL L1COOR_ST_VS_RS_INIT(LT_REF_MIN)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign Start Digitize signals
        ELSEIF (COMMAND .EQ. COOR_STARTDGT) THEN
          CALL L1COOR_ST_STARTDGT_INIT()
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign Prescalers
        ELSE IF (COMMAND .EQ. COOR_PRESCALE) THEN
          CALL L1COOR_ST_PRESCALER_INIT()
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign Start Digitize Off states
C       Start Digitize Off should be ignored in the simulator 15-OCT-1991 
C
C        ELSEIF (COMMAND .EQ. COOR_DGTZOFF) THEN
C          CALL LV1_DGTZOFF()
C          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign Missing Pt thresholds
        ELSEIF (COMMAND .EQ. COOR_MISPTSUM) THEN
          CALL L1COOR_MISPTSUM_INIT()
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign Level 1.5 Trigger Type
        ELSEIF (COMMAND .EQ. COOR_L15TYPE) THEN
          CALL L1COOR_L15TYPE_INIT()
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign Level 1.5 Term Allocations
        ELSEIF (COMMAND .EQ. COOR_L15TERM) THEN
          CALL L1COOR_L15TERM_INIT()
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign obey Front End Busy veto obey states
C
        ELSEIF (COMMAND .EQ. COOR_OBEYBUSY) THEN
          CALL L1COOR_INIT_OBEYVETO(OBEY_FEBUSY)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign obey Level 2 veto obey states
C
        ELSEIF (COMMAND .EQ. COOR_OBEYLEV2) THEN
          CALL L1COOR_INIT_OBEYVETO(OBEY_L2BUSY)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Assign Front End Busy mapping
C
        ELSEIF (COMMAND .EQ. COOR_FEBZDIS) THEN
          CALL L1COOR_INIT_FEBZDIS
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Free Specific Triggers
C
        ELSEIF (COMMAND .EQ. COOR_FREE) THEN
          CALL L1COOR_FREE_SPECTRIG(.FALSE.)
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Enable/Disable Specific Triggers
C
        ELSEIF (COMMAND .EQ. COOR_ENABLE) THEN
          CALL L1COOR_ENABLE_SPECTRIG
          IF (PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       Reset Specific Trigger Scalers
C
        ELSE IF (COMMAND .EQ. COOR_RESETSCL) THEN
          CALL L1COOR_FREE_SPECTRIG(.TRUE.)
C
C       L1.5  Reference settings
C
        ELSE IF (COMMAND .EQ. L15COOR_REFSET) THEN
          CALL L1COOR_REFSET_INIT(L15COOR_REFSET)
          IF(PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       L1.5  local DSP
C
        ELSE IF (COMMAND .EQ. L15COOR_LOCDSP) THEN
          CALL L15COOR_TOOL_INIT(L15COOR_LOCDSP)
          IF(PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       L1.5  GLOBAL DSP
C
        ELSE IF (COMMAND .EQ. L15COOR_GLBDSP) THEN
          CALL L15COOR_TOOL_INIT(L15COOR_GLBDSP)
          IF(PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       L1.5  Frame Code
C
        ELSE IF (COMMAND .EQ. L15COOR_FRMCOD) THEN
          CALL L15COOR_FRAME_CODE_INIT
          IF(PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
C       L1.5  Specific Trigger vs. Terms
C
        ELSE IF (COMMAND .EQ. L15COOR_STVSTM) THEN
          CALL L15COOR_STVSTM_INIT
          IF(PARSE_STATUS .NE. PARSE_SUCCESS) GOTO 200
C
        ELSE
          IF (DEBUG_COOR_PARSER .EQV. .TRUE.) THEN
            WRITE (6,*) 'Unrecognized command'
          ENDIF
        ENDIF
      END DO
C
  200 CONTINUE
C      IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
C        IF (RECOGNIZED_COMMANDS .EQ. 0) THEN
C          PARSE_STATUS = PARSE_NO_VALID_MESSAGES
C        ENDIF
C      ENDIF
      IF (PARSE_STATUS .NE. PARSE_SUCCESS) THEN
        PROGR_STATUS = PARSE_STATUS
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
