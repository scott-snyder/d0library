      SUBROUTINE L1_FW_AND_CT_INIT_PROGR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call routine to read in the TCC message file, 
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   3-FEB-1992   Philippe Laurens, Steven Klocek
C-                      Moved this code from L1_FW_AND_CT_INIT.
C-                      Added display of programming file name.
C-   Updated  13-JUL-1993   Philippe Laurens - MSU L1 Trigger   
C-                      Add a temporary patch to read in 8 Large Tile Reference
C-                      Set files  in the format COOR uses, translating each
C-                      line before interpretation. 
C-                      Force the Programming Mask to show that all Specific
C-                      Triggers depend on all the Large Tile Reference Sets
C-                      specified through the patch .
C-                      Fix the routine name passed to ERRMSG from
C-                      'L1_FW_AND_CT_INIT' to 'L1_FW_AND_CT_INIT_PROGR'.
C-                      Extend writing programming mask from TOT_ET_REF_MAX to
C-                      LT_REF_MAX.
C-                      
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
C
      INTEGER  TRULEN
      EXTERNAL TRULEN
C
      INTEGER  GT_USER
      PARAMETER (GT_USER = 523)           ! DEFAULT USER UNIT NUMBER
      INTEGER LUN, PROGR_STATUS, IER
      LOGICAL OK
      CHARACTER*72 STRING
      CHARACTER*127 BUF
      INTEGER REF, TRIGGER
C     The following variables are for the temporary patch 
C     to read in Large Tile Ref Sets
      CHARACTER*19 RCP_NAME
      CHARACTER*80 LT_FILE_NAME
      INTEGER      STRING_LENGTH
      INTEGER      POUND_SIGN
      INTEGER      RCP_OK, FIRST_STRING
      PARAMETER   (RCP_OK = 0, FIRST_STRING = 1 )
      LOGICAL      BAD_LINE, SEND_LINE
C-----------------------------------------------------------------------
C
      CALL L1UTIL_EXPAND_FILENAME(
     &            PROGRAMMING_FILE_NAME(1:PROGRAMMING_FILE_NAME_LENGTH),
     &            BUF)
      CALL INTMSG ( ' L1SIM> Programming File: ' // BUF(1:TRULEN(BUF)) )
C
C       Allocate a logical unit
C
      CALL GTUNIT (GT_USER , LUN ,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG (' LUN ','GTUNIT',
     &    'Could not allocate logical unit','F')
        GOTO 999
      ENDIF
C
C       Read in programming file
C
      OK = .TRUE.                     ! PRESET TO TRUE
      CALL D0OPEN(LUN,
     &  PROGRAMMING_FILE_NAME(1:PROGRAMMING_FILE_NAME_LENGTH),
     &  'I',OK)
      IF (OK.EQV..FALSE.) THEN
        CALL ERRMSG('L1SIM BAD PROGRAMMING FILE',
     &    'L1_FW_AND_CT_INIT_PROGR',
     &    'Error opening the programming file','F')
        GOTO 999
      ENDIF
C
C
      CALL L1_FW_AND_CT_PROGR(LUN,PROGR_STATUS)
      CLOSE (LUN)
C
      IF (PROGR_STATUS.NE.0) THEN
        STRING = ' '
        WRITE(STRING,200) PARSE_LINE_NUM
  200   FORMAT(I10)
C
C       Pick an error message depending on the error status
        IF (PROGR_STATUS .EQ. PARSE_BAD_RANGE) THEN
          CALL ERRMSG('L1C BAD PROGRAMMING FILE',
     &      'L1_FW_AND_CT_INIT_PROGR',
     &      'Bad range in the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
C
        ELSEIF (PROGR_STATUS .EQ. PARSE_BAD_PARAM) THEN
          CALL ERRMSG('L1C BAD PROGRAMMING FILE',
     &      'L1_FW_AND_CT_INIT_PROGR',
     &      'Bad parameter in the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
C
        ELSEIF (PROGR_STATUS .EQ. PARSE_BAD_UNKNOWN) THEN
          CALL ERRMSG('L1C BAD PROGRAMMING FILE',
     &      'L1_FW_AND_CT_INIT_PROGR',
     &      'Unknown keyword in the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
C
        ELSEIF (PROGR_STATUS .EQ. PARSE_BAD_FORMAT) THEN
          CALL ERRMSG('L1C BAD PROGRAMMING FILE',
     &      'L1_FW_AND_CT_INIT_PROGR',
     &      'Bad format in the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
C
        ELSEIF (PROGR_STATUS .EQ. PARSE_NO_VALID_MESSAGES) THEN
          CALL ERRMSG('L1C BAD PROGRAMMING FILE',
     &      'L1_FW_AND_CT_INIT_PROGR',
     &      'No recognizable messages in programming file',
     &      'F')
C
        ELSE ! PARSE_FAILURE and anything else that might have been missed
          CALL ERRMSG('L1C BAD PROGRAMMING FILE',
     &      'L1_FW_AND_CT_INIT_PROGR',
     &      'Error parsing the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
        ENDIF
      ENDIF
C
C     This is now a patch to read in Large Tile Reference Set Files that weren't
C     read in, and translated to TCC messages by COOR.
C
C      
C       Pick up RCP parameters
      CALL L1UTIL_PICK_L1SIM_RCP
C
      DO REF = 0 , 7
        WRITE ( RCP_NAME, 1000 ) REF
 1000   FORMAT ( 'LARGE_TILE_REFSET_', I1 )
        CALL EZGETS( RCP_NAME, FIRST_STRING, LT_FILE_NAME,
     &               STRING_LENGTH, IER)
        IF (IER .NE. RCP_OK) GOTO 500
C
        CALL D0OPEN(LUN, LT_FILE_NAME, 'I',OK)
        IF (OK.EQV..FALSE.) THEN
          CALL ERRMSG('L1SIM BAD LT REFSET FILE', 
     &      'L1_FW_AND_CT_INIT_PROGR',
     &      'Error opening Large Tile Ref Set file ' // LT_FILE_NAME,
     &      'F')
          GOTO 500 
        ENDIF
C       
C       Read a line
  300   CONTINUE
        READ( LUN, 2000, END=400 ) BUF
 2000   FORMAT( A )
C
C       Translate the line, but first, remove poun sign
        POUND_SIGN = INDEX( BUF, '#' )
        WRITE ( BUF(POUND_SIGN:POUND_SIGN), 3000 ) REF
 3000   FORMAT ( I1 )
        CALL L1UTIL_TRANSLATE_LTRS_LINE ( BUF, SEND_LINE, BAD_LINE )
        IF ( BAD_LINE .EQV. .TRUE. ) THEN
          CALL ERRMSG('L1SIM BAD LT REFSET LINE', 
     &      'L1_FW_AND_CT_INIT_PROGR',
     &      'Bad Line in Large Tile Ref Set file ' // LT_FILE_NAME,
     &      'F')
          GOTO 600 
        ENDIF
C
C       Interpret the line
        IF ( SEND_LINE .EQV. .TRUE. ) 
     &    CALL L1COOR_REFSET_INIT ( COOR_RSLGTILE )
C
C       Next Line
        GOTO 300
C
C       Done wth this file
  400   CONTINUE
        CLOSE (LUN)
C
C       Force The Programming mask to show that ALL sptrg use this refset.
        PROGRAMMING_MASK(REF) = 0
        DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
          ST_VS_RS( TRIGGER, LT_REF_MIN + REF ) = .TRUE.
        END DO
C
C       Next File
  500   CONTINUE
      ENDDO
C
C     De-select RCP bank
C
  600 CONTINUE
      CALL EZRSET
C
C       Derive the Jet List Programming Masks from the SpTrg vs RefSet data
C
      DO REF = EM_ET_REF_MIN, LT_REF_MAX
        PROGRAMMING_MASK(REF) = 0
        DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
          IF (ST_VS_RS(TRIGGER, REF) .EQV. .TRUE.) THEN
            CALL SBIT1 (PROGRAMMING_MASK(REF), TRIGGER-TRG_NUM_MIN+1)
          ENDIF
        END DO
      END DO
C
C     Assign the value for the maximum length of the Jet Lists in the raw data 
      JET_LENGTH = 16
C
C     Release IO Unit
      CALL RLUNIT(GT_USER, LUN, IER)
C----------------------------------------------------------------------
  999 RETURN
      END
