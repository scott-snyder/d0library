      SUBROUTINE L1_MUON_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Read in the muon trigger configuration
C-                          file which contains COOR messages to be
C                           parsed and used for initialization of
C-                          the muon trigger system
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  21-NOV-1991   Kamel A. Bazizi
C-   Updated  12-DEC-1991   Philippe Laurens, Steven Klocek
C-                          move here the reading of muon programming file
C-                          name in L1SIM.RCP file
C-   Updated  12-JUL-1992   James T. Linnemann  and announce by INTMSG
C-   Updated   6-DEC-1992   Kamel A. Bazizi, Guilherme Lima
C-                          Initialization for muon trigger simulator
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
C
      INTEGER  TRULEN
      EXTERNAL TRULEN
      INTEGER  GT_USER
      PARAMETER (GT_USER = 1)           ! DEFAULT USER UNIT NUMBER
C
      INTEGER  LUN,  PROGR_STATUS, IER
      LOGICAL OK
      CHARACTER*72 STRING
C      INTEGER      REF, TRIGGER
      INTEGER I
C
C
c      I=2
c      CALL INZCOM(I)            !Initialize Zebra data banks
c      CALL INZSTP               !Initialize Zebra STP banks
C
C
C.. Pick up RCP parameters
      CALL L1UTIL_PICK_L1SIM_RCP
C
C.. Read in MUON Configuration File name
C
      CALL      EZGETS ( MU_CONFIG_FILE_RCPKEY,
     &                   1,
     &                   MU_PROGRAMMING_FILE_NAME,
     &                   MU_PROGRAMMING_FILE_NAME_LENGTH,
     &                   IER)
      CALL INTMSG(' MUON PROGRAMMING FILE : '//
     &  MU_PROGRAMMING_FILE_NAME(1:TRULEN(MU_PROGRAMMING_FILE_NAME)) )
      IF (IER .NE. 0) THEN
        CALL    EZGET_ERROR_TEXT ( IER, STRING )
        CALL    ERRMSG ( MU_CONFIG_FILE_RCPKEY,
     &                  'L1_MUON_INIT',
     &                   STRING,
     &                  'F')
        GOTO 999
      ENDIF
C
      IF(IER.EQ.0)CALL EZRSET
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
C
C
C       Read in muon programming file (.INFO file)
C
      OK = .TRUE.                     ! PRESET TO TRUE
      CALL D0OPEN(LUN,
     &  MU_PROGRAMMING_FILE_NAME(1:MU_PROGRAMMING_FILE_NAME_LENGTH),
     &  'I',OK)
      IF (OK.EQV..FALSE.) THEN
        CALL ERRMSG('L1SIM BAD MUON PROGRAMMING FILE',
     &    'L1_MUON_INIT',
     &    'Error opening the programming file','F')
        GOTO 999
      ENDIF
C
C
      CALL MU_PROGR(LUN,PROGR_STATUS)
      CALL MU_TRREG_DISBL
      CLOSE (LUN)
C
      IF (PROGR_STATUS.NE.0) THEN
        STRING = ' '
        WRITE(STRING,200) PARSE_LINE_NUM
  200   FORMAT(I10)
C
C       Pick an error message depending on the error status
        IF (PROGR_STATUS .EQ. PARSE_BAD_RANGE) THEN
          CALL ERRMSG('L1SIM BAD MUON PROGRAMMING FILE',
     &      'L1_MUON_INIT',
     &      'Bad range in the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
C
        ELSEIF (PROGR_STATUS .EQ. PARSE_BAD_PARAM) THEN
          CALL ERRMSG('L1SIM BAD MUON PROGRAMMING FILE',
     &      'L1_MUON_INIT',
     &      'Bad parameter in the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
C
        ELSEIF (PROGR_STATUS .EQ. PARSE_BAD_UNKNOWN) THEN
          CALL ERRMSG('L1SIM BAD MUON PROGRAMMING FILE',
     &      'L1_MUON_INIT',
     &      'Unknown keyword in the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
C
        ELSEIF (PROGR_STATUS .EQ. PARSE_BAD_FORMAT) THEN
          CALL ERRMSG('L1SIM BAD MUON PROGRAMMING FILE',
     &      'L1_MUON_INIT',
     &      'Bad format in the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
C
        ELSEIF (PROGR_STATUS .EQ. PARSE_NO_VALID_MESSAGES) THEN
          CALL ERRMSG('L1SIM BAD MUON PROGRAMMING FILE',
     &      'L1_MUON_INIT',
     &      'No recognizable messages in programming file',
     &      'F')
C
        ELSE ! PARSE_FAILURE and anything else that might have been missed
          CALL ERRMSG('L1SIM BAD MUON PROGRAMMING FILE',
     &      'L1_MUON_INIT',
     &      'Error parsing the programming file line ' //
     &      STRING(1:TRULEN(STRING)), 'F')
        ENDIF
      ENDIF
C<<
C<<
C.. Read in MUON RCP file MUSIM_RCP
C
C.. Read control file into an SRCP bank
      CALL INRCP('MUSIM_RCP',IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG(' INRCP','L1_MUON_INIT','Could not read: '
     &    // 'MUSIM_RCP','F')
        GOTO 999
      ENDIF
C<<
      CALL EZPICK('MUSIM_RCP')
      CALL EZERR(IER)     ! Check if error
      IF(IER.NE.0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(' EZPICK ERR','L1_MUON_INIT',STRING,'F')
        GOTO 999
      ENDIF
C
C-- De-select RCP bank
C
      CALL EZRSET()
C<<
C
C-- Read in the muon level 1.5 Tables
C
      CALL MU_L15_TAB_INIT
C
C.. Create a new Zebra subdirectory for MUSIM histograms
      CALL HMDIR('//PAWC/MUSIM_L1','S')

C.. Create a new Zebra subdirectory for MUSIM histograms
      CALL HMDIR('//PAWC/MUSIM_L15','S')
C
C----------------------------------------------------------------------
  999 RETURN
      END
