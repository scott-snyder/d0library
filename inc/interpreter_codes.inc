C---------------------------------------------------------------------
C
C                       INTERPRETER_CODES
C                       =================
C          Modified 14-AUG-1991, Amber Boehnlein, added "Muon" section
C                                                 id number
C
C---------------------------------------------------------------------
C
C     Interpreter error codes :
C     -------------------------
C
      INTEGER    PARSER_SUCCESS,
     +           PARSER_MISSING_ACTION,
     +           PARSER_UNKNOWN_ACTION,
     +           PARSER_SECTION_MISSING,
     +           PARSER_BAD_SECTION_DEFINITION,
     +           PARSER_SYNTAX_ERROR,
     +           PARSER_ETA_OUT_OF_RANGE,
     +           PARSER_PHI_OUT_OF_RANGE,
     +           PARSER_END_OF_FILE,
     +           PARSER_ILLEGAL_ARGUMENT,
     +           PARSER_TOO_LONG_LIST,
     +           PARSER_OPEN_FAILURE,
     +           PARSER_ALL_TOWERS_MASKED,
     +           PARSER_TRIGGER_OUT_OF_RANGE,
     +           PARSER_SET_OUT_OF_RANGE,
     +           PARSER_TERM_OUT_OF_RANGE,
     +           PARSER_LENGTH_OUT_OF_RANGE
C
      PARAMETER (PARSER_SUCCESS                =  0,
     +           PARSER_MISSING_ACTION         =  1,
     +           PARSER_UNKNOWN_ACTION         =  2,
     +           PARSER_SECTION_MISSING        =  3,
     +           PARSER_BAD_SECTION_DEFINITION =  4,
     +           PARSER_SYNTAX_ERROR           =  5,
     +           PARSER_ETA_OUT_OF_RANGE       =  6,
     +           PARSER_PHI_OUT_OF_RANGE       =  7,
     +           PARSER_END_OF_FILE            =  8,
     +           PARSER_ILLEGAL_ARGUMENT       =  9,
     +           PARSER_TOO_LONG_LIST          = 10,
     +           PARSER_OPEN_FAILURE           = 11,
     +           PARSER_ALL_TOWERS_MASKED      = 12,
     +           PARSER_TRIGGER_OUT_OF_RANGE   = 13,
     +           PARSER_SET_OUT_OF_RANGE       = 14,
     +           PARSER_TERM_OUT_OF_RANGE      = 15,
     +           PARSER_LENGTH_OUT_OF_RANGE    = 16)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     Section identificater numbers :
C     -------------------------------
C
      INTEGER    TT_DECLARE,
     +           EM_ET_HOT_TOWER,
     +           TOT_ET_HOT_TOWER,
     +           GLOBAL_REF,
     +           SPECIFIC_TRIGGER_DEF,
     +           ACTIVE_TRIGGER,
     +           EVENT_DEFINITION,
     +           JET_LIST_LENGTH,
     +           HOW_MANY_MUON
C
      PARAMETER (TT_DECLARE           = 1,
     +           EM_ET_HOT_TOWER      = 2,
     +           TOT_ET_HOT_TOWER     = 3,
     +           GLOBAL_REF           = 4,
     +           SPECIFIC_TRIGGER_DEF = 5,
     +           ACTIVE_TRIGGER       = 6,
     +           EVENT_DEFINITION     = 7,
     +           JET_LIST_LENGTH      = 8,
     +           HOW_MANY_MUON        = 9)
C
