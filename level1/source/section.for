      SUBROUTINE SECTION (LINE, LENGTH, SECTION_ID, ARGUMENT, ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search for SECTION definition.
C-
C-   Inputs  : LINE :   Character string to be decoded;
C-             LENGTH : String length.
C-
C-   Outputs : SECTION_ID : Section identification number;
C-             ARGUMENT :   Integer argument following section definition
C-                          if requested;
C-             ERR :        Error code.
C-
C-             SECTION_ID et ERR codes can be found in :
C-             INTERPRETER_CODES.INC.
C-
C-   Controls: None.
C-
C-   Created  27-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Philippe Laurens, Steven Klocek
C-                            Removed extra RETURN statements to meet D0 
C-                            standards 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:INTERPRETER_CODES.INC'
C
      INTEGER   LENGTH, SECTION_ID, ARGUMENT, ERR
      CHARACTER LINE*80
C
      INTEGER       SECTION_MIN,  SECTION_MAX
      PARAMETER    (SECTION_MIN = TT_DECLARE,
     +              SECTION_MAX = JET_LIST_LENGTH)
      INTEGER       NB_CHAR(SECTION_MIN:SECTION_MAX)
      CHARACTER*40  SECTION_NAME(SECTION_MIN:SECTION_MAX)
      LOGICAL       NEED_ARGUMENT(SECTION_MIN:SECTION_MAX)
C
      INTEGER L, L0, PNTR, PNTR1, TYPE
      REAL    VAL
C
      DATA SECTION_NAME / 'TRIGGER_TOWER_DECLARATION',
     +                    'EM_ET_HOT_TOWER_SET_NUMBER',
     +                    'TOT_ET_HOT_TOWER_SET_NUMBER',
     +                    'GLOBAL_ENERGY_REFERENCES',
     +                    'SPECIFIC_TRIGGER_NUMBER',
     +                    'ACTIVE_SPECIFIC_TRIGGER',
     +                    'EVENT_DECLARATION',
     +                    'JET_LIST_LENGTH'            /
      DATA NB_CHAR / 25, 26, 27, 24, 23, 23, 17, 15 /
      DATA NEED_ARGUMENT/.FALSE., .TRUE.,  .TRUE.,  .FALSE.,
     +                   .TRUE.,  .FALSE., .FALSE., .TRUE.  /
C
C----------------------------------------------------------------------
C
      ERR = PARSER_SECTION_MISSING
      IF(LENGTH.LT.7) GOTO 999
      IF(LINE(1:7).NE.'SECTION') GOTO 999
      PNTR = 8
      L0   = LENGTH - 7
      DO SECTION_ID = SECTION_MIN, SECTION_MAX
        L = NB_CHAR(SECTION_ID)
        IF(L0.GE.L) THEN
          PNTR1 = PNTR + L - 1
          IF(LINE(PNTR:PNTR1).EQ.SECTION_NAME(SECTION_ID)) GOTO 10
        ENDIF
      ENDDO
      ERR = PARSER_BAD_SECTION_DEFINITION
      GOTO 999
C
   10 IF(NEED_ARGUMENT(SECTION_ID)) THEN
        PNTR = PNTR1 + 1
        CALL FIND_ONE_VALUE (LINE, LENGTH, PNTR, VAL, TYPE, ERR)
        IF(ERR.NE.0) GOTO 999
        ERR = PARSER_ILLEGAL_ARGUMENT
        IF(TYPE.NE.1) GOTO 999
        ARGUMENT = VAL
      ENDIF
      ERR  = PARSER_SUCCESS
  999 RETURN
      END
