      FUNCTION EZERR (IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set TRUE if IER not equal to ZERO. Use this
C-                         function after a call to an SRCP routine
C-                         to test for errors. Use the routine
C-                         EZGET_ERROR_TEXT(IER,STRING) to return a
C-                         string describing error given error code.
C-
C-   Inputs  : None
C-
C-   Outputs : IER         Error code
C-                         0 --- OK. NO ERROR.
C-   Controls: None
C-
C-   ENTRY EZGET_ERROR_TEXT (IER,STRING)
C-
C-   Created  23-SEP-1988   Harrison B. Prosper
C-   Updated  14-MAY-1990   Harrison B. Prosper
C-      Add entry points EZERROR,EZGET_ERROR_TEXT.
C-   Updated   3-Jan-1996   sss - Compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EZERR,EZERROR,EZGET_ERROR_TEXT
      INTEGER IER
      CHARACTER*(*) STRING
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
C----------------------------------------------------------------------
      ENTRY  EZERROR (IER)
C----------------------------------------------------------------------
      IER = ERRSRC
      EZERR = IER .NE. 0
      EZERROR = IER .NE. 0
      RETURN
C
      ENTRY EZGET_ERROR_TEXT (IER,STRING)
      EZGET_ERROR_TEXT = IER .NE. 0
C
      STRING=
     &'                                                          '
      IF     ( IER .EQ. EZS_SUCCESS )           THEN
        STRING=' Everything is just dandy!'
      ELSEIF ( IER .EQ. EZS_BANK_NOTFOUND )     THEN
        STRING=' SRCP Bank NOT found'
      ELSEIF ( IER .EQ. EZS_PARAM_NOTFOUND)     THEN
        STRING=' Parameter NOT found within SRCP bank'
      ELSEIF ( IER .EQ. EZS_LINK_NOTFREE )      THEN
        STRING=' Support link for SRCP bank is NOT free'
      ELSEIF ( IER .EQ. EZS_TOOMANY_BANKS )     THEN
        STRING=' Maximum allowed number of SRCP banks reached'
      ELSEIF ( IER .EQ. EZS_DUPLICATE_BANK )    THEN
        STRING=' Duplicate bank name; choose another name or drop bank'
      ELSEIF ( IER .EQ. EZS_DUPLICATE_NAME )    THEN
        STRING=' Duplicate parameter name within current SRCP bank'
      ELSEIF ( IER .EQ. EZS_BAD_ARGUMENT )      THEN
        STRING=' An input argument has a bad value'
      ELSEIF ( IER .EQ. EZS_NOT_STANDALONE )    THEN
        STRING=' SRCP bank is NOT standalone'
      ELSEIF ( IER .EQ. EZS_NOT_SRCPBANK )      THEN
        STRING=' Hang on! This is NOT an SRCP bank'
      ELSEIF ( IER .EQ. EZS_MAX_DEPTH )         THEN
        STRING=' EZPICKs already nested to maximum allowed depth'
      ELSEIF ( IER .EQ. EZS_BAD_VALUE )         THEN
        STRING=' The current value is screwed up!'
      ELSEIF ( IER .EQ. EZS_ARRAY_TOOLARGE )    THEN
        STRING=' The array parameter is TOO large for internal buffers'
      ELSEIF ( IER .EQ. EZS_BANK_NOTSELECTED )  THEN
        STRING=' NO RCP bank is currently selected; use EZPICK.'
      ELSEIF ( IER .EQ. EZS_BANK_EXTENDED )     THEN
        STRING=' SRCP bank has been EXTENDED'
      ELSEIF ( IER .EQ. EZS_MAX_PARAMS )        THEN
        STRING=' Maximum number of parameters/bank reached'
      ELSEIF ( IER .EQ. EZS_MAX_VALUES )        THEN
        STRING=' Maximum number of values/bank reached'
      ELSEIF ( IER .EQ. EZS_ENDOF_FILE )        THEN
        STRING=' End-Of-File'
      ELSEIF ( IER .EQ. EZS_ENDOF_DATA )        THEN
        STRING=' End-Of-Data in RCP bank (\STOP reached)'
      ELSE
        STRING=' Unrecognized error code'
      ENDIF
  999 RETURN
      END
