      SUBROUTINE L2_KEEP_CD_RAW( PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Forbid dropping of raw data in this script
C-
C-   Inputs  : PARAM_SET_NUMBER : # of parameter set to use
C-             HARDWARE:          mask of set bits for LV1 trigger which started
C-                                  this filter.
C-   Outputs : RESULT_FLAG :      Flag set to TRUE when we want to pass tool
C-                                  under this PARAM_SET_NUMBER
C-             EXTRA_FLAG  :      Set to TRUE when we want to pass event and
C-                                  do no further filtering. (NOT IMPLEMENTED)
C-   Controls:
C-
C-   Created 15-OCT-1993   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL EXTRA_FLAG,RESULT_FLAG
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      CHARACTER*80 MSG
      CHARACTER*64 STRING
C
      INTEGER IP,NPARIN,IER
      LOGICAL EZERROR,OK
      INTEGER I,J,N,NCHR
C----------------------------------------------------------------------
      EXTRA_FLAG = .FALSE.
      RESULT_FLAG = .FALSE.
C
C...first, carefully retrieve cuts from RCP
      IP = PARAM_SET_NUMBER !I'm getting tired of typing this
      CALL EZPICK('L2_KEEP_CD_RAW') ! downloaded from config file (.FILTs)
      OK = .NOT.EZERROR(IER)
      IF (OK) THEN
C...is IP consistent with the number of sets which exist?
        IF (IER .EQ. 0) CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
        IF (IER.EQ.0) THEN
          IF ((IP.LE.0).OR.(IP.GT.NPARIN)) THEN
            WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &        PARAM_SET_NUMBER, ' but only had ',NPARIN
            CALL ERRMSG('L2_KEEP_CD_RAW','L2_KEEP_CD_RAW',MSG,'F')
            GO TO 999
          ENDIF
C...Get the parameters from the IPth set
          IF (IER.EQ.0) CALL EZGETS('RAW_TO_KEEP',IP,STRING,NCHR,IER)
        ENDIF
      ENDIF
      IF (.NOT.OK) THEN
        CALL ERRMSG('L2_KEEP_CD_RAW','L2_KEEP_CD_RAW',
     &    'Couldn''t find bank','F')
      ELSEIF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('L2_KEEP_CD_RAW','L2_KEEP_CD_RAW_PARAMETERS',
     &          'Couldn''t find parameter','F')
      ENDIF
      CALL L2_DONT_DROP_RAW(STRING)  !forbids dropping for CDDn's mentioned
  999 CONTINUE
      RESULT_FLAG = .TRUE.
      IF (OK) CALL EZRSET
      RETURN
      END
