      SUBROUTINE L2_TEST(PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  FORCE_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tool that contains utilities useful for test
C-                         l2 nodes
C-
C-   Inputs  : PARAM_SET_NUMBER : Number in series of parameters to use.
C-             HARDWARE : Mask with bit set for Level-1 trigger which
C-                        started this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when TOOL wants event passed
C-             FORCE_FLAG :  Flag set to TRUE when TOOL wants event passed
C-                           without further filtering.
C-   Controls: None
C-
C-   Created  1-May-1992 Amber Boehnlein from TOOL1
C-   This version includes several test utilities for l2 commissioning.
C-   wait_a_bit for testing shadow nodes, and ABC for testing EDEBUG
C-   array handling and an error sender for testing the
C-   alarm server.  ASB, ATZ, MVSR.
C-   Updated   2-MAY-1992   James T. Linnemann  kill off common block
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL RESULT_FLAG,FORCE_FLAG
      INTEGER NUM_FOUND
      INTEGER IP,NPARIN,IER
      LOGICAL EZERROR,OK,PARAMS_CALLED,CALL_ABC
      INTEGER NPAR_PARAMS
      CHARACTER*80 MSG
      REAL DELAY_SEC
      INTEGER NCALLS,ERROR_1_OF
      CHARACTER*1 SEVERITY
      SAVE NCALLS,SEVERITY 
      DATA NCALLS/0/, SEVERITY/'W'/ 
C----------------------------------------------------------------------
      RESULT_FLAG = .TRUE.  !define initial state
      FORCE_FLAG = .FALSE.
C
C...first, carefully retrieve cuts from RCP
      IP = PARAM_SET_NUMBER !I'm getting tired of typing this
C
C...did parameters entry ever get called?
      CALL L2_TEST_GOT_PARAMETERS(PARAMS_CALLED,NPAR_PARAMS)
      IF (.NOT.PARAMS_CALLED) THEN
        CALL ERRMSG('NOT_INI','L2_TEST',
     &    'Parameters entry never called','W')
      ENDIF
      IF (NPAR_PARAMS.LE.0) THEN
        CALL ERRMSG('NOT_INI','L2_TEST',
     &    'Parameters entry called with .le. parameters','W')
      ENDIF
      CALL EZPICK('L2_TEST') ! downloaded from configuration file (.FILTs)
      OK = .NOT.EZERROR(IER)
      IF (OK) THEN
C...is IP consistent with the number of sets which exist?
        IF (IER .EQ. 0) CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
        IF (IER.EQ.0) THEN
          IF ((IP.LE.0).OR.(IP.GT.NPARIN)) THEN
            WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &        PARAM_SET_NUMBER, ' but only had ',NPARIN
            CALL ERRMSG('NPAR_BAD','L2_TEST',MSG,'W')
            GO TO 999
          ENDIF
C...Get the parameters from the IPth set
          IF (IER.EQ.0) CALL EZGETA('DELAY_SEC',IP,IP,1,DELAY_SEC,IER)
          IF (IER.EQ.0) CALL EZGETA('ERROR_1_OF',IP,IP,1,
     &      ERROR_1_OF,IER)
C...examples of getting other variable types
C          IF (IER.EQ.0) CALL EZGETS('CHAR1',IP,CHAR1,NCHR,IER)
C          IF (IER.EQ.0) CALL EZGETA('LOG2',IP,IP,1,LOG2,IER)
        ENDIF
      ENDIF
      IF (.NOT.OK) THEN
        CALL ERRMSG('NO_RCP','L2_TEST','Couldn''t find bank','F')
      ELSEIF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('L2_TEST','L2_TEST_PARAMETERS',
     &          'Couldn''t find parameter','F')
      ENDIF
      IF (OK) CALL EZRSET
C-------------------------------------------------------------------------
C now get to actually do something
C-------------------------------------------------------------------------
C
C ****  pause execution for a number of seconds defined in filter script
C
      CALL WAIT_A_BIT(DELAY_SEC)
C
C ****  send out an alarm every X calls where X is defined in the filter
C       script
      NCALLS = NCALLS + 1
      IF (ERROR_1_OF.GT.0) THEN
        IF (MOD(NCALLS,ERROR_1_OF).EQ.1) THEN
          WRITE(MSG,120)NCALLS !  encode serial call # into message
  120     FORMAT(' TEST MESSAGE ON CALL NUMBER: ',I7)
          CALL ERRMSG('L2TEST','TEST ERROR',
     &      MSG,SEVERITY)
        ENDIF
      ENDIF
C
C...call debugger testing routine; warning!!! this is an infinite loop
      CALL_ABC = .FALSE.
      IF (CALL_ABC) CALL ABC
  999 RETURN
      END
