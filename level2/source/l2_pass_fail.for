      SUBROUTINE L2_PASS_FAIL( PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PASS_FAIL TOOL :
C-            PASS or FAIL the event, depending on the parameter SHOULD_PASS
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
C-   Created 1-APR-1992   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL EXTRA_FLAG,RESULT_FLAG
      BYTE NEWPAR
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      CHARACTER*80 MSG
      INTEGER NUM_FOUND
      REAL    ET_MIN
      REAL    CUT1,CUT2   ! PASS_FAIL cuts of type real
      INTEGER ICUT1,ICUT2 ! PASS_FAIL cuts of type integer
      LOGICAL SHOULD_PASS,LOG2   ! PASS_FAIL parameters of type logical
      CHARACTER*64 CHAR1,CHAR2  ! PASS_FAIL parameters of type character
C
      INTEGER IP,NPARIN,IER
      LOGICAL EZERROR,OK
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INTEGER NGOT
      INTEGER NFOUND(ID_ALL:LAST_TYPE),ID,IFLAG
      REAL ET,ETA,THETA,PHI
      INTEGER I,J,N,NCHR
      REAL    E1,E2,PX1,PX2,PY1,PY2,PZ1,PZ2,MASS
      INTEGER NMAX
      PARAMETER(NMAX = 40)    !be generous
      INTEGER IORDER(NMAX),WORK(NMAX)
C----------------------------------------------------------------------
      EXTRA_FLAG = .FALSE.
      RESULT_FLAG = .FALSE.
C
C...first, carefully retrieve cut from RCP
      IP = PARAM_SET_NUMBER !I'm getting tired of typing this
      CALL EZPICK('L2_PASS_FAIL') ! downloaded from configuration file (.FILTs)
      OK = .NOT.EZERROR(IER)
      IF (OK) THEN
C...is IP consistent with the number of sets which exist?
        IF (IER .EQ. 0) CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
        IF (IER.EQ.0) THEN
          IF ((IP.LE.0).OR.(IP.GT.NPARIN)) THEN
            WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &        PARAM_SET_NUMBER, ' but only had ',NPARIN
            CALL ERRMSG('L2_PASS_FAIL','L2_PASS_FAIL',MSG,'W')
            GO TO 999
          ENDIF
C...Get the parameters from the IPth set
          IF (IER.EQ.0) CALL EZGETA('SHOULD_PASS',IP,IP,1,SHOULD_PASS,
     &      IER)
        ENDIF
      ENDIF
      IF (.NOT.OK) THEN
        CALL ERRMSG('L2_PASS_FAIL','L2_PASS_FAIL',
     &    'Couldn''t find bank','F')
      ELSEIF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('L2_PASS_FAIL','L2_PASS_FAIL_PARAMETERS',
     &          'Couldn''t find parameter','F')
      ENDIF

C
C...now you can actually do what was requested
  999 CONTINUE
      RESULT_FLAG = SHOULD_PASS
      IF (OK) CALL EZRSET
      RETURN
C#######################################################################
      ENTRY L2_PASS_FAIL_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : prepare download for L2_PASS_FAIL tool
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      RETURN
C#######################################################################
      ENTRY L2_PASS_FAIL_PARAMETERS(NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialization of downloaded parameters
C-      for L2_PASS_FAIL tool
C-      Always forget previously stored parameters
C-
C-   Inputs  : NEWPAR : [BYTE] if it's equal to zero, ignore
C-      this run begin--nothing new downloaded.  It's the number of sets of
C-      parameters downloaded for THIS tool.
C-   Outputs :
C-   Controls:
C-
C-   Created  29-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      RETURN
      END
