      SUBROUTINE L2_PRESCALE(PSN,BITS,RESULT,EXTRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to prescale the level2 trigger
C-
C-   Inputs  : PSN    parameter set number
C-           : BITS   mask of set bits for LV1 trigger which
C-                           started this filter.
C-   Outputs : RESULT T/F for pass/fail this tool under this PSN
C-             EXTRA  n/a
C-   Controls: 
C-
C-   Created   8-JAN-1992   Drew Baden
C-   Updated  23-JAN-1994   James T. Linnemann  RCP control for simulation 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INTEGER PSN,BITS
      LOGICAL EXTRA,RESULT
C
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      CHARACTER*80 MSG
      LOGICAL APPLY_L2_PRESCALE,L2_PRESCALE_FROM_DATA
      LOGICAL OK,EZERROR,FIRST,FOUND,ONLINE
      INTEGER IER,FACTOR,NPARIN,NCHR
      REAL TEST,SEED,RNDM
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C     set up seed for random numbers
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL L2_RNDM_SET  !set random number generator (depending on node #)
      ENDIF
C
C     default is to FLUNK
C
      RESULT = .FALSE.
C
C     get prescale factor for this parameter set
C
      CALL EZPICK('L2_PRESCALE') 
      OK = .NOT.EZERROR(IER)
      IF (.NOT.OK) THEN
        WRITE(MSG,'(''Could not find L2_PRESCALE BANK '')') 
        CALL ERRMSG('L2_PRESCALE','L2_PRESCALE',MSG,'F')
        GOTO 999
      ENDIF
C
C     make sure the parameter set is within the bounds of the
C     RCP - otherwise, something has happened!!!
C
      CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
      IF (IER.EQ.0) THEN
        IF ( PSN.LT.1 .OR. PSN.GT.NPARIN ) THEN
          WRITE(MSG,
     &      '('' Parameter set ('',I5,'') outside limits 0,'',I5)') 
     &      PSN,NPARIN
          CALL ERRMSG('L2_PRESCALE','L2_PRESCALE',MSG,'W')
          GO TO 999
        ENDIF
      ENDIF
C
C     finally, get the prescale factor for this parameter set
C
      CALL EZGETA('L2_PRESCALE_BY',PSN,PSN,1,FACTOR,IER)
      IF (IER.NE.0) THEN
        CALL ERRMSG('L2_PRESCALE','L2_PRESCALE',
     &    'Could not find L2_PRESCALE parameter ','F')
        GOTO 999
      ENDIF
C
C     here is where we do the calculation
C
      TEST = FACTOR * RNDM(SEED)
      IF (TEST.LT.1.0) RESULT = .TRUE.
C
C...now, if doing a simulation, override actual result on request
      CALL EZPICK_NOMSG('L2SIM_RCP',IER)
      IF (IER.EQ.0) THEN
        APPLY_L2_PRESCALE = .TRUE.  !default
        CALL EZGET('APPLY_L2_PRESCALE',APPLY_L2_PRESCALE,IER)
        L2_PRESCALE_FROM_DATA = .FALSE. !default
        IF(IER.EQ.0) 
     &    CALL EZGET('L2_PRESCALE_FROM_DATA',L2_PRESCALE_FROM_DATA,IER)
        IF(.NOT.APPLY_L2_PRESCALE) THEN
          RESULT = .TRUE.   !no prescale => always pass event
        ELSEIF (L2_PRESCALE_FROM_DATA) THEN
          CALL L2_PRE_GET_ONLINE(FOUND,ONLINE)
          IF (FOUND) RESULT = ONLINE
        ENDIF
        CALL EZRSET
      ENDIF
C
C     and that's all folks
C
  999 CONTINUE
      CALL EZRSET
      RETURN
      END
