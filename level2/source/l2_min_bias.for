      SUBROUTINE L2_MIN_BIAS
     &         (PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check if have a good min bias event
C-
C-   Inputs  : PARAM_SET_NUMBER : Number in series of parameters to use.
C-             HARDWARE : Mask with bit set for Level-1 trigger which
C-                        started this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when TOOL wants event passed
C-             EXTRA_FLAG :  Not used
C-   Controls: None
C-
C-   Created   8-AUG-1992   James T. Linnemann
C-   Updated  27-JAN-1993   Jeffrey Bantly  fully implement L0 Slow Z options
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL RESULT_FLAG,EXTRA_FLAG
      REAL    L2_VERT
      REAL    FASTZ,SLOWZ
      INTEGER FAST_FLAG,SLOW_FLAG
      LOGICAL OK
      LOGICAL L0_CUTS_HAS
      LOGICAL SINGLE, MULTIPLE, LIKELY, SURELY, CENTER
      LOGICAL SLOW_SUM
      REAL    MAX_Z
      CHARACTER*24 VERTEX
      CHARACTER*6 SUB
      INTEGER IER,IP,NPARIN,NCHR,TRULEN
      LOGICAL EZERROR
      CHARACTER*80 MSG
      CHARACTER*7 A_FAST,A_GOOD,A_SLOW,A_LIKE,A_SURE,A_SING,A_MULT
      PARAMETER( A_FAST = 'FAST' )
      PARAMETER( A_GOOD = 'GOOD' )
      PARAMETER( A_SLOW = 'SLOW' )
      PARAMETER( A_LIKE = 'LIKE' )
      PARAMETER( A_SURE = 'SURE' )
      PARAMETER( A_SING = 'SING' )
      PARAMETER( A_MULT = 'MULT' )
C----------------------------------------------------------------------
C...statement function returns true if substring SUB found in SHAPE_CUTS
      L0_CUTS_HAS(SUB) = INDEX(VERTEX,SUB(1:TRULEN(SUB))).NE.0
C----------------------------------------------------------------------
      EXTRA_FLAG=.FALSE.
      RESULT_FLAG=.FALSE.
C...retrieve cuts from RCP
      IP=PARAM_SET_NUMBER
      CALL EZPICK('L2_MIN_BIAS')
      OK=.NOT.EZERROR(IER)
      IF (OK) THEN
C...is IP consistent with the number of sets which exist?
        IF (IER .EQ. 0) CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
        IF (IER.EQ.0) THEN
          IF ((IP.LE.0).OR.(IP.GT.NPARIN)) THEN
            WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &        PARAM_SET_NUMBER, ' but only had ',NPARIN
            CALL ERRMSG('L2_MIN_BIAS','L2_MIN_BIAS',MSG,'F')
            GO TO 999
          ENDIF
C...Get the parameters from the IPth set
          IF (IER.EQ.0) CALL EZGETA('ABS_Z_MAX',IP,IP,1,MAX_Z,IER)
          IF (IER.EQ.0) CALL EZGETS('VERTEX_QUALITY',IP,VERTEX,NCHR,IER)
          IF (MAX_Z.LT. 0.0) MAX_Z=ABS(MAX_Z)
        ENDIF
      ENDIF
      IF (.NOT.OK) THEN
        CALL ERRMSG('L2_MIN_BIAS','L2_MIN_BIAS',
     &    'Couldn''t find bank','F')
      ELSEIF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('L2_MIN_BIAS','L2_MIN_BIAS_PARAMETERS',
     &          'Couldn''t find parameter','F')
      ENDIF
C...get any info available from level 0; will try to book bank if it's not there
      CALL GTL0VT(FASTZ,FAST_FLAG,SLOWZ,SLOW_FLAG,OK)
C...fastz cuts will be applied
      IF ( L0_CUTS_HAS(A_FAST) ) THEN
        IF ( L0_CUTS_HAS(A_GOOD) ) THEN
          IF ( FAST_FLAG.EQ.1 ) RESULT_FLAG = .TRUE.
        ENDIF
        IF ( ABS(FASTZ).GT.MAX_Z ) RESULT_FLAG = .FALSE.
        GOTO 999
      ENDIF
C
C...slow z cuts will be applied
      IF ( L0_CUTS_HAS(A_SLOW) ) THEN
        IF ( SLOW_FLAG.LE.0 .OR. SLOW_FLAG.GT.4 ) THEN
          RESULT_FLAG = .FALSE.
          GOTO 999
        ENDIF
        IF ( L0_CUTS_HAS(A_GOOD) ) THEN
          RESULT_FLAG = .TRUE.
        ELSE
C...slow z multiple interaction flag cuts will be applied
          SINGLE = L0_CUTS_HAS(A_SING)
          MULTIPLE = L0_CUTS_HAS(A_MULT)
          SURELY = L0_CUTS_HAS(A_SURE)
          LIKELY = L0_CUTS_HAS(A_LIKE)
          IF ( SINGLE ) THEN
            IF ( SURELY ) THEN
              IF ( SLOW_FLAG.EQ.1 ) RESULT_FLAG = .TRUE.
            ELSEIF ( LIKELY ) THEN
              IF ( SLOW_FLAG.EQ.2 ) RESULT_FLAG = .TRUE.
            ELSE
              RESULT_FLAG = .FALSE.
            ENDIF
          ELSEIF ( MULTIPLE ) THEN
            IF ( SURELY ) THEN
              IF ( SLOW_FLAG.EQ.4 ) RESULT_FLAG = .TRUE.
            ELSEIF ( LIKELY ) THEN
              IF ( SLOW_FLAG.EQ.3 ) RESULT_FLAG = .TRUE.
            ELSE
              RESULT_FLAG = .FALSE.
            ENDIF
          ENDIF
C...slow z position cut is applied
          IF ( ABS(SLOWZ).GT.MAX_Z ) RESULT_FLAG = .FALSE.
        ENDIF
C
      ENDIF
C
  999 CONTINUE
      IF ( OK ) CALL EZRSET
      RETURN
      END
