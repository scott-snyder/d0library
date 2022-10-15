      FUNCTION LSQ_EVENT_LAMBDA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Event loop for determining Lambda tensors
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-FEB-1992   Rajendran Raja
C-   Updated   2-Sep-1992   Herbert Greenlee
C-      Changed OPEN to D0OPEN.
C-   Updated   2-Feb-1993   Herbert Greenlee
C-      Added RCP parameters LYR_LIVE, LYR_DEAD, ON
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LSQ_EVENT_LAMBDA
      LOGICAL FIRST, OPENOK
      DATA FIRST/.TRUE./
      LOGICAL DO_LSQ_ANAL
      INTEGER IER,DMPUNI
      INTEGER IERR
      INCLUDE 'D0$INC:LSQ_EVENT.INC'
      INCLUDE 'D0$INC:DUMP.INC'
      INTEGER INDEX,NROWS,NCOLS
      DOUBLE PRECISION VAL
C
      INTEGER LSQ_DUMP_EVENTS
C
      INTEGER UNIT
      LOGICAL EOF
      INTEGER JJ
C----------------------------------------------------------------------
      LSQ_EVENT_LAMBDA = .TRUE.
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('LSQ_RCP')
        CALL EZGET_l('DO_LSQ_ANAL',DO_LSQ_ANAL,IER)
        CALL EZGET_i('LSQ_DUMP_EVENTS',LSQ_DUMP_EVENTS,IER)
        CALL EZGETA('LYR_LIVE',1,8,1,LYR_LIVE,IER)
        CALL EZGETA('LYR_DEAD',1,8,1,LYR_DEAD,IER)
        CALL EZGETA('ON',1,25,1,ON,IER)
        CALL EZRSET
        DUNIT = 15
C
C       initialisation: open file & booking histogramms
C
        UNIT = 13
        CALL D0OPEN (UNIT,'FOR013_DAT','IO',OPENOK)
        IF(.NOT.OPENOK)GO TO 998
        NEV=0
C
        CALL LSQ_BKMATRIX('LIVE_LIVET',NLIVE,NLIVE,IER)
        CALL LSQ_BKMATRIX('DEPOSITED_LIVET',NLIVE,NLIVE,IER)
        CALL LSQ_BKMATRIX('DEPOSITED_DEPOSITEDT',NLIVE,NLIVE,IER)
        CALL LSQ_BKMATRIX('LIVE_AVERAGE',NLIVE,1,IER)
        CALL LSQ_BKMATRIX('DEPOSITED_AVERAGE',NLIVE,1,IER)
        CALL LSQ_BKMATRIX('ES_EST',NLIVE,NLIVE,IER)
        CALL LSQ_BKMATRIX('EPS_EPST',NLIVE,NLIVE,IER)
        CALL LSQ_BKMATRIX('ES_AVERAGE',NLIVE,1,IER)
        CALL LSQ_BKMATRIX('EPS_AVERAGE',NLIVE,1,IER)
C
      ENDIF
C----------------------------------------------------------------------
      IF(DO_LSQ_ANAL)CALL LSQ_ANAL        ! ANALYZE
C
      DO JJ=1,10000
        CALL LSQ_READ_EVENT(UNIT,EOF)
        IF ( EOF ) THEN
          GO TO 998
        ENDIF
        CALL LSQ_MATRIX_FILL('LIVE',ELIVE,NLIVE,1,1,IER)
        CALL LSQ_MATRIX_FILL('DEAD',EDEAD,NLIVE,1,1,IER)
        CALL LSQ_MATRIX_ADD('LIVE','DEAD','DEPOSITED',1.0,1.0,IER)
        CALL LSQ_VECTOR_DIRECT_PRODUCT('LIVE','LIVE','LIVE2',IER)
        CALL LSQ_VECTOR_DIRECT_PRODUCT('DEPOSITED','LIVE',
     &    'DLIVE2',IER)
        CALL LSQ_VECTOR_DIRECT_PRODUCT('DEPOSITED','DEPOSITED',
     &    'DEPOS2',IER)
C
        CALL LSQ_MATRIX_ADD('LIVE2','LIVE_LIVET','LIVE_LIVET',
     &    1.,1.,IER)
C
        CALL LSQ_MATRIX_ADD('DLIVE2','DEPOSITED_LIVET',
     &    'DEPOSITED_LIVET', 1.,1.,IER)
C
        CALL LSQ_MATRIX_ADD('DEPOS2','DEPOSITED_DEPOSITEDT',
     &    'DEPOSITED_DEPOSITEDT', 1.,1.,IER)
C
        CALL LSQ_MATRIX_ADD('LIVE_AVERAGE','LIVE','LIVE_AVERAGE',
     &    1.0,1.0,IER)
C
        CALL LSQ_MATRIX_ADD('DEPOSITED_AVERAGE','DEPOSITED',
     &    'DEPOSITED_AVERAGE',1.0,1.0,IER)
      ENDDO
C----------------------------------------------------------------------
  998 CONTINUE
C
C ****  IF HERE EOF
C
      EVINV = 1./FLOAT(NEV)
      CALL LSQ_SCALAR_MULTIPLY('LIVE_LIVET',EVINV,'LIVE_LIVET',IER)
      CALL LSQ_SCALAR_MULTIPLY('DEPOSITED_LIVET',EVINV,
     &  'DEPOSITED_LIVET',IER)
      CALL LSQ_SCALAR_MULTIPLY('DEPOSITED_DEPOSITEDT',EVINV,
     &  'DEPOSITED_DEPOSITEDT',IER)
C
      CALL LSQ_SCALAR_MULTIPLY('LIVE_AVERAGE',EVINV,'LIVE_AVERAGE',
     &  IER)
      CALL LSQ_SCALAR_MULTIPLY('DEPOSITED_AVERAGE',EVINV,
     &  'DEPOSITED_AVERAGE',IER)
      CALL LSQ_VECTOR_DIRECT_PRODUCT('LIVE_AVERAGE','LIVE_AVERAGE',
     &  'LIVE_LIVE_AVERAGE',IER)
      CALL LSQ_VECTOR_DIRECT_PRODUCT('DEPOSITED_AVERAGE',
     &  'DEPOSITED_AVERAGE','DEPOSITED_DEPOSITED_AVERAGE',IER)
      CALL LSQ_VECTOR_DIRECT_PRODUCT('DEPOSITED_AVERAGE',
     &  'LIVE_AVERAGE','DEPOSITED_LIVE_AVERAGE',IER)
C
      CALL LSQ_MATRIX_ADD('LIVE_LIVET','LIVE_LIVE_AVERAGE',
     &  'LIVE_ERROR_MATRIX',1.0,-1.0,IER)
C
      CALL LSQ_MATRIX_ADD('DEPOSITED_DEPOSITEDT',
     &  'DEPOSITED_DEPOSITED_AVERAGE', 'DEPOSITED_ERROR_MATRIX',
     &  1.0,-1.0,IER)
C
      CALL LSQ_MATRIX_ADD('DEPOSITED_LIVET',
     &  'DEPOSITED_LIVE_AVERAGE', 'DEPLIVE_ERROR_MATRIX',
     &  1.0,-1.0,IER)
C
      CALL LSQ_MATRIX_INVERT('LIVE_LIVET','M_INVERSE',IER)
      CALL LSQ_MATRIX_MULTIPLY('DEPOSITED_LIVET','M_INVERSE',
     &  'LAMBDA',IER)
C
      CALL LSQ_MATRIX_INVERT('DEPOSITED_DEPOSITEDT','M_INVERSE1',IER)
      CALL LSQ_MATRIX_TRANSPOSE('DEPOSITED_LIVET',
     &  'LIVE_DEPOSITEDT',IER)
      CALL LSQ_MATRIX_MULTIPLY('LIVE_DEPOSITEDT','M_INVERSE1',
     &  'KAPPA',IER)
C
      CALL LSQ_MATRIX_PRINT(DUNIT,'LAMBDA',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'KAPPA',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'LIVE_LIVET',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'DEPOSITED_LIVET',NLIVE,
     &  '(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'LIVE_DEPOSITEDT',NLIVE,
     &  '(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'DEPOSITED_DEPOSITEDT',NLIVE,
     &  '(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'LIVE_AVERAGE',NLIVE,
     &  '(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'DEPOSITED_AVERAGE',NLIVE,
     &  '(D10.3)',IER)
C
      REWIND UNIT
C
      NEV = 0
      DO JJ=1,10000
        CALL LSQ_READ_EVENT(UNIT,EOF)
        IF ( EOF ) THEN
          GO TO 996
        ENDIF
        CALL LSQ_MATRIX_FILL('LIVE',ELIVE,NLIVE,1,1,IER)
        CALL LSQ_MATRIX_FILL('DEAD',EDEAD,NLIVE,1,1,IER)
        CALL LSQ_MATRIX_ADD('LIVE','DEAD','DEPOSITED',1.0,1.0,IER)
C
        CALL LSQ_MATRIX_MULTIPLY('LAMBDA','LIVE','LAMBDA_LIVE',IER)
        CALL LSQ_MATRIX_MULTIPLY('KAPPA','DEPOSITED',
     &    'KAPPA_DEPOSITED',IER)
        CALL LSQ_MATRIX_ADD('DEPOSITED','LAMBDA_LIVE','ES',1.0,-1.0,IER)
        CALL LSQ_MATRIX_ADD('LIVE','KAPPA_DEPOSITED','EPS',1.0,-1.0,IER)
        CALL LSQ_MATRIX_ADD('ES','ES_AVERAGE','ES_AVERAGE',1.0,1.0,IER)
        CALL LSQ_MATRIX_ADD('EPS','EPS_AVERAGE','EPS_AVERAGE',
     &    1.0,1.0,IER)
C
        CALL LSQ_VECTOR_DIRECT_PRODUCT('ES','ES','ES2',IER)
        CALL LSQ_VECTOR_DIRECT_PRODUCT('EPS','EPS','EPS2',IER)
C
        CALL LSQ_MATRIX_ADD('ES2','ES_EST','ES_EST',
     &    1.,1.,IER)
        CALL LSQ_MATRIX_ADD('EPS2','EPS_EPST','EPS_EPST',
     &    1.,1.,IER)
C
      ENDDO
  996 CONTINUE
      EVINV = 1./FLOAT(NEV)
      CALL LSQ_SCALAR_MULTIPLY('ES_EST',EVINV,'ES_EST',IER)
      CALL LSQ_SCALAR_MULTIPLY('EPS_EPST',EVINV,'EPS_EPST',IER)
C
      CALL LSQ_SCALAR_MULTIPLY('ES_AVERAGE',EVINV,'ES_AVERAGE',IER)
      CALL LSQ_SCALAR_MULTIPLY('EPS_AVERAGE',EVINV,'EPS_AVERAGE',IER)
C
      CALL LSQ_MATRIX_PRINT(DUNIT,'ES_EST',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'EPS_EPST',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'ES_AVERAGE',1,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'EPS_AVERAGE',1,'(D10.3)',IER)
C
      CALL LSQ_MATRIX_MULTIPLY('KAPPA','LAMBDA','KAPPA_LAMBDA',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'KAPPA_LAMBDA',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_MULTIPLY('LAMBDA','KAPPA','LAMBDA_KAPPA',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'LAMBDA_KAPPA',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'LAMBDA_KAPPA',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_ADD('KAPPA_LAMBDA','LAMBDA_KAPPA','LKDIFF',
     &  1.0,-1.0,IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'LKDIFF',NLIVE,'(D10.3)',IER)
C
      CALL LSQ_MATRIX_INVERT('LAMBDA','LAMBDA_INVERSE',IER)
      CALL LSQ_MATRIX_ADD('DEPOSITED_DEPOSITEDT','ES_EST',
     &  'DEP_DEPT_SUB',1.0,-1.0,IER)
      CALL LSQ_MATRIX_MULTIPLY('LAMBDA_INVERSE','DEP_DEPT_SUB',
     &  'RES1',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'RES1',NLIVE,'(D10.3)',IER)
C
C
C ****  now calculate residual error matrices
C
      CALL LSQ_VECTOR_DIRECT_PRODUCT('ES_AVERAGE',
     &  'ES_AVERAGE','ES_ES_AVERAGE',IER)
      CALL LSQ_VECTOR_DIRECT_PRODUCT('EPS_AVERAGE',
     &  'EPS_AVERAGE','EPS_EPS_AVERAGE',IER)
C
      CALL LSQ_MATRIX_ADD('ES_EST', 'ES_ES_AVERAGE', 
     &  'ES_ERROR_MATRIX',1.0,-1.0,IER)
C
      CALL LSQ_MATRIX_ADD('EPS_EPST', 'EPS_EPS_AVERAGE', 
     &  'EPS_ERROR_MATRIX',1.0,-1.0,IER)
C
      CALL LSQ_MATRIX_TRANSPOSE('LAMBDA','LAMBDA_TRANSPOSE',IER)
      CALL LSQ_MATRIX_TRANSPOSE('KAPPA','KAPPA_TRANSPOSE',IER)
C
      CALL LSQ_MATRIX_MULTIPLY('LAMBDA',
     &  'LIVE_ERROR_MATRIX','LAMBDA_LIVE_ERROR',IERR)
      CALL LSQ_MATRIX_MULTIPLY('LAMBDA_LIVE_ERROR',
     &  'LAMBDA_TRANSPOSE',
     &  'LAMBDA_LIVE_ERROR_LAMBDA',IER)
      CALL LSQ_MATRIX_ADD('LAMBDA_LIVE_ERROR_LAMBDA','ES_ERROR_MATRIX',
     &  'EQUAL_DEPOSITED_ERROR_MATRIX',1.0,1.0,IERR)
      CALL LSQ_MATRIX_PRINT(DUNIT,'EQUAL_DEPOSITED_ERROR_MATRIX',
     &  NLIVE,'(D10.3)',IERR)
      CALL LSQ_MATRIX_PRINT(DUNIT,'DEPOSITED_ERROR_MATRIX',
     &  NLIVE,'(D10.3)',IERR)
C
C ****  NOW CALCULATE ELL
C
      CALL LSQ_MATRIX_MULTIPLY('KAPPA','DEPOSITED_ERROR_MATRIX',
     &  'KAPPA_DEPOSITED_ERROR',IERR)
      CALL LSQ_MATRIX_MULTIPLY('KAPPA_DEPOSITED_ERROR',
     &  'KAPPA_TRANSPOSE','KAPPA_DEPOSITED_ERROR_KAPPA',IERR)
      CALL LSQ_MATRIX_ADD('KAPPA_DEPOSITED_ERROR_KAPPA',
     &  'EPS_ERROR_MATRIX','EQUAL_LIVE_ERROR_MATRIX',
     &  1.0,1.0,IERR)
      CALL LSQ_MATRIX_PRINT(DUNIT,'EQUAL_LIVE_ERROR_MATRIX',
     &  NLIVE,'(D10.3)',IERR)
      CALL LSQ_MATRIX_PRINT(DUNIT,'LIVE_ERROR_MATRIX',
     &  NLIVE,'(D10.3)',IERR)
C
C ****  now diagonalize ELL
C
      CALL LSQ_MATRIX_DIAG('LIVE_ERROR_MATRIX','UNITARY_LIVE',
     &  'EIGEN_LIVE',IER)
C
      CALL LSQ_MATRIX_TRANSFORM('LIVE_ERROR_MATRIX','UNITARY_LIVE',
     &  'LIVE_ERROR_DIAG',IER)
      CALL LSQ_MATRIX_TRANSFORM('EQUAL_LIVE_ERROR_MATRIX',
     &  'UNITARY_LIVE', 'EQUAL_LIVE_ERROR_DIAG',IER)
      CALL LSQ_MATRIX_TRANSFORM('EPS_ERROR_MATRIX',
     &  'UNITARY_LIVE','EPS_ERROR_DIAG',IER)
C
      CALL LSQ_MATRIX_PRINT(DUNIT,'EIGEN_LIVE',
     &  1,'(D10.3)',IERR)
      CALL LSQ_MATRIX_PRINT(DUNIT,'UNITARY_LIVE',
     &  NLIVE,'(D10.3)',IERR)
      CALL LSQ_MATRIX_PRINT(DUNIT,'EPS_ERROR_MATRIX',
     &  NLIVE,'(D10.3)',IERR)
      CALL LSQ_MATRIX_PRINT(DUNIT,'ES_ERROR_MATRIX',
     &  NLIVE,'(D10.3)',IERR)
      CALL LSQ_MATRIX_PRINT(DUNIT,'EPS_ERROR_DIAG',
     &  NLIVE,'(D10.3)',IERR)
      CALL LSQ_MATRIX_PRINT(DUNIT,'EQUAL_LIVE_ERROR_DIAG',
     &  NLIVE,'(D10.3)',IERR)
C
      CALL LSQ_MATRIX_TRANSFORM('LIVE_ERROR_MATRIX','UNITARY_LIVE',
     &  'LIVE_ERROR_DIAG',IER)
  999 RETURN
      END
