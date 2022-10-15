      FUNCTION LSQ_EVENT_LAMBDA3()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DIAGANOLIZE KAPPA*LAMBDA
C-                         SEE what effect this has on live live
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LSQ_EVENT_LAMBDA3
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL DO_LSQ_ANAL
      INTEGER IER,DMPUNI
      INCLUDE 'D0$INC:DUMP.INC'
      INTEGER INDEX,NROWS,NCOLS
      DOUBLE PRECISION VAL
      REAL    E0 , EVINV
C
      INTEGER NEV,I,J,IFAIL,JJ
      INTEGER LSQ_DUMP_EVENTS
C
      INTEGER NLIVE
      PARAMETER( NLIVE = 5 )
      REAL    EIGEN_DEP(NLIVE),EIGEN2(NLIVE,NLIVE)
      REAL    LAMBDA(NLIVE,NLIVE)
      REAL    WORK(NLIVE,NLIVE)
C----------------------------------------------------------------------
      LSQ_EVENT_LAMBDA3=.TRUE.
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('LSQ_RCP')
        CALL EZGET_l('DO_LSQ_ANAL',DO_LSQ_ANAL,IER)
        CALL EZGET_i('LSQ_DUMP_EVENTS',LSQ_DUMP_EVENTS,IER)
        CALL EZRSET
        DUNIT = 15
C
C       initialisation: open file & booking histogramms
C
      ENDIF
C----------------------------------------------------------------------
      IF(DO_LSQ_ANAL)CALL LSQ_ANAL        ! ANALYZE
C
      CALL LSQ_MATRIX_MULTIPLY('KAPPA','LAMBDA','KAPPA_LAMBDA',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'KAPPA_LAMBDA',NLIVE,'(D10.3)',IER)
C
      CALL LSQ_MATRIX_DIAG('KAPPA_LAMBDA','UNIT_KAPPA_LAMBDA',
     &  'EIGEN_KAPPA_LAMBDA',IER)
C
      CALL LSQ_MATRIX_TRANSFORM('LIVE_ERROR_MATRIX',
     &  'UNIT_KAPPA_LAMBDA','DIAG_LIVEKL',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'DIAG_LIVEKL',NLIVE,'(D10.3)',IER)
C
  999 RETURN
      END
