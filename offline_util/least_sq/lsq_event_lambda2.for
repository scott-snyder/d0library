      FUNCTION LSQ_EVENT_LAMBDA2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Event loop for determining Lambda tensors
C-   DIAGONALIZE LAMBDA. Technique here is to diagonalize both
C-   Deposited_deposited and live live simultaneously. This requires a 
C-   non-unitary transformation. Hopefully this will diagonalize Lambda as well
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
      LOGICAL LSQ_EVENT_LAMBDA2
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
      LSQ_EVENT_LAMBDA2=.TRUE.
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('LSQ_RCP')
        CALL EZGET('DO_LSQ_ANAL',DO_LSQ_ANAL,IER)
        CALL EZGET('LSQ_DUMP_EVENTS',LSQ_DUMP_EVENTS,IER)
        CALL EZRSET
        DUNIT = 15
C
C       initialisation: open file & booking histogramms
C
      ENDIF
C----------------------------------------------------------------------
      IF(DO_LSQ_ANAL)CALL LSQ_ANAL        ! ANALYZE
C
  998 CONTINUE
C
C ****  IF HERE EOF
C
      CALL LSQ_MATRIX_GET('LAMBDA',LAMBDA,NLIVE,NLIVE,1,IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'LAMBDA',NLIVE,'(D10.3)',IER)
C
C
C ****  deposited_error_matrix,Live_error_matrix
C
      CALL LSQ_MATRIX_DIAG('DEPOSITED_ERROR_MATRIX','UNIT_DEP',
     &  'EIGEN_DEP',IER)
      CALL LSQ_MATRIX_TRANSPOSE('UNIT_DEP','UNIT_DEP_T',IER)
C
      CALL LSQ_MATRIX_PRINT(DUNIT,'UNIT_DEP',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'UNIT_DEP_T',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'EIGEN_DEP',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_GET('EIGEN_DEP',EIGEN_DEP,NLIVE,1,1,IER)
C
      DO I = 1 , NLIVE
        DO J = 1 , NLIVE
          EIGEN2(I,J) = 0.0
          IF ( I.EQ.J ) THEN
            EIGEN2(I,J) = SQRT(EIGEN_DEP(I))
          ENDIF
        ENDDO
      ENDDO
C
      CALL LSQ_MATRIX_FILL('EIGEN2',EIGEN2,NLIVE,NLIVE,1,IER)
C
      CALL LSQ_MATRIX_MULTIPLY('EIGEN2','UNIT_DEP_T','TRANSF1',IER)
      CALL LSQ_MATRIX_INVERT('TRANSF1','TRANSF1_I',IER)
C
      CALL LSQ_MATRIX_MULTIPLY('TRANSF1','LIVE_ERROR_MATRIX',
     &  'TEMP_LIVE',IER)
      CALL LSQ_MATRIX_MULTIPLY('TEMP_LIVE','TRANSF1_I','LIVE1',IER)
C
C ****  LIVE 1 SHOULD BE SYMMETRIC AND DIAGONALIZABLE
C
      CALL LSQ_MATRIX_MULTIPLY('TRANSF1','DEPOSITED_ERROR_MATRIX',
     &  'TEMP_DEP',IER)
      CALL LSQ_MATRIX_MULTIPLY('TEMP_DEP','TRANSF1_I','DEP1',IER)
C
C ****  DEP1 SHOULD BE UNIT MATRIX
C
      CALL LSQ_MATRIX_PRINT(DUNIT,'LIVE1',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'DEP1',NLIVE,'(D10.3)',IER)
C
      CALL LSQ_MATRIX_DIAG('LIVE1','UNIT_LIVE1','EIGEN_LIVE1',IER)
      CALL LSQ_MATRIX_TRANSFORM('DEP1','UNIT_LIVE1','DEP2',IER)
      CALL LSQ_MATRIX_TRANSFORM('LIVE1','UNIT_LIVE1','LIVE2',IER)
C
C ****  DEP2 SHOULD STILL BE UNIT MATRIX
C
      CALL LSQ_MATRIX_PRINT(DUNIT,'DEP2',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_PRINT(DUNIT,'LIVE2',NLIVE,'(D10.3)',IER)
  999 RETURN
      END
