      FUNCTION LSQ_EVENT_LAMBDA1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Event loop for determining Lambda tensors
C-   DIAGONALIZE LAMBDA.
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
      LOGICAL LSQ_EVENT_LAMBDA1
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
      INTEGER IERR
C----------------------------------------------------------------------
      INTEGER NLIVE
      PARAMETER( NLIVE = 5 )
      REAL    LAMBDA(NLIVE,NLIVE),DLIVE(NLIVE,NLIVE),LIVE(NLIVE,NLIVE)
      COMPLEX UMAT(NLIVE,NLIVE),UMATI(NLIVE,NLIVE),EIGEN(NLIVE)
      COMPLEX PROD(NLIVE,NLIVE),TEMP(NLIVE,NLIVE)
      COMPLEX DIAG_LAMBDA(NLIVE,NLIVE)
      COMPLEX DIAG_DLIVE(NLIVE,NLIVE)
      COMPLEX DIAG_LIVE(NLIVE,NLIVE)
      REAL    EIGR(NLIVE),EIGI(NLIVE),ZR(NLIVE,NLIVE)
      REAL    WORK(NLIVE,NLIVE)
      INTEGER A,B
C----------------------------------------------------------------------
      LSQ_EVENT_LAMBDA1=.TRUE.
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
      CALL LSQ_MATRIX_PRINT(DUNIT,'LAMBDA',NLIVE,'(D10.3)',IER)
      CALL LSQ_MATRIX_GET('LAMBDA',LAMBDA,NLIVE,NLIVE,1,IER)
C
      CALL EISRG1(NLIVE,NLIVE,LAMBDA,EIGR,EIGI,ZR,IERR,WORK)
C
C
C ****  RESTIRE LAMBDA AFTER EISRG1
C
      CALL LSQ_MATRIX_GET('LAMBDA',LAMBDA,NLIVE,NLIVE,1,IER)
      CALL LSQ_MATRIX_GET('LIVE_ERROR_MATRIX',LIVE,
     &  NLIVE,NLIVE,1,IER)
      CALL LSQ_MATRIX_GET('DEPLIVE_ERROR_MATRIX',DLIVE,
     &  NLIVE,NLIVE,1,IER)
C
      DO I =  1, NLIVE
        EIGEN(I) = CMPLX(EIGR(I),EIGI(I))
      ENDDO
      I = 0
      DO WHILE  (I .LE. NLIVE)
        I = I + 1
        IF ( ABS(EIGI(I)).GT.0.0 ) THEN
C
C ****  COMPLEX EIGEN VALUE. NEXT ONE WILL ALSO BE COMPLEX.
C
          DO J = 1, NLIVE
            UMAT(J,I) = CMPLX(ZR(J,I),ZR(J,I+1))
            UMAT(J,I+1) = CMPLX(ZR(J,I), - ZR(J,I+1))
          ENDDO
C
          I = I + 1   !DONE
C
        ELSE
C
C ****  REAL EIGEN VALUE. COPY REAL EIGEN VECTOR TO UMAT
C
          DO J = 1 , NLIVE
            UMAT(J,I) = ZR(J,I)
          ENDDO
        ENDIF
      ENDDO
      DO I = 1 , NLIVE
        DO J = 1 , NLIVE
          UMATI(I,J) = UMAT(I,J)
        ENDDO
      ENDDO
C
C ****  NOW TO SEE IF TRANSFORMATION WORKS.
C
      CALL CINV(NLIVE,UMATI,NLIVE,WORK,IER)
      DO I = 1 , NLIVE
        DO J = 1 , NLIVE
          DIAG_LAMBDA(I,J) = 0.0
          DIAG_LIVE(I,J) = 0.0
          DIAG_DLIVE(I,J) = 0.0
          TEMP(I,J) =0.0
          DO A = 1 , NLIVE
            TEMP(I,J) = TEMP(I,J)+UMATI(I,A)*UMAT(A,J)
            DO B = 1 , NLIVE
C
              DIAG_LAMBDA(I,J) = DIAG_LAMBDA(I,J) + 
     &          UMATI(I,A)*LAMBDA(A,B)* UMAT(B,J)
C
              DIAG_LIVE(I,J) = DIAG_LIVE(I,J) + 
     &          UMATI(I,A)*LIVE(A,B)* UMAT(B,J)
C
              DIAG_DLIVE(I,J) = DIAG_DLIVE(I,J) + 
     &          UMATI(I,A)*DLIVE(A,B)* UMAT(B,J)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
  999 RETURN
      END
