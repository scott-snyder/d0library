      SUBROUTINE LSQ_DIAG_TRAN(UNITARY_MATRIX,EIGEN_VALUES,
     &  QUAN,DIAG_QUAN,UNIT_GAUSS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN A UNITARY MATRIX THAT IS OBTAINED FROM
C-   DIAGONALIZING AN ERROR MATRIX (LSQ_MATRIX_DIAG), This routine
C-   transforms the vector QUAN to the Vector DIAG_QUAN which are
C-   un-correlated provided the error matrix is the error matrix of the QUAN
C-   vector. A set of unit gaussian quantities (mean = zero, std devn = 1)
C-   derived from QUAN are also put out.
C-
C-   Inputs  : UNITARY_MATRIX matrix used to diagonalize the error matrix
C-             in LSQ_MATRIX_DIAG.
C-             EIGEN_VALUES . Vector of eigen values from the same
C-             diagonalization.
C-             QUAN = Vector of quantities to diagonalize.
C-   Outputs : DIAG_QUAN = Vector of diagonalized quantities
C-             UNIT_GAUSS = vector of Unit Gaussian quantities.
C-             IER = 1 UNITARY_MATRIX does not exist
C-             IER = 2 EIGEN_VALUES vector does not exist
C-             IER = 3 QUAN vector does not exist
C-
C-
C-   Controls:
C-
C-   Created  25-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
C
      CHARACTER*(*) UNITARY_MATRIX,EIGEN_VALUES,QUAN,DIAG_QUAN,
     &  UNIT_GAUSS
      INTEGER IER,IERR
      INTEGER INDEX_U,INDEX_E,INDEX_Q,INDEX_D,INDEX_G
      INTEGER LINK_U,LINK_E,LINK_Q,LINK_D,LINK_G
      LOGICAL LSQ_MATRIX_EXIST
C
      INTEGER IROW,ICOL,INDD,INDS
      DOUBLE PRECISION VAL,EIGEN
C----------------------------------------------------------------------
      IER = 0
      IF (.NOT.LSQ_MATRIX_EXIST(UNITARY_MATRIX,INDEX_U,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_DIAG_TRAN',
     &      ' Unitary Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF (.NOT.LSQ_MATRIX_EXIST(EIGEN_VALUES,INDEX_E,IER) ) THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_DIAG_TRAN',
     &      ' Eigen value vector does not exist ','W')
        RETURN
      ENDIF
C
      IF (.NOT.LSQ_MATRIX_EXIST(QUAN,INDEX_Q,IER) ) THEN
        IER = 3
        CALL ERRMSG('LSQ','LSQ_DIAG_TRAN',
     &      ' QUAN vector does not exist ','W')
        RETURN
      ENDIF
      IF((M_ROWS(INDEX_E).NE.M_ROWS(INDEX_Q)).OR.
     &  M_COLS(INDEX_E).NE.M_COLS(INDEX_Q))THEN
        IER = 4
        CALL ERRMSG('LSQ','LSQ_DIAG_TRAN',
     &      ' EIGEN VALUES VECTOR AND QUAN VECTOR MISMATCH ','W')

        RETURN
      ENDIF
C
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_MATRIX_TRANSPOSE(UNITARY_MATRIX,'WORK',IER)
      CALL LSQ_MATRIX_MULTIPLY('WORK',QUAN,DIAG_QUAN,IER)
      CALL LSQ_MATRIX_COPY(DIAG_QUAN,UNIT_GAUSS,IER)
C
      DO IROW = 1 , M_ROWS(INDEX_Q)
        DO ICOL = 1 , M_COLS(INDEX_Q)
          CALL LSQ_GET_VAL(UNIT_GAUSS,IROW,ICOL,INDEX_U,VAL,IER)
          CALL LSQ_GET_VAL(EIGEN_VALUES,IROW,ICOL,INDEX_E,EIGEN,IER)
          IF ( EIGEN.GT.0.0 ) THEN
            VAL = VAL/SQRT(EIGEN)
          ELSE
            IER = 5
            CALL ERRMSG('LSQ','LSQ_DIAG_TRAN',
     &        'EIGEN VALUE LESS THAN OR EQUAL TO ZERO','W')
          ENDIF
          CALL LSQ_SET_VAL(UNIT_GAUSS,IROW,ICOL,INDEX_U,VAL,IER)
        ENDDO
      ENDDO
C
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_COLLECT_GARBAGE(IER)
C
  999 RETURN
      END
