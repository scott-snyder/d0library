      SUBROUTINE LSQ_SCALAR_MULTIPLY(MATRIX_NAME,SCALAR,
     &  RESULT_MATRIX, IER)  
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MULTIPLY first matrix by a scalar
C-   put the result in result matrix. If result matrix does not exist
C-   it is created. Result matrix may be the same as the first matrix.
C-
C-   Inputs  : MATRIX_NAME = Name of matrix to be multiplied by scalar
C-             SCALAR      = Scalar to multiply
C-             RESULT_MATRIX = Name of output matrix
C-   Outputs : IER = 1 MaTRIX_NAME does not exist
C-
C-   Controls: 
C-
C-   Created  26-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
C
      CHARACTER*(*) MATRIX_NAME,RESULT_MATRIX
      INTEGER IROW,ICOL,I,IER
      INTEGER INDEX_F,INDEX_P
      DOUBLE PRECISION VALF,VALP,SCALAR
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      IER = 0
      IF ( .NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX_F,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_MATRIX_MULTIPLY',
     &      ' First Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF (.NOT. LSQ_MATRIX_EXIST(RESULT_MATRIX,INDEX_P,IER) ) THEN
        CALL LSQ_BKMATRIX(RESULT_MATRIX,M_ROWS(INDEX_F),
     &    M_COLS(INDEX_F),IER)
        INDEX_P = NMAT
      ENDIF
C
      DO IROW = 1 , M_ROWS(INDEX_P)
        DO ICOL = 1 , M_COLS(INDEX_P)
            CALL LSQ_GET_VAL(MATRIX_NAME,IROW,ICOL,INDEX_F,VALF,IER)
            VALP = VALF*SCALAR
            CALL LSQ_SET_VAL(RESULT_MATRIX,IROW,ICOL,INDEX_P,VALP,IER)
        ENDDO
      ENDDO
C
  999 RETURN
      END
