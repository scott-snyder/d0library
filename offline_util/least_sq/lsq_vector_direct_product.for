      SUBROUTINE LSQ_VECTOR_DIRECT_PRODUCT(FIRST_VECTOR,SECOND_VECTOR,
     &  PRODUCT_MATRIX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MULTIPLIES VECTORS
C-                      PRODUCT_MATRIX(I,J) = 
C-                      FIRST_VECTOR(I)*SECOND_VECTOR(J)
C-   Inputs  : FIRST_VECTOR,SECOND_VECTOR. Both these must be 
C-   column vectors. I.E. Number of columns = 1
C-   Outputs : PRODUCT_MATRIX
C-             IER = 1 FIRST_VECTOR DOES NOT EXIST
C-             IER = 2 SECOND_VECTOR DOES NOT EXIST
C-             IER = 3 FIRST VECTOR NOT A VECTOR
C-             IER = 4 SECOND VECTOR NOT A VECTOR
C-   Controls:
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
C
      CHARACTER*(*) FIRST_VECTOR,SECOND_VECTOR,PRODUCT_MATRIX
      INTEGER IROW,ICOL,I,IER
      INTEGER INDEX_F,INDEX_S,INDEX_P
      DOUBLE PRECISION VALF,VALS,VALSUM
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      IER = 0
      IF ( .NOT.LSQ_MATRIX_EXIST(FIRST_VECTOR,INDEX_F,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_VECTOR_DIRECT_PRODUCT',
     &      ' First Vector does not exist ','W')
        RETURN
      ENDIF
C
      IF ( .NOT.LSQ_MATRIX_EXIST(SECOND_VECTOR,INDEX_S,IER) ) THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_VECTOR_DIRECT_PRODUCT',
     &      ' Second Vector does not exist ','W')
        RETURN
      ENDIF
C
      IF (M_COLS(INDEX_F).NE.1 ) THEN
        IER = 3
        CALL ERRMSG('LSQ','LSQ_VECTOR_DIRECT_PRODUCT',
     &      ' First Vector not a column vector ','W')
        RETURN
      ENDIF
C
      IF (M_COLS(INDEX_S).NE.1 ) THEN
        IER = 4
        CALL ERRMSG('LSQ','LSQ_VECTOR_DIRECT_PRODUCT',
     &      ' Second Vector not a column vector ','W')
        RETURN
      ENDIF
C
      IF (.NOT. LSQ_MATRIX_EXIST(PRODUCT_MATRIX,INDEX_P,IER) ) THEN
        CALL LSQ_BKMATRIX(PRODUCT_MATRIX,M_ROWS(INDEX_F),
     &    M_ROWS(INDEX_S),IER)
        INDEX_P = NMAT
      ENDIF
C
      CALL LSQ_MATRIX_DELETE('TEMP',IER)
      CALL LSQ_MATRIX_TRANSPOSE(SECOND_VECTOR,'TEMP',IER)
      CALL LSQ_MATRIX_MULTIPLY(FIRST_VECTOR,'TEMP',PRODUCT_MATRIX,
     &  IER)
C
      CALL LSQ_MATRIX_DELETE('TEMP',IER)
      CALL LSQ_COLLECT_GARBAGE(IER)
C
  999 RETURN
      END
