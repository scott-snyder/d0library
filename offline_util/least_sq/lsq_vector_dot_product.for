      SUBROUTINE LSQ_VECTOR_DOT_PRODUCT(FIRST_VECTOR,SECOND_VECTOR,
     &  DOT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :TAKES DOT PRODUCT OF VECTORS SUCH THAT
C-                        DOT = SUM FIRST_VECTOR(I)*SECOND_VECTOR(I)
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
      CHARACTER*(*) FIRST_VECTOR,SECOND_VECTOR
      INTEGER IROW,ICOL,I,IER
      INTEGER INDEX_F,INDEX_S,INDEX_P
      DOUBLE PRECISION DOT
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      IER = 0
      IF ( .NOT.LSQ_MATRIX_EXIST(FIRST_VECTOR,INDEX_F,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_VECTOR_DOT_PRODUCT',
     &      ' First Vector does not exist ','W')
        RETURN
      ENDIF
C
      IF ( .NOT.LSQ_MATRIX_EXIST(SECOND_VECTOR,INDEX_S,IER) ) THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_VECTOR_DOT_PRODUCT',
     &      ' Second Vector does not exist ','W')
        RETURN
      ENDIF
C
      IF (M_COLS(INDEX_F).NE.1 ) THEN
        IER = 3
        CALL ERRMSG('LSQ','LSQ_VECTOR_DOT_PRODUCT',
     &      ' First Vector not a column vector ','W')
        RETURN
      ENDIF
C
      IF (M_COLS(INDEX_S).NE.1 ) THEN
        IER = 4
        CALL ERRMSG('LSQ','LSQ_VECTOR_DOT_PRODUCT',
     &      ' Second Vector not a column vector ','W')
        RETURN
      ENDIF
C
      CALL LSQ_MATRIX_DELETE('TEMP',IER)
      CALL LSQ_MATRIX_DELETE('WORK',IER)
C
      CALL LSQ_MATRIX_TRANSPOSE(SECOND_VECTOR,'TEMP',IER)
      CALL LSQ_MATRIX_MULTIPLY('TEMP',FIRST_VECTOR,'WORK',
     &  IER)
      INDEX_P = 0
      CALL LSQ_GET_VAL('WORK',1,1,INDEX_P,DOT,IER)
C
      CALL LSQ_MATRIX_DELETE('TEMP',IER)
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_COLLECT_GARBAGE(IER)
C
  999 RETURN
      END
