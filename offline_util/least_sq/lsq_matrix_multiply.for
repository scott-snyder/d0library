      SUBROUTINE LSQ_MATRIX_MULTIPLY(FIRST_MATRIX,SECOND_MATRIX,
     &  PRODUCT_MATRIX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MULTIPLIES MATRICES
C-                      PRODUCT_MATRIX = FIRST_MATRIX*SECOND_MATRIX
C-   Inputs  : FIRST_MATRIX,SECOND_MATRIX
C-   Outputs : PRODUCT_MATRIX
C-             IER = 1 FIRST_MATRIX DOES NOT EXIST
C-             IER = 2 SECOND_MATRIX DOES NOT EXIST
C-             IER =3 MATRICES CANNOT BE MULTIPLIED.
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
      CHARACTER*(*) FIRST_MATRIX,SECOND_MATRIX,PRODUCT_MATRIX
      INTEGER IROW,ICOL,I,IER
      INTEGER INDEX_F,INDEX_S,INDEX_P
      DOUBLE PRECISION VALF,VALS,VALSUM
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      IER = 0
      IF ( .NOT.LSQ_MATRIX_EXIST(FIRST_MATRIX,INDEX_F,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_MATRIX_MULTIPLY',
     &      ' First Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF ( .NOT.LSQ_MATRIX_EXIST(SECOND_MATRIX,INDEX_S,IER) ) THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_MATRIX_MULTIPLY',
     &      ' Second Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF(M_COLS(INDEX_F).NE.M_ROWS(INDEX_S))THEN
        IER = 3
        CALL ERRMSG('LSQ','LSQ_MATRIX_MULTIPLY',
     &      ' Matrices cannot be multiplied ','W')
        RETURN
C
C ****  MATRIX CANNOT BE MULTIPLIED
C
      ENDIF
C
      IF (.NOT. LSQ_MATRIX_EXIST(PRODUCT_MATRIX,INDEX_P,IER) ) THEN
        CALL LSQ_BKMATRIX(PRODUCT_MATRIX,M_ROWS(INDEX_F),
     &    M_COLS(INDEX_S),IER)
        INDEX_P = NMAT
      ENDIF
C
      DO IROW = 1 , M_ROWS(INDEX_P)
        DO ICOL = 1 , M_COLS(INDEX_P)
          VALSUM = 0.0
          DO I = 1 , M_COLS(INDEX_F)
            CALL LSQ_GET_VAL(FIRST_MATRIX,IROW,I,INDEX_F,VALF,IER)
            CALL LSQ_GET_VAL(SECOND_MATRIX,I,ICOL,INDEX_S,VALS,IER)
            VALSUM = VALSUM + VALF*VALS
          ENDDO
          CALL LSQ_SET_VAL(PRODUCT_MATRIX,IROW,ICOL,INDEX_P,VALSUM,IER)
        ENDDO
      ENDDO
C
  999 RETURN
      END
