      SUBROUTINE LSQ_MATRIX_TRANSPOSE(MATRIX_NAME,
     &  TRANSPOSED_MATRIX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRANSPOSE THE MATRIX
C-
C-   Inputs  : MATRIX_NAME = NAME OF MATRIX TO BE TRANSPOSED
C-   Outputs : TRANSPOSED_MATRIX = NAME OF TRANSPOSED MATRIX
C-             IER = 1 INPUT MATRIX DOES NOT EXIST
C-   Controls:
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MATRIX_NAME,TRANSPOSED_MATRIX
      INTEGER IER
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      INTEGER IROW,ICOL
      INTEGER INDEX,INDEX_T
      DOUBLE PRECISION VAL
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      IER = 0
      IF (.NOT. LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_MATRIX_TRANSPOSE',
     &      ' Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF (.NOT. LSQ_MATRIX_EXIST(TRANSPOSED_MATRIX,INDEX_T,IER) ) THEN
        CALL LSQ_BKMATRIX(TRANSPOSED_MATRIX,M_COLS(INDEX),
     &    M_ROWS(INDEX),IER)
        INDEX_T = NMAT
      ELSE
        IF(M_ROWS(INDEX_T).NE.M_COLS(INDEX).OR.
     &    M_COLS(INDEX_T).NE.M_ROWS(INDEX))THEN
          IER = 2
          CALL ERRMSG('LSQ','LSQ_MATRIX_TRANSPOSE',
     &      'Transpose matrix  not commensurate with input matrix','W')
          RETURN
        ENDIF
      ENDIF
C
      DO IROW = 1 , M_ROWS(INDEX)
        DO ICOL = 1 , M_COLS(INDEX)
          CALL LSQ_GET_VAL(MATRIX_NAME,IROW,ICOL,INDEX,VAL,IER)
          CALL LSQ_SET_VAL(TRANSPOSED_MATRIX,ICOL,IROW,INDEX_T,VAL,IER)
        ENDDO
      ENDDO
C
  999 RETURN
      END
