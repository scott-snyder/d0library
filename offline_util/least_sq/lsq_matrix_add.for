      SUBROUTINE LSQ_MATRIX_ADD(FIRST_MATRIX,SECOND_MATRIX,
     &  RESULT_MATRIX,FACT1,FACT2,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ADDS or subtracts MATRICES
C-                      RESULT_MATRIX = FACT1*FIRST_MATRIX+FACT2*SECOND_MATRIX
C-                      I.E USING FACT1 AND FACT2, ONE CAN
C-                      ADD OR SUBTRACT MATRICES AT WILL
C-   Inputs  : FIRST_MATRIX,SECOND_MATRIX
C-   FACT1, FACT2 = SCALE FACTORS FOR FIRST AND SECOND MATRIX.
C-
C-   Outputs : RESULT_MATRIX
C-             IER = 1 FIRST_MATRIX DOES NOT EXIST
C-             IER = 2 SECOND_MATRIX DOES NOT EXIST
C-             IER = 3 MATRICES CANNOT BE ADDED
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
      CHARACTER*(*) FIRST_MATRIX,SECOND_MATRIX,RESULT_MATRIX
      INTEGER IROW,ICOL,I,IER
      INTEGER INDEX_F,INDEX_S,INDEX_P
      DOUBLE PRECISION VALF,VALS,VALSUM
      LOGICAL LSQ_MATRIX_EXIST
      DOUBLE PRECISION FACT1,FACT2
C----------------------------------------------------------------------
      IER = 0
      IF ( .NOT.LSQ_MATRIX_EXIST(FIRST_MATRIX,INDEX_F,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_MATRIX_ADD',
     &      ' First Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF ( .NOT.LSQ_MATRIX_EXIST(SECOND_MATRIX,INDEX_S,IER) ) THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_MATRIX_ADD',
     &      ' Second Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF((M_ROWS(INDEX_F).NE.M_ROWS(INDEX_S)).OR.
     &  (M_COLS(INDEX_F).NE.M_COLS(INDEX_S)))THEN
        IER = 3
        CALL ERRMSG('LSQ','LSQ_MATRIX_ADD',
     &      ' Matrices cannot be added ','W')
        RETURN
C
C ****  MATRIX CANNOT BE ADDED OR SUBTRACTED
C
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
          CALL LSQ_GET_VAL(FIRST_MATRIX,IROW,ICOL,INDEX_F,VALF,IER)
          CALL LSQ_GET_VAL(SECOND_MATRIX,IROW,ICOL,INDEX_S,VALS,IER)
          VALSUM = FACT1*VALF+FACT2*VALS
          CALL LSQ_SET_VAL(RESULT_MATRIX,IROW,ICOL,INDEX_P,VALSUM,IER)
        ENDDO
      ENDDO
C
  999 RETURN
      END
