      SUBROUTINE LSQ_MATRIX_ADD_ELEMENT(MATRIX_NAME,INDEX,
     &  IROW,ICOL,FACT1,FACT2,VALUE,RESULT_MATRIX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : does the following computation
C-   MATRIX_NAME(IROW,ICOL) = FACT1*MATRIX_NAME(IROW,ICOL)+FACT2*VALUE
C-   Inputs  : MATRIX_NAME = Name of matrix
C-             INDEX = INDEX of matrix. If zero, will work it out.
C-             If repeated elements are to be added, call first time
C-             with index  zero and re-call routine with given index.
C-
C-             FACT1, FACT2 = SCALE FACTORS FOR addition
C-             VALUE = VALUE TO BE ADDED TO ELEMENT.
C-             USING FACT1 AND FACT2, ONE CAN add or subtract as needed
C-
C-   Outputs : RESULT_MATRIX
C-             IER = 1 MATRIX_NAME DOES NOT EXIST
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
      CHARACTER*(*) MATRIX_NAME,RESULT_MATRIX
      INTEGER IROW,ICOL,I,IER
      INTEGER INDEX,INDEX_P
      DOUBLE PRECISION VALF,VALSUM,VALUE
      LOGICAL LSQ_MATRIX_EXIST
      DOUBLE PRECISION FACT1,FACT2
C----------------------------------------------------------------------
      IER = 0
      IF ( INDEX.EQ.0 ) THEN
        IF ( .NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
          IER = 1
          CALL ERRMSG('LSQ','LSQ_MATRIX_ADD_ELEMENT',
     &      ' First Matrix does not exist ','W')
          RETURN
        ENDIF
      ENDIF
C
      IF (.NOT. LSQ_MATRIX_EXIST(RESULT_MATRIX,INDEX_P,IER) ) THEN
        CALL LSQ_BKMATRIX(RESULT_MATRIX,M_ROWS(INDEX),
     &    M_COLS(INDEX),IER)
        INDEX_P = NMAT
      ENDIF
C
      CALL LSQ_GET_VAL(MATRIX_NAME,IROW,ICOL,INDEX,VALF,IER)
      VALSUM = FACT1*VALF+FACT2*VALUE
      CALL LSQ_SET_VAL(RESULT_MATRIX,IROW,ICOL,INDEX_P,VALSUM,IER)
C
  999 RETURN
      END
