      SUBROUTINE LSQ_MATRIX_COPY(MATRIX_NAME,MATRIX_COPY,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COPY ONE MATRIX TO OTHER
C-   will create copy matrix if it does not exist
C-
C-   Inputs  : MATRIX_NAME
C-   Outputs : MATRIX_COPY
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
      CHARACTER*(*) MATRIX_NAME,MATRIX_COPY
      INTEGER IROW,ICOL,I,IER
      INTEGER INDEX,INDEX_C,LINK,LINK_C
      LOGICAL LSQ_MATRIX_EXIST
      DOUBLE PRECISION VAL
C----------------------------------------------------------------------
      IER = 0
      IF (.NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
          CALL ERRMSG('LSQ','LSQ_MATRIX_COPY',
     &      ' Matrix does not exist ','W')
        IER = 1 
        RETURN
      ENDIF
C
      IF (.NOT. LSQ_MATRIX_EXIST(MATRIX_COPY,INDEX_C,IER) ) THEN
        CALL LSQ_BKMATRIX(MATRIX_COPY,M_ROWS(INDEX),
     &    M_COLS(INDEX),IER)
        INDEX_C = NMAT
      ENDIF
C
      DO IROW = 1 , M_ROWS(INDEX)
        DO ICOL = 1 , M_COLS(INDEX)
          CALL LSQ_GET_VAL(MATRIX_NAME,IROW,ICOL,INDEX,VAL,IER)
          CALL LSQ_SET_VAL(MATRIX_COPY,IROW,ICOL,INDEX_C,VAL,IER)
        ENDDO
      ENDDO
  999 RETURN
      END
