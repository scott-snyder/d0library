      SUBROUTINE LSQ_MATRIX_TRANSFORM(MATRIX_NAME,UNITARY_MATRIX,
     &  TRANSFORMED_MATRIX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRANSFORMS MATRIX SUCH THAT
C-          
C-      TRANSFORMED_MATRIX = 
C-      (UNITARY_MATRIX TRANSPOSED)*MATRIX_NAME*UNITARY_MATRIX
C-                      
C-   Inputs  : MATRIX_NAME,UNITARY_MATRIX
C-   Outputs : TRANSFORMED_MATRIX
C-             IER = 1 MATRIX_NAME DOES NOT EXIST
C-             IER = 2 UNITARY_MATRIX DOES NOT EXIST
C-             IER = 3 MATRICES CANNOT BE MULTIPLIED.
C-             IER = 4 MATRIX NOT SQUARE
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
      CHARACTER*(*) MATRIX_NAME,UNITARY_MATRIX,TRANSFORMED_MATRIX
      INTEGER IROW,ICOL,I,IER
      INTEGER INDEX,INDEX_U,INDEX_T
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      IER = 0
      IF ( .NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_MATRIX_TRANSFORM',
     &      ' First Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF ( .NOT.LSQ_MATRIX_EXIST(UNITARY_MATRIX,INDEX_U,IER) ) THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_MATRIX_TRANSFORM',
     &      ' Second Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF(M_COLS(INDEX).NE.M_ROWS(INDEX_U))THEN
        IER = 3
        CALL ERRMSG('LSQ','LSQ_MATRIX_TRANSFORM',
     &      ' Matrices cannot be multiplied ','W')
        RETURN
      ENDIF
C
      IF(M_COLS(INDEX).NE.M_ROWS(INDEX))THEN
        IER = 4
        CALL ERRMSG('LSQ','LSQ_MATRIX_TRANSFORM',
     &      ' Matrix not square ','W')
        RETURN
C
C ****  matrix cannot be transformed
C
      ENDIF
C
      IF (.NOT. LSQ_MATRIX_EXIST(TRANSFORMED_MATRIX,INDEX_T,IER) ) THEN
        CALL LSQ_BKMATRIX(TRANSFORMED_MATRIX,M_ROWS(INDEX_U),
     &    M_COLS(INDEX_U),IER)
        INDEX_T = NMAT
      ENDIF
C
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_MATRIX_DELETE('TEMP',IER)
C
      CALL LSQ_MATRIX_TRANSPOSE(UNITARY_MATRIX,'WORK',IER)
      CALL LSQ_MATRIX_MULTIPLY(MATRIX_NAME,UNITARY_MATRIX,'TEMP',IER)
      CALL LSQ_MATRIX_MULTIPLY('WORK','TEMP',TRANSFORMED_MATRIX,IER)
C
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_MATRIX_DELETE('TEMP',IER)
      CALL LSQ_COLLECT_GARBAGE(IER)
C
  999 RETURN
      END
