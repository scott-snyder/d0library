      SUBROUTINE LSQ_GET_ROWS_COLUMNS(MATRIX_NAME,ROWS,COLS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets the number of rows and columns in the matrix
C-
C-   Inputs  :          MATRIX_NAME = NAME OF MATRIX 
C-   Outputs : ROWS,COLS = number of rows and columns in matrix
C-             IER = 1 MATRIX DOES NOT EXIST
C-   Controls:
C-
C-   Updated  20-APR-1992   Rajendran Raja   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      CHARACTER*(*) MATRIX_NAME
      INTEGER INDEX,IER
      INTEGER LINK,ROWS,COLS
      INTEGER I,PRUNIT
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      IER = 0
      IF ( .NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 1
          CALL ERRMSG('LSQ','LSQ_MATRIX_PRINT',
     &      ' Matrix does not exist ','W')
        RETURN
      ENDIF
      LINK = LSTLNK(INDEX)
      ROWS = IC(LINK+1)
      COLS = IC(LINK+2)
  999 RETURN
      END
