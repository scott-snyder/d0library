      SUBROUTINE LSQ_BKMATRIX(MATRIX_NAME,NROWS,NCOLS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK a matrix with matrix name
C-
C-   Inputs  : MATRIX_NAME = name of matrix
C-             NROWS = Number of rows
C-             NCOLS = Number of columns
C-             A(I,j) is a matrix element. I is row, J is column.
C-   Outputs : IER = 1 Matrix already exists
C-             IER = 2 Number of matrices exceeded
C-   Controls:
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IXIO
      CHARACTER*(*) MATRIX_NAME
      INTEGER NROWS,NCOLS,IER
C
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /

      LOGICAL LSQ_MATRIX_EXIST
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
C
      INTEGER NDATA,LDUM
      INTEGER INDEX
C----------------------------------------------------------------------
      IER = 0
      IF ( LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_BKMATRIX',
     &    ' Matrix already exists ','W')
        RETURN
      ENDIF
      IF ( NMAT.GE.LSQ_MAX ) THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_BKMATRIX',
     &    ' Number of matrices exceeds maximum ','W')
        RETURN
      ENDIF
C
C ****  IF HERE OK TO BOOK MATRIX
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('LSQM','2I-D',IXIO)        ! Describe Bank format
      ENDIF
C
      NDATA = 2*NROWS*NCOLS + 2
C
C ****  BANK FORMAT IS 1ST TWO NUMBERS = ROW , COL
C ****  REST IS DOUBLE PRECISION MATRIX
C
      NMAT = NMAT + 1
C
      M_NAME(NMAT) = MATRIX_NAME
      M_ROWS(NMAT) = NROWS
      M_COLS(NMAT) = NCOLS
      M_DELETE(NMAT) = 0  !NOT DELETED
C
      CALL MZBOOK
     &  (IDVSTP,LSTLNK(NMAT),LDUM,2,'LSQM',0,0,NDATA,IXIO,0)
C
      IC(LSTLNK(NMAT)+1) = NROWS
      IC(LSTLNK(NMAT)+2) = NCOLS
C
  999 RETURN
      END
