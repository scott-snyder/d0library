      SUBROUTINE LSQ_MATRIX_INVERT(MATRIX_NAME,MATRIX_INVERSE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inverts a matrix
C-
C-   Inputs  : MATRIX_NAME
C-   Outputs : MATRIX_INVERSE
C-             IER = 1 MATRIX_NAME DOES NOT EXIST
C-             IER = 2 Matrix not square . cannot be inverted
C-             IER = 3 Error inverting matrix
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
      CHARACTER*(*) MATRIX_NAME,MATRIX_INVERSE
      INTEGER IROW,ICOL,I,IER
      INTEGER INDEX,INDEX_I,LINK,LINK_I,INDEX_W,LWORK
      LOGICAL LSQ_MATRIX_EXIST
      INTEGER NERROR
C----------------------------------------------------------------------
      IER = 0
      IF (.NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 1 
          CALL ERRMSG('LSQ','LSQ_MATRIX_INVERT',
     &      ' Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF(M_ROWS(INDEX).NE.M_COLS(INDEX))THEN
        IER = 2
          CALL ERRMSG('LSQ','LSQ_MATRIX_INVERT',
     &      ' Matrix not square ','W')
        RETURN
      ENDIF
C
      IF (.NOT. LSQ_MATRIX_EXIST(MATRIX_INVERSE,INDEX_I,IER) ) THEN
        CALL LSQ_BKMATRIX(MATRIX_INVERSE,M_ROWS(INDEX),
     &    M_COLS(INDEX),IER)
        INDEX_I = NMAT
      ENDIF
C
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_BKMATRIX('WORK',M_ROWS(INDEX),M_COLS(INDEX),IER)
C
C ****  book anew
C
      INDEX_W = NMAT
C
      LINK = LSTLNK(INDEX)+3
      LINK_I = LSTLNK(INDEX_I)+3
      LWORK = LSTLNK(INDEX_W)+3
C
      CALL MATRIX_INVERT_2(MATRIX_NAME,C(LINK),M_ROWS(INDEX),C(LWORK),
     &  C(LINK_I),NERROR)
C
      IF ( NERROR.NE.0 ) THEN
        IER = 3
          CALL ERRMSG('LSQ','LSQ_MATRIX_INVERT',
     &      ' Error inverting matrix','W')
      ENDIF
C
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_COLLECT_GARBAGE(IER)
C
  999 RETURN
      END
