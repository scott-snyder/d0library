      SUBROUTINE LSQ_MATRIX_INVERT_TEST(MATRIX_NAME,MATRIX_INVERSE,
     &  TOL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TESTS whether matrix_name and inverse
C-   are inverses of each other within tolerence
C-
C-   Inputs  : MATRIX_NAME,MATRIX_INVERSE
C-             TOL = Tolerance outside which error will be generated
C-   Outputs : MATRIX_INVERSE
C-             IER = 1 MATRIX_NAME DOES NOT EXIST
C-             IER = 2 MATRIX_NAME not square . does not have inverse
C-             IER = 3 MATRIX_INVERSE DOES NOT EXIST
C-             IER = 4 MATRIX_INVERSE not square . does not have inverse
C-             IER = 5 Matrix and matrix_inverse have different dimensions
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
      REAL    TOL
C----------------------------------------------------------------------
      IER = 0
      IF (.NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 1 
          CALL ERRMSG('LSQ','LSQ_MATRIX_INVERT_TEST',
     &      ' Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF(M_ROWS(INDEX).NE.M_COLS(INDEX))THEN
        IER = 2
          CALL ERRMSG('LSQ','LSQ_MATRIX_INVERT_TEST',
     &      ' Matrix not square','W')
        RETURN
      ENDIF
C
      IF (.NOT.LSQ_MATRIX_EXIST(MATRIX_INVERSE,INDEX_I,IER) ) THEN
        IER = 3
          CALL ERRMSG('LSQ','LSQ_MATRIX_INVERT_TEST',
     &      ' Matrix inverse does not exist ','W')
        RETURN
      ENDIF
C
      IF(M_ROWS(INDEX_I).NE.M_COLS(INDEX_I))THEN
        IER = 4
          CALL ERRMSG('LSQ','LSQ_MATRIX_INVERT_TEST',
     &      ' Matrix inverse not square ','W')
        RETURN
      ENDIF
C
      IF ( M_ROWS(INDEX).NE.M_ROWS(INDEX_I) ) THEN
        IER = 5
          CALL ERRMSG('LSQ','LSQ_MATRIX_INVERT_TEST',
     &      ' Matrix and inverse have different dimensions ','W')
        RETURN
      ENDIF
C
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_BKMATRIX('WORK',M_ROWS(INDEX),M_COLS(INDEX),IER)
      INDEX_W = NMAT
C
      LINK = LSTLNK(INDEX)+3
      LINK_I = LSTLNK(INDEX_I)+3
      LWORK = LSTLNK(INDEX_W)+3
C
      CALL MATRIX_INVERT_TEST2(MATRIX_NAME,C(LINK),C(LINK_I),
     &  TOL,M_ROWS(INDEX),C(LWORK))
C
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_COLLECT_GARBAGE(IER)
C
  999 RETURN
      END
