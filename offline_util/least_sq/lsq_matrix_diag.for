      SUBROUTINE LSQ_MATRIX_DIAG(MATRIX_NAME,UNITARY_MATRIX,
     &  EIGEN_VALUES,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN A MATRIX, WORKS OUT THE unitary matrix
C-   necessary to diagionalize the matrix and computes the eigen values
C-   of the matrix.
C-
C-   Inputs  : MATRIX_NAME
C-   Outputs : UNITARY_MATRIX (U). SUCH THAT U(TRANSPOSED)*M*U IS DIAGONAL
C-             = MATRIX EIGEN_VALUES
C-             The columns of U are the components of the Eigen vectors.
C-             In order to get uncorrelated quantities from vectors Q
C-             whose error matrix is in Matrix_name, do x = U(Transposed)Q
C-             . The components of X are then uncorrelated.
C-             IER = 1 MATRIX_NAME DOES NOT EXIST.
C-             IER = 2 matrix not square
C-             IER = 3 ERROR diagonalizing matrix
C-
C-   Controls:
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-   Updated   7-APR-1995   Alan M. Jonckheere  
C-      Change calls DGET -> DDGET and DSET -> DDSET 
C-        to avoid conflict with new intrinsic functions
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
C
      CHARACTER*(*) MATRIX_NAME,UNITARY_MATRIX,EIGEN_VALUES
      INTEGER IER,IERR
      INTEGER INDEX,INDEX_T,INDEX_U,INDEX_E,INDEX_W
      INTEGER LINK,LINK_E,LINK_U,LTEMP,LWORK
      LOGICAL LSQ_MATRIX_EXIST
C
      INTEGER IROW,ICOL,INDD,INDS
      DOUBLE PRECISION VAL
C----------------------------------------------------------------------
      IER = 0
      IF (.NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 1
        CALL ERRMSG('LSQ','LSQ_MATRIX_DIAG',
     &      ' Matrix does not exist ','W')
        RETURN
      ENDIF
C
      IF(M_ROWS(INDEX).NE.M_COLS(INDEX))THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_MATRIX_DIAG',
     &      ' Matrix is not square ','W')
        RETURN
      ENDIF
C
      CALL LSQ_MATRIX_DELETE('TEMP',IER)
      CALL LSQ_MATRIX_COPY(MATRIX_NAME,'TEMP',IER)
      INDEX_T = NMAT
C
C
C ****  CONVERT TEMP FROM DOUBLE PRECISION TO SINGLE PRECISION
C ****  FOR USE IN EISRS1
C
      LTEMP = LSTLNK(INDEX_T)+3
      DO ICOL = 1, M_COLS(INDEX_T)
        DO IROW = 1, M_ROWS(INDEX_T)
          INDS = (ICOL -1 )*M_ROWS(INDEX_T) + IROW
          INDD = 2*(INDS-1) + 1
          CALL DDGET(LTEMP+INDD-1,VAL)
          C(LTEMP+INDS-1) = VAL
        ENDDO
      ENDDO
C
      CALL LSQ_MATRIX_DELETE(UNITARY_MATRIX,IER)
      CALL LSQ_BKMATRIX(UNITARY_MATRIX,M_ROWS(INDEX),M_COLS(INDEX),IER)
      INDEX_U = NMAT
C
      CALL LSQ_MATRIX_DELETE(EIGEN_VALUES,IER)
      CALL LSQ_BKMATRIX(EIGEN_VALUES,M_ROWS(INDEX),1,IER)
C
C ****  EIGEN VALUES WILL BE IN INCREASING ORDER
C
      INDEX_E = NMAT
C
      CALL LSQ_MATRIX_DELETE('WORK',IER)
      CALL LSQ_BKMATRIX('WORK',M_ROWS(INDEX),M_COLS(INDEX),IER)
      INDEX_W = NMAT
C
C ****  book anew
C
      LINK = LSTLNK(INDEX)+3
      LINK_U = LSTLNK(INDEX_U)+3
      LINK_E = LSTLNK(INDEX_E)+3
      LWORK = LSTLNK(INDEX_W) + 3
C
      CALL EISRS1(M_ROWS(INDEX),M_COLS(INDEX),C(LTEMP),C(LINK_E),
     &        C(LINK_U),IERR,C(LWORK))
C
      IF ( IERR.NE.0 ) THEN
        IER = 3
        CALL ERRMSG('LSQ','LSQ_MATRIX_DIAG',
     &      ' Error diagonalizing matrix ','W')
      ENDIF
C
C
C ****  CONVERT TO DOUBLE PRECISION BOTH EIGEN AND UMAT
C
      DO ICOL =  M_COLS(INDEX_U) , 1 , -1
        DO IROW = M_ROWS(INDEX_U) , 1 , -1
          INDS = (ICOL -1 )*M_ROWS(INDEX_U) + IROW
          INDD = 2*(INDS-1) + 1
          VAL = C(LINK_U+INDS-1)
          CALL DDSET(LINK_U+INDD-1,VAL)
        ENDDO
      ENDDO
C
      DO ICOL =  M_COLS(INDEX_E) , 1 , -1
        DO IROW = M_ROWS(INDEX_E) , 1 , -1
          INDS = (ICOL -1 )*M_ROWS(INDEX_E) + IROW
          INDD = 2*(INDS-1) + 1
          VAL = C(LINK_E+INDS-1)
          CALL DDSET(LINK_E+INDD-1,VAL)
        ENDDO
      ENDDO
C
      CALL LSQ_MATRIX_DELETE('TEMP',IER)
      CALL LSQ_COLLECT_GARBAGE(IER)
C
  999 RETURN
      END
