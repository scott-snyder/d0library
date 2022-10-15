      SUBROUTINE LSQ_MATRIX_FILL2(MATRIX_NAME,ARRAY,NROWS,NCOLS,
     &  IER)
      IMPLICIT NONE
      CHARACTER*(*) MATRIX_NAME
      double precision    ARRAY(*)
      INTEGER NROWS,NCOLS,ier
      call lsq_matrix_fill(matrix_name, array, nrows, ncols, ier)
      return
      end
      
