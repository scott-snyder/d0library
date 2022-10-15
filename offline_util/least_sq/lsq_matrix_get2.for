      SUBROUTINE LSQ_MATRIX_GET2(MATRIX_NAME,ARRAY,NROWS,NCOLS,
     &  IER)
      IMPLICIT NONE
      CHARACTER*(*) MATRIX_NAME
      double precision    ARRAY(*)
      INTEGER IER
      INTEGER NROWS,NCOLS
      call lsq_matrix_get(matrix_name, array, nrows, ncols, 2, ier)
      return
      end
