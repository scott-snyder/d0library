      SUBROUTINE MATRIX_INVERT_1(STRING,MATRIX,NDIM,WORK,
     &  MATRIX_INV,NERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inverts a Single precision matirx.
C-
C-   Inputs  : STRING : Character string for error message printout
C-             Single precision MATRIX of dimension NDIM.
C-             REAL works space of dimension at least NDIM.
C-   Outputs : Single precision MATRIX_INV Inverted matrix.
C-             NERROR Non Zero if problems.
C-   Controls:
C-
C-   Created  17-AUG-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER NERROR,NDIM,I,J
      REAL    MATRIX(NDIM,NDIM),MATRIX_INV(NDIM,NDIM)
      REAL    WORK(NDIM)
C----------------------------------------------------------------------
      DO I = 1 , NDIM
        DO J = 1 , NDIM
          MATRIX_INV(I,J) = MATRIX(I,J)
        ENDDO
      ENDDO
C
C ****  CAN INVERT NOW.
C
      NERROR = 0
      CALL RINV(NDIM,MATRIX_INV,NDIM,WORK,NERROR)
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('MATRIX_INVERT_2',STRING,
     &    'ERROR INVERTING MATRIX','W')
      ENDIF
  999 RETURN
      END
