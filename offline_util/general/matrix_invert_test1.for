      SUBROUTINE MATRIX_INVERT_TEST1(STRING,MAT,MAT_INV,TOL,NDIM,PROD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tests whether double precision
C-   Matrices have inverted correctly.
C-
C-   Inputs  : STRING - CHARACTER string for Error messages
C-             MAT and MATINV are SINGLE precision matrices of DIMENSION NDIM.
C-             TOL = number by which PROD is allowed to differ from Unit
C-             matrix.
C-
C-   Outputs : PROD is the product matrix.
C-   Controls:
C-
C-   Created  17-AUG-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER *(*) STRING
      INTEGER NDIM
      REAL MAT(NDIM,NDIM),MAT_INV(NDIM,NDIM)
      REAL PROD(NDIM,NDIM)
      REAL    TOL,TEST
      INTEGER I,J,K,DMPUNI
C----------------------------------------------------------------------
      DO 1100 I = 1 , NDIM
        DO 1100 J = 1,NDIM
          PROD(I,J) = 0.0
          DO 1100 K=1,NDIM
            PROD(I,J) = PROD(I,J)+MAT(I,K)*MAT_INV(K,J)
 1100 CONTINUE
C
      DO 1200 I = 1 , NDIM
        DO 1200 J = 1,NDIM
          IF(I.EQ.J)THEN
            TEST = ABS(PROD(I,J)-1.0)
          ELSE
            TEST = ABS(PROD(I,J))
          ENDIF
          IF(TEST.GT.TOL)THEN
            WRITE(DMPUNI(),1201)STRING,I,J,TEST
 1201       FORMAT(' BAD INVERSION:,I,J,TEST ',A,2I12,F15.7)
          ENDIF
 1200 CONTINUE
  999 RETURN
      END
