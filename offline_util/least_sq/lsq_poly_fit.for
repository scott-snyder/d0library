      LOGICAL FUNCTION LSQ_POLY_FIT(NORDER,COEFF_VECT,ERROR_MATRIX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds and returns the coeffecents for a polynomial of
C-                         order NORDER.  The data points etc MUST have been
C-                         initialized using LSQ_POLY_INIT and "loaded" using
C-                         LSQ_POLY_LOAD.  If there was a problem the logical
C-                         function is .FALSE. and IER can be used to debug it.
C-
C-   Inputs  : 
C-            NORDER = order of the fitted polynomial
C-            
C-   Outputs : 
C-            COEFF_VECT = a one dimensional array with NORDER parameters
C-                         resulting from the fit to Yi = a0+a1*x+a2*x**2+...
C-                         where COEFF_VECT(i) = ai. Thus for a fit to a 3rd
C-                         order polynomial there are 4 coefficients.
C-                         
C-            ERROR_MATRIX(i,j) = (sigma**2)ij.  The square root of the diagonal
C-                         elements correspond to the errors of the
C-                         corresponding coefficients.  The off diagonal
C-                         elements are the correlations.  This array has
C-                         dimensions (NORDER+1),(NORDER+1)
C-                         
C-            IER = 0 --> everything is ok, otherwise there was a problem in one
C-                        of the LSQ routines and this returns the error flag
C-                        given there. (+1000 for LSQ_MATRIX INVERT
C-                                      +2000 for LSQ_MATRIX_MULTIPLY
C-                                      +3000 for LSQ_MATRIX_GET)  Thus if there
C-                        is a problem in inverting the matrix IER=1000+IER from
C-                        LSQ_MATRIX INVERT
C-   Controls: 
C-
C-   Created  22-MAR-1992   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER IER,NROWS,NCOL,NORDER
      REAL    COEFF_VECT(NORDER+1),ERROR_MATRIX(NORDER+1,NORDER+1)
C----------------------------------------------------------------------
      LSQ_POLY_FIT=.FALSE.

      CALL LSQ_MATRIX_INVERT('ALPHA','ALPHA_INVERSE',IER)
      IF ( IER.NE.0 ) THEN
        IER=IER+1000
        RETURN
      ENDIF

      CALL LSQ_MATRIX_MULTIPLY('ALPHA_INVERSE','BETA','COEFF',IER)
      IF ( IER.NE.0 ) THEN
        IER=IER+2000
        RETURN
      ENDIF

      NROWS=NORDER+1
      NCOL=1
      CALL LSQ_MATRIX_GET('COEFF',COEFF_VECT,NROWS,NCOL,1,IER)
      IF ( IER.NE.0 ) THEN
        IER=IER+3000
        RETURN
      ENDIF

      NROWS=NORDER+1
      NCOL=NORDER+1
      CALL LSQ_MATRIX_GET('ALPHA_INVERSE',ERROR_MATRIX,NROWS,NCOL,1,
     &  IER) 
      IF ( IER.NE.0 ) THEN
        IER=IER+3000
        RETURN
      ENDIF

      LSQ_POLY_FIT=.TRUE.

  999 RETURN
      END
