      SUBROUTINE LSQ_POLY_LOAD(YI,XI,SIGMA_I,NORDER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loads the various arrays used for fitting to a
C-                         polynomial.  MUST be called AFTER LSQ_POLY_INIT and
C-                         BEFORE calling LSQ_POLY_FIT
C-
C-   Inputs  : 
C-             YI = Measured Ith data point
C-             XI = Ith value of the varible used in the Polynomial
C-                  ie  YI = a0 + a1*XI + a2*XI**2 + ...
C-             SIGMA_I = error assocatied with this measurement.
C-             NORDER = order of the polynomial to be fit.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  22-MAR-1992   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XI,YI,SIGMA_I,A,B
      INTEGER NORDER,I,J,INDEX_A,INDEX_B,IER
C----------------------------------------------------------------------

      IF( SIGMA_I.LE.0. ) SIGMA_I=1.

      INDEX_A=0
      INDEX_B=0

      DO I = 1 , NORDER+1
        DO J = 1 , NORDER+1
          A=(XI**(I-1))*(XI**(J-1))/SIGMA_I**2
          CALL LSQ_MATRIX_ADD_ELEMENT('ALPHA',INDEX_A,I,J,1.,1.,A,
     &      'ALPHA',IER)
        ENDDO
        B=YI*(XI**(I-1))/SIGMA_I
        CALL LSQ_MATRIX_ADD_ELEMENT('BETA',INDEX_B,I,1,1.,1.,B,'BETA'
     &    ,IER)
      ENDDO

  999 RETURN
      END
