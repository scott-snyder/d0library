      REAL FUNCTION SAMUS_BIAS_RC_MU_PLUS_1B(P,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate SAMUS min_bias background correction
C-                         for RECO ( RUN 1B )
C-
C-                          6 GeV <   P < 200 GeV
C-                          2.0   < ETA <    3.5
C-
C-   Returned value  :
C-   Inputs  :
C-              P     -   Muon momentum
C-              ETA   -   Pseudorapidity
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-JUL-1996   O.Eroshin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER  NA(2)
      REAL     FINT, P, ETA, X(2), A(24), F(9,15)
C----------------------------------------------------------------------
      DATA     NA/9,15/
      DATA     A / 6., 8., 10., 15., 20., 25., 50., 100., 200.,
     *             2.05, 2.15, 2.25, 2.35, 2.45, 2.55, 2.65, 2.75,
     *             2.85, 2.95, 3.05, 3.15, 3.25, 3.35, 3.45/
      DATA F /
     +  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,
     +  1.00,  1.00,  1.00,  1.17,  1.09,  1.00,  1.00,  1.00,  1.00,
     +  1.00,  1.00,  1.17,  1.00,  0.95,  1.00,  0.90,  0.90,  0.91,
     +  1.00,  2.00,  1.43,  1.55,  1.18,  1.04,  0.98,  0.96,  0.96,
     +  1.00,  1.00,  2.00,  1.68,  1.10,  0.95,  0.94,  0.95,  0.88,
     +  1.00,  1.00,  1.00,  1.65,  1.15,  1.01,  0.94,  0.90,  0.92,
     +  1.00,  1.00,  1.00,  1.50,  2.06,  1.20,  0.88,  0.85,  0.90,
     +  1.00,  1.00,  1.00,  1.00,  1.56,  1.35,  1.03,  0.94,  0.92,
     +  1.00,  1.00,  1.00,  2.00,  1.67,  1.23,  0.89,  0.89,  0.89,
     +  1.00,  1.00,  1.00,  1.00,  2.00,  1.40,  0.90,  0.83,  0.82,
     +  1.00,  1.00,  1.00,  1.00,  1.00,  3.00,  1.13,  0.90,  0.87,
     +  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  0.92,  0.89,  0.89,
     +  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  0.95,  0.87,  0.91,
     +  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  1.20,  0.86,  0.93,
     +  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  1.00/
C
C..........
C
      X(1) = ABS(P)
      X(2) = ABS(ETA)
      IF (X(1) .LT.   6.) X(1) = 6.
      IF (X(1) .GT. 200.) X(1) = 200.
C
      IF (X(2) .LT. 2.05) X(2) = 2.05
      IF (X(2) .GT. 3.45) X(2) = 3.45
C
      SAMUS_BIAS_RC_MU_PLUS_1B = FINT(2,X,NA,A,F)
C
  999 RETURN
      END
