      REAL FUNCTION SAMUS_LEVEL1_EFF_MU_MINUS_1A(P,FI,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate SAMUS LEVEL1 efficiency for mu- 
C-                         for RUN 1A
C-
C-                          6 GeV <   P < 200 GeV
C-                          0.0   <  Fi < 1.8
C-                          2.0   < Eta < 3.5
C-
C-   Returned value  : 
C-   Inputs  : 
C-              P     -   Muon momentum 
C-              FI    -   Fi angle
C-              ETA   -   Pseudorapidity 
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-OCT-1993   O.Eroshin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL ETA_MIN,ETA_MAX
      PARAMETER (ETA_MIN = 2.0)
      PARAMETER (ETA_MAX = 3.5)
C----------------------------------------------------------------------
      LOGICAL  FLAG
      INTEGER  I, NA(3)
      REAL     FINT, P, FI, ETA, X(3), A(42), F_MU_5(9,18,15)
      REAL     F_1 (9,18),F_2 (9,18),F_3 (9,18),F_4 (9,18),F_5 (9,18),
     +         F_6 (9,18),F_7 (9,18),F_8 (9,18),F_9 (9,18),F_10(9,18),
     +         F_11(9,18),F_12(9,18),F_13(9,18),F_14(9,18),F_15(9,18)
C----------------------------------------------------------------------
      EQUIVALENCE ( F_1,F_MU_5(1,1, 1)),( F_2,F_MU_5(1,1, 2))
      EQUIVALENCE ( F_3,F_MU_5(1,1, 3)),( F_4,F_MU_5(1,1, 4))
      EQUIVALENCE ( F_5,F_MU_5(1,1, 5)),( F_6,F_MU_5(1,1, 6))
      EQUIVALENCE ( F_7,F_MU_5(1,1, 7)),( F_8,F_MU_5(1,1, 8))
      EQUIVALENCE ( F_9,F_MU_5(1,1, 9)),(F_10,F_MU_5(1,1,10))
      EQUIVALENCE (F_11,F_MU_5(1,1,11)),(F_12,F_MU_5(1,1,12))
      EQUIVALENCE (F_13,F_MU_5(1,1,13)),(F_14,F_MU_5(1,1,14))
      EQUIVALENCE (F_15,F_MU_5(1,1,15))
C----------------------------------------------------------------------
      DATA     FLAG/ .TRUE. /
      DATA     NA  / 9, 18, 15/
      DATA     A   / 6.,8.,10.,15.,20.,25.,50.,100.,200.,33*0.0/
C----------------------------------------------------------------------
      DATA F_1  /
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.00,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.00,  0.02,
     +    0.01,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.00,  0.02,
     +    0.00,  0.01,  0.01,  0.01,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.01,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.02,  0.01,  0.00,  0.02,  0.00,  0.01,  0.00,  0.02,
     +    0.01,  0.02,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.02,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.03,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.03,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.00,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02/
      DATA F_2  /
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.00,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.00,  0.00,  0.02,
     +    0.01,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.02,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.01,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.02,  0.01,  0.01,  0.02,  0.01,  0.01,  0.01,  0.02,
     +    0.00,  0.02,  0.02,  0.01,  0.02,  0.01,  0.01,  0.01,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.03,  0.01,  0.02,  0.02,  0.02,
     +    0.00,  0.02,  0.01,  0.01,  0.02,  0.01,  0.01,  0.02,  0.02,
     +    0.00,  0.02,  0.02,  0.00,  0.02,  0.01,  0.01,  0.01,  0.03,
     +    0.00,  0.02,  0.02,  0.00,  0.02,  0.01,  0.01,  0.01,  0.03,
     +    0.00,  0.02,  0.02,  0.00,  0.02,  0.00,  0.01,  0.01,  0.03,
     +    0.00,  0.01,  0.02,  0.00,  0.02,  0.01,  0.01,  0.01,  0.03,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.01,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.01/
      DATA F_3  /
     +    0.00,  0.01,  0.01,  0.01,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.01,  0.02,  0.01,  0.01,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.01,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.01,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.02,  0.02,  0.01,  0.02,  0.01,  0.01,  0.01,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.02,  0.01,  0.01,  0.02,  0.02,
     +    0.00,  0.03,  0.03,  0.04,  0.04,  0.02,  0.04,  0.05,  0.04,
     +    0.01,  0.03,  0.04,  0.09,  0.11,  0.14,  0.19,  0.25,  0.23,
     +    0.01,  0.03,  0.06,  0.06,  0.06,  0.09,  0.12,  0.15,  0.18,
     +    0.01,  0.02,  0.02,  0.01,  0.02,  0.02,  0.03,  0.04,  0.04,
     +    0.00,  0.02,  0.02,  0.01,  0.03,  0.01,  0.01,  0.02,  0.03,
     +    0.00,  0.02,  0.01,  0.00,  0.03,  0.01,  0.01,  0.02,  0.03,
     +    0.00,  0.01,  0.02,  0.00,  0.02,  0.01,  0.01,  0.02,  0.03,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.01,  0.01,  0.02,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.00,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.01,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.01,  0.02,  0.01,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.01,  0.01,  0.01,  0.02/
      DATA F_4  /
     +    0.01,  0.01,  0.01,  0.01,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.01,  0.02,  0.01,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.01,  0.02,  0.01,  0.00,  0.01,  0.02,
     +    0.01,  0.02,  0.02,  0.01,  0.02,  0.00,  0.01,  0.02,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.03,  0.01,  0.03,  0.03,  0.03,
     +    0.00,  0.04,  0.05,  0.05,  0.06,  0.05,  0.07,  0.11,  0.09,
     +    0.02,  0.03,  0.09,  0.13,  0.14,  0.17,  0.18,  0.27,  0.27,
     +    0.02,  0.06,  0.09,  0.20,  0.26,  0.30,  0.31,  0.36,  0.37,
     +    0.01,  0.04,  0.07,  0.13,  0.18,  0.23,  0.27,  0.34,  0.34,
     +    0.01,  0.03,  0.03,  0.04,  0.07,  0.09,  0.14,  0.22,  0.21,
     +    0.00,  0.02,  0.02,  0.01,  0.04,  0.03,  0.04,  0.06,  0.07,
     +    0.00,  0.02,  0.02,  0.01,  0.03,  0.01,  0.02,  0.03,  0.03,
     +    0.00,  0.02,  0.02,  0.01,  0.02,  0.02,  0.01,  0.02,  0.03,
     +    0.00,  0.01,  0.01,  0.01,  0.02,  0.01,  0.01,  0.03,  0.03,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.01,  0.00,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.00,  0.01,  0.01,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.01,  0.01,  0.01,  0.02,
     +    0.00,  0.02,  0.01,  0.00,  0.02,  0.01,  0.01,  0.01,  0.02/
      DATA F_5  /
     +    0.00,  0.02,  0.02,  0.01,  0.02,  0.01,  0.02,  0.03,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.03,  0.02,  0.02,  0.03,  0.02,
     +    0.01,  0.03,  0.02,  0.02,  0.05,  0.02,  0.04,  0.06,  0.05,
     +    0.01,  0.03,  0.02,  0.03,  0.06,  0.04,  0.08,  0.11,  0.11,
     +    0.01,  0.04,  0.04,  0.08,  0.09,  0.09,  0.18,  0.21,  0.27,
     +    0.02,  0.04,  0.07,  0.11,  0.19,  0.20,  0.25,  0.30,  0.31,
     +    0.02,  0.06,  0.09,  0.19,  0.25,  0.29,  0.30,  0.36,  0.36,
     +    0.02,  0.06,  0.09,  0.19,  0.26,  0.30,  0.30,  0.35,  0.36,
     +    0.01,  0.04,  0.08,  0.15,  0.24,  0.31,  0.27,  0.35,  0.29,
     +    0.01,  0.04,  0.04,  0.09,  0.16,  0.22,  0.34,  0.36,  0.39,
     +    0.00,  0.02,  0.03,  0.03,  0.08,  0.09,  0.17,  0.24,  0.27,
     +    0.01,  0.02,  0.03,  0.02,  0.02,  0.05,  0.10,  0.14,  0.18,
     +    0.00,  0.02,  0.02,  0.02,  0.03,  0.02,  0.04,  0.08,  0.08,
     +    0.00,  0.02,  0.02,  0.01,  0.03,  0.02,  0.02,  0.04,  0.04,
     +    0.00,  0.01,  0.01,  0.00,  0.02,  0.01,  0.01,  0.03,  0.02,
     +    0.00,  0.01,  0.01,  0.00,  0.01,  0.01,  0.02,  0.01,  0.01,
     +    0.00,  0.01,  0.01,  0.01,  0.01,  0.01,  0.02,  0.01,  0.02,
     +    0.00,  0.02,  0.01,  0.01,  0.02,  0.02,  0.02,  0.03,  0.03/
      DATA F_6  /
     +    0.01,  0.02,  0.02,  0.06,  0.07,  0.09,  0.14,  0.21,  0.26,
     +    0.01,  0.01,  0.03,  0.07,  0.10,  0.12,  0.18,  0.27,  0.25,
     +    0.01,  0.03,  0.03,  0.09,  0.13,  0.16,  0.21,  0.31,  0.31,
     +    0.00,  0.04,  0.04,  0.12,  0.19,  0.27,  0.29,  0.35,  0.35,
     +    0.01,  0.04,  0.05,  0.13,  0.22,  0.28,  0.38,  0.35,  0.42,
     +    0.02,  0.04,  0.06,  0.16,  0.22,  0.29,  0.30,  0.34,  0.34,
     +    0.01,  0.04,  0.08,  0.16,  0.21,  0.28,  0.30,  0.31,  0.35,
     +    0.02,  0.03,  0.07,  0.15,  0.20,  0.25,  0.31,  0.33,  0.35,
     +    0.02,  0.04,  0.05,  0.15,  0.21,  0.30,  0.32,  0.33,  0.37,
     +    0.02,  0.02,  0.04,  0.11,  0.22,  0.27,  0.37,  0.37,  0.40,
     +    0.01,  0.03,  0.05,  0.05,  0.13,  0.19,  0.28,  0.35,  0.32,
     +    0.00,  0.02,  0.03,  0.04,  0.09,  0.13,  0.28,  0.37,  0.36,
     +    0.00,  0.01,  0.02,  0.02,  0.04,  0.08,  0.20,  0.33,  0.32,
     +    0.00,  0.02,  0.01,  0.01,  0.04,  0.03,  0.10,  0.24,  0.26,
     +    0.00,  0.01,  0.02,  0.01,  0.03,  0.03,  0.07,  0.18,  0.22,
     +    0.00,  0.02,  0.02,  0.00,  0.03,  0.03,  0.06,  0.15,  0.20,
     +    0.00,  0.02,  0.02,  0.01,  0.02,  0.03,  0.07,  0.17,  0.19,
     +    0.00,  0.02,  0.01,  0.01,  0.02,  0.03,  0.07,  0.20,  0.23/
      DATA F_7  /
     +    0.01,  0.04,  0.04,  0.08,  0.20,  0.27,  0.32,  0.37,  0.38,
     +    0.01,  0.02,  0.06,  0.13,  0.23,  0.31,  0.32,  0.38,  0.38,
     +    0.01,  0.03,  0.04,  0.15,  0.26,  0.32,  0.32,  0.35,  0.42,
     +    0.01,  0.03,  0.03,  0.18,  0.26,  0.34,  0.32,  0.33,  0.33,
     +    0.01,  0.04,  0.06,  0.15,  0.28,  0.29,  0.36,  0.34,  0.41,
     +    0.02,  0.04,  0.05,  0.14,  0.21,  0.28,  0.33,  0.36,  0.35,
     +    0.01,  0.03,  0.05,  0.14,  0.18,  0.24,  0.29,  0.30,  0.34,
     +    0.01,  0.02,  0.06,  0.16,  0.22,  0.28,  0.36,  0.31,  0.40,
     +    0.01,  0.03,  0.04,  0.14,  0.19,  0.25,  0.37,  0.34,  0.39,
     +    0.01,  0.03,  0.05,  0.11,  0.19,  0.24,  0.30,  0.36,  0.35,
     +    0.01,  0.03,  0.04,  0.06,  0.17,  0.18,  0.29,  0.33,  0.34,
     +    0.01,  0.02,  0.04,  0.06,  0.11,  0.22,  0.30,  0.37,  0.32,
     +    0.01,  0.02,  0.02,  0.04,  0.08,  0.17,  0.31,  0.39,  0.37,
     +    0.00,  0.02,  0.01,  0.03,  0.06,  0.11,  0.27,  0.38,  0.35,
     +    0.00,  0.01,  0.02,  0.03,  0.04,  0.07,  0.27,  0.36,  0.36,
     +    0.00,  0.02,  0.02,  0.02,  0.06,  0.09,  0.25,  0.36,  0.37,
     +    0.00,  0.02,  0.02,  0.02,  0.05,  0.11,  0.27,  0.36,  0.35,
     +    0.00,  0.02,  0.02,  0.03,  0.06,  0.12,  0.30,  0.37,  0.37/
      DATA F_8  /
     +    0.00,  0.03,  0.03,  0.12,  0.24,  0.29,  0.29,  0.34,  0.35,
     +    0.01,  0.04,  0.04,  0.13,  0.28,  0.31,  0.33,  0.35,  0.37,
     +    0.01,  0.03,  0.04,  0.12,  0.25,  0.29,  0.31,  0.38,  0.37,
     +    0.01,  0.03,  0.05,  0.18,  0.21,  0.26,  0.26,  0.28,  0.31,
     +    0.01,  0.03,  0.04,  0.13,  0.22,  0.26,  0.34,  0.37,  0.34,
     +    0.01,  0.04,  0.05,  0.13,  0.19,  0.22,  0.31,  0.37,  0.35,
     +    0.01,  0.02,  0.05,  0.11,  0.18,  0.24,  0.32,  0.37,  0.37,
     +    0.01,  0.04,  0.06,  0.11,  0.17,  0.24,  0.29,  0.33,  0.34,
     +    0.01,  0.03,  0.05,  0.10,  0.18,  0.22,  0.30,  0.36,  0.33,
     +    0.01,  0.04,  0.06,  0.10,  0.16,  0.24,  0.31,  0.34,  0.35,
     +    0.01,  0.04,  0.03,  0.08,  0.16,  0.20,  0.30,  0.37,  0.33,
     +    0.01,  0.02,  0.03,  0.05,  0.15,  0.18,  0.28,  0.34,  0.31,
     +    0.01,  0.02,  0.03,  0.05,  0.12,  0.17,  0.28,  0.32,  0.32,
     +    0.00,  0.02,  0.03,  0.03,  0.11,  0.19,  0.31,  0.37,  0.37,
     +    0.00,  0.02,  0.03,  0.03,  0.10,  0.17,  0.28,  0.36,  0.35,
     +    0.00,  0.02,  0.03,  0.03,  0.10,  0.15,  0.30,  0.34,  0.35,
     +    0.00,  0.02,  0.02,  0.03,  0.08,  0.17,  0.30,  0.37,  0.36,
     +    0.00,  0.02,  0.02,  0.05,  0.09,  0.20,  0.30,  0.36,  0.35/
      DATA F_9  /
     +    0.01,  0.03,  0.03,  0.11,  0.16,  0.23,  0.30,  0.36,  0.32,
     +    0.01,  0.03,  0.03,  0.11,  0.20,  0.24,  0.32,  0.37,  0.34,
     +    0.01,  0.03,  0.04,  0.14,  0.18,  0.23,  0.30,  0.34,  0.31,
     +    0.01,  0.03,  0.04,  0.13,  0.18,  0.22,  0.30,  0.37,  0.36,
     +    0.01,  0.03,  0.06,  0.14,  0.21,  0.29,  0.34,  0.37,  0.37,
     +    0.01,  0.03,  0.04,  0.10,  0.19,  0.27,  0.31,  0.39,  0.39,
     +    0.02,  0.03,  0.05,  0.11,  0.18,  0.24,  0.31,  0.37,  0.37,
     +    0.01,  0.03,  0.04,  0.08,  0.15,  0.19,  0.24,  0.35,  0.35,
     +    0.01,  0.03,  0.06,  0.09,  0.14,  0.20,  0.28,  0.30,  0.31,
     +    0.01,  0.02,  0.06,  0.10,  0.15,  0.24,  0.31,  0.38,  0.34,
     +    0.01,  0.03,  0.04,  0.09,  0.12,  0.20,  0.31,  0.40,  0.36,
     +    0.01,  0.02,  0.03,  0.07,  0.10,  0.17,  0.26,  0.35,  0.33,
     +    0.00,  0.02,  0.02,  0.06,  0.10,  0.15,  0.26,  0.29,  0.31,
     +    0.00,  0.02,  0.01,  0.07,  0.13,  0.18,  0.28,  0.35,  0.35,
     +    0.01,  0.02,  0.02,  0.04,  0.12,  0.19,  0.27,  0.36,  0.35,
     +    0.01,  0.03,  0.03,  0.05,  0.11,  0.17,  0.26,  0.34,  0.34,
     +    0.01,  0.03,  0.02,  0.05,  0.11,  0.20,  0.27,  0.33,  0.29,
     +    0.01,  0.02,  0.03,  0.04,  0.10,  0.19,  0.27,  0.35,  0.32/
      DATA F_10 /
     +    0.00,  0.03,  0.04,  0.11,  0.19,  0.26,  0.33,  0.39,  0.38,
     +    0.01,  0.03,  0.03,  0.11,  0.19,  0.23,  0.36,  0.39,  0.40,
     +    0.01,  0.04,  0.05,  0.10,  0.19,  0.23,  0.31,  0.36,  0.38,
     +    0.01,  0.03,  0.05,  0.13,  0.22,  0.26,  0.33,  0.38,  0.38,
     +    0.01,  0.02,  0.05,  0.13,  0.22,  0.28,  0.31,  0.32,  0.31,
     +    0.01,  0.02,  0.04,  0.10,  0.19,  0.22,  0.30,  0.33,  0.35,
     +    0.01,  0.04,  0.04,  0.11,  0.17,  0.19,  0.27,  0.34,  0.36,
     +    0.01,  0.03,  0.04,  0.08,  0.12,  0.15,  0.26,  0.34,  0.34,
     +    0.01,  0.04,  0.04,  0.08,  0.11,  0.14,  0.23,  0.30,  0.27,
     +    0.01,  0.02,  0.05,  0.08,  0.16,  0.15,  0.25,  0.28,  0.31,
     +    0.01,  0.03,  0.04,  0.08,  0.13,  0.21,  0.29,  0.34,  0.32,
     +    0.01,  0.02,  0.03,  0.07,  0.10,  0.14,  0.31,  0.33,  0.37,
     +    0.01,  0.01,  0.03,  0.09,  0.11,  0.14,  0.27,  0.29,  0.36,
     +    0.00,  0.02,  0.03,  0.07,  0.13,  0.19,  0.30,  0.37,  0.37,
     +    0.00,  0.02,  0.03,  0.08,  0.12,  0.19,  0.28,  0.36,  0.37,
     +    0.00,  0.02,  0.02,  0.07,  0.11,  0.18,  0.30,  0.37,  0.36,
     +    0.01,  0.02,  0.03,  0.07,  0.11,  0.18,  0.28,  0.35,  0.32,
     +    0.01,  0.03,  0.02,  0.07,  0.13,  0.19,  0.29,  0.34,  0.35/
      DATA F_11 /
     +    0.01,  0.03,  0.06,  0.11,  0.20,  0.28,  0.33,  0.37,  0.37,
     +    0.01,  0.03,  0.04,  0.13,  0.19,  0.27,  0.31,  0.37,  0.35,
     +    0.01,  0.03,  0.04,  0.13,  0.18,  0.27,  0.32,  0.37,  0.36,
     +    0.01,  0.03,  0.04,  0.10,  0.19,  0.24,  0.30,  0.35,  0.36,
     +    0.01,  0.03,  0.05,  0.08,  0.15,  0.20,  0.23,  0.28,  0.36,
     +    0.01,  0.03,  0.05,  0.07,  0.14,  0.18,  0.26,  0.32,  0.42,
     +    0.01,  0.03,  0.04,  0.07,  0.12,  0.15,  0.24,  0.26,  0.29,
     +    0.01,  0.03,  0.03,  0.06,  0.10,  0.12,  0.16,  0.19,  0.19,
     +    0.01,  0.03,  0.04,  0.05,  0.08,  0.11,  0.15,  0.13,  0.17,
     +    0.01,  0.02,  0.03,  0.06,  0.10,  0.13,  0.17,  0.25,  0.21,
     +    0.01,  0.03,  0.04,  0.08,  0.10,  0.13,  0.19,  0.29,  0.31,
     +    0.01,  0.02,  0.04,  0.07,  0.08,  0.14,  0.23,  0.26,  0.31,
     +    0.01,  0.02,  0.03,  0.08,  0.09,  0.15,  0.23,  0.26,  0.21,
     +    0.01,  0.03,  0.02,  0.09,  0.13,  0.16,  0.27,  0.35,  0.32,
     +    0.00,  0.03,  0.03,  0.09,  0.13,  0.18,  0.30,  0.36,  0.36,
     +    0.01,  0.03,  0.03,  0.08,  0.12,  0.17,  0.31,  0.37,  0.37,
     +    0.01,  0.03,  0.02,  0.08,  0.13,  0.16,  0.30,  0.35,  0.33,
     +    0.01,  0.03,  0.04,  0.08,  0.10,  0.17,  0.27,  0.33,  0.31/
      DATA F_12 /
     +    0.01,  0.03,  0.04,  0.09,  0.16,  0.24,  0.27,  0.31,  0.35,
     +    0.01,  0.03,  0.05,  0.10,  0.16,  0.22,  0.24,  0.33,  0.35,
     +    0.01,  0.03,  0.05,  0.09,  0.18,  0.23,  0.28,  0.35,  0.36,
     +    0.01,  0.03,  0.05,  0.09,  0.17,  0.20,  0.26,  0.35,  0.36,
     +    0.01,  0.02,  0.04,  0.07,  0.11,  0.16,  0.23,  0.32,  0.33,
     +    0.01,  0.02,  0.03,  0.05,  0.11,  0.13,  0.23,  0.26,  0.26,
     +    0.01,  0.02,  0.03,  0.05,  0.09,  0.09,  0.12,  0.13,  0.15,
     +    0.01,  0.02,  0.03,  0.04,  0.07,  0.06,  0.06,  0.07,  0.08,
     +    0.01,  0.03,  0.03,  0.03,  0.05,  0.05,  0.06,  0.08,  0.07,
     +    0.01,  0.03,  0.03,  0.05,  0.07,  0.06,  0.09,  0.12,  0.12,
     +    0.01,  0.02,  0.04,  0.04,  0.08,  0.10,  0.14,  0.21,  0.22,
     +    0.01,  0.02,  0.03,  0.06,  0.08,  0.10,  0.16,  0.29,  0.27,
     +    0.01,  0.02,  0.03,  0.06,  0.07,  0.11,  0.15,  0.23,  0.28,
     +    0.00,  0.02,  0.03,  0.05,  0.10,  0.11,  0.20,  0.26,  0.29,
     +    0.01,  0.03,  0.03,  0.08,  0.11,  0.14,  0.21,  0.27,  0.31,
     +    0.01,  0.02,  0.03,  0.07,  0.12,  0.14,  0.23,  0.26,  0.32,
     +    0.01,  0.03,  0.03,  0.07,  0.09,  0.15,  0.23,  0.24,  0.26,
     +    0.01,  0.02,  0.02,  0.07,  0.09,  0.16,  0.21,  0.27,  0.29/
      DATA F_13 /
     +    0.01,  0.03,  0.04,  0.08,  0.15,  0.17,  0.26,  0.32,  0.34,
     +    0.01,  0.03,  0.02,  0.07,  0.16,  0.17,  0.25,  0.34,  0.34,
     +    0.01,  0.03,  0.04,  0.08,  0.11,  0.19,  0.24,  0.29,  0.37,
     +    0.00,  0.02,  0.03,  0.05,  0.11,  0.14,  0.22,  0.22,  0.24,
     +    0.00,  0.02,  0.03,  0.06,  0.09,  0.09,  0.14,  0.17,  0.18,
     +    0.01,  0.02,  0.03,  0.04,  0.08,  0.07,  0.11,  0.10,  0.11,
     +    0.00,  0.03,  0.03,  0.04,  0.06,  0.04,  0.07,  0.06,  0.04,
     +    0.01,  0.02,  0.03,  0.02,  0.04,  0.05,  0.04,  0.02,  0.03,
     +    0.01,  0.02,  0.03,  0.03,  0.04,  0.04,  0.03,  0.04,  0.03,
     +    0.01,  0.02,  0.02,  0.02,  0.04,  0.03,  0.05,  0.06,  0.05,
     +    0.01,  0.03,  0.02,  0.04,  0.06,  0.05,  0.07,  0.10,  0.11,
     +    0.01,  0.02,  0.02,  0.04,  0.07,  0.05,  0.11,  0.12,  0.13,
     +    0.00,  0.02,  0.03,  0.05,  0.06,  0.07,  0.11,  0.18,  0.21,
     +    0.01,  0.02,  0.03,  0.05,  0.07,  0.09,  0.13,  0.22,  0.24,
     +    0.00,  0.02,  0.03,  0.06,  0.08,  0.09,  0.15,  0.22,  0.26,
     +    0.01,  0.03,  0.03,  0.06,  0.09,  0.11,  0.15,  0.24,  0.26,
     +    0.01,  0.02,  0.02,  0.06,  0.09,  0.10,  0.13,  0.19,  0.19,
     +    0.01,  0.02,  0.03,  0.04,  0.09,  0.10,  0.21,  0.22,  0.29/
      DATA F_14 /
     +    0.01,  0.02,  0.03,  0.05,  0.10,  0.10,  0.16,  0.20,  0.18,
     +    0.01,  0.03,  0.03,  0.04,  0.10,  0.09,  0.16,  0.16,  0.18,
     +    0.01,  0.03,  0.04,  0.05,  0.10,  0.12,  0.15,  0.14,  0.14,
     +    0.01,  0.03,  0.04,  0.04,  0.07,  0.07,  0.09,  0.08,  0.10,
     +    0.01,  0.03,  0.03,  0.03,  0.06,  0.05,  0.06,  0.05,  0.08,
     +    0.01,  0.02,  0.03,  0.02,  0.04,  0.03,  0.05,  0.02,  0.04,
     +    0.01,  0.03,  0.02,  0.02,  0.04,  0.02,  0.03,  0.02,  0.03,
     +    0.01,  0.02,  0.02,  0.03,  0.05,  0.03,  0.02,  0.02,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.04,  0.02,  0.02,  0.01,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.04,  0.03,  0.03,  0.03,  0.03,
     +    0.00,  0.03,  0.02,  0.01,  0.03,  0.02,  0.05,  0.05,  0.05,
     +    0.01,  0.02,  0.02,  0.03,  0.04,  0.03,  0.05,  0.06,  0.07,
     +    0.01,  0.02,  0.02,  0.02,  0.04,  0.04,  0.06,  0.07,  0.10,
     +    0.00,  0.02,  0.03,  0.04,  0.05,  0.05,  0.07,  0.08,  0.12,
     +    0.01,  0.02,  0.02,  0.04,  0.06,  0.07,  0.10,  0.11,  0.12,
     +    0.00,  0.02,  0.02,  0.04,  0.07,  0.07,  0.08,  0.10,  0.11,
     +    0.01,  0.02,  0.02,  0.05,  0.06,  0.08,  0.07,  0.08,  0.10,
     +    0.01,  0.02,  0.02,  0.04,  0.05,  0.06,  0.09,  0.12,  0.13/
      DATA F_15 /
     +    0.01,  0.03,  0.03,  0.04,  0.07,  0.06,  0.07,  0.08,  0.08,
     +    0.01,  0.03,  0.04,  0.04,  0.06,  0.05,  0.07,  0.07,  0.09,
     +    0.00,  0.02,  0.02,  0.04,  0.06,  0.05,  0.06,  0.07,  0.06,
     +    0.00,  0.03,  0.02,  0.03,  0.05,  0.04,  0.05,  0.03,  0.04,
     +    0.01,  0.02,  0.03,  0.02,  0.05,  0.03,  0.03,  0.02,  0.03,
     +    0.01,  0.02,  0.03,  0.02,  0.04,  0.03,  0.02,  0.02,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.03,  0.03,  0.01,  0.02,  0.02,
     +    0.01,  0.03,  0.02,  0.02,  0.04,  0.01,  0.01,  0.02,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.03,  0.02,  0.01,  0.02,  0.02,
     +    0.00,  0.02,  0.02,  0.02,  0.03,  0.02,  0.02,  0.01,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.03,  0.02,  0.03,  0.02,  0.02,
     +    0.01,  0.02,  0.02,  0.02,  0.03,  0.03,  0.03,  0.03,  0.03,
     +    0.01,  0.02,  0.03,  0.01,  0.03,  0.02,  0.04,  0.04,  0.04,
     +    0.01,  0.03,  0.03,  0.02,  0.05,  0.02,  0.04,  0.02,  0.04,
     +    0.01,  0.02,  0.02,  0.02,  0.04,  0.04,  0.04,  0.05,  0.06,
     +    0.01,  0.02,  0.03,  0.03,  0.05,  0.05,  0.04,  0.04,  0.04,
     +    0.01,  0.02,  0.02,  0.03,  0.05,  0.05,  0.05,  0.05,  0.04,
     +    0.01,  0.02,  0.02,  0.03,  0.04,  0.05,  0.06,  0.06,  0.06/
C
C..........
C
      IF (FLAG)             THEN
        DO I = 0,17
          A(I+10) = I/10.+0.05
        END DO
C
        DO I = 20,34
          A(I+ 8) = I/10.+0.05
        END DO
C
        FLAG = .FALSE.
      END IF
C
      X(1) = P
      X(2) = FI
      X(3) = ETA
C
      SAMUS_LEVEL1_EFF_MU_MINUS_1A   = FINT(3,X,NA,A,F_MU_5)
      IF ((ETA .LE. ETA_MIN) .OR. (ETA .GE. ETA_MAX)) THEN
        SAMUS_LEVEL1_EFF_MU_MINUS_1A = 0.0
      END IF
C
  999 RETURN
      END
