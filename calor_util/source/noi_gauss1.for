      REAL FUNCTION NOI_GAUSS1(MEAN,SIGMA,SEED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gaussian generator using sum of 12 numbers
C-
C-   Inputs  : MEAN = Gaussian mean
C-             SIGMA = Sigma of the gaussian
C-             SEED = SEED to VAX RAN generator
C-   Outputs : NOI_GAUSS1 = random number picked for gaussian
C-               distribution with above mean and sigma
C-   Controls:
C-
C-   Created   16-SEP-1991   Peter Nemethy and Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL R,SIGMA,MEAN,RNDM
      INTEGER*4 I,SEED
      CALL RDMIN(SEED)
      R=0.
      DO 20 I=1,12
        R=R+RNDM(I)
   20 CONTINUE
      R=R-6.
      R=R*SIGMA
      NOI_GAUSS1=R+MEAN
      CALL RDMOUT(SEED)
  999 RETURN
      END
