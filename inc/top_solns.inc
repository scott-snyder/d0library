C----------------------------------------------------------------------
C-
C-   Created  26-JAN-1993   Rajendran Raja
C-   SAVES THE top solutions. there are 4 solutions
C-   for any given theta and phi. 2 W's for 1st W and 2 for the second
C-
C----------------------------------------------------------------------
      COMMON /TOP_SOLNS/ PHI_MIN,WEIGHT(2,2),DEL_MASS(2,2),
     &  LAMBDA_MIN(2,2),
     &  PNUT1_MIN(2,2,2),PNUT2_MIN(2,2,2),W1_MIN(4,2,2),W2_MIN(4,2,2),
     &  TOP1_MIN(5,2,2),TOP2_MIN(5,2,2),SOL_MIN(2,2)
C
      DOUBLE PRECISION    WEIGHT,DEL_MASS,PHI_MIN,LAMBDA_MIN
      DOUBLE PRECISION    PNUT1_MIN,PNUT2_MIN,W1_MIN,W2_MIN,
     &  TOP1_MIN,TOP2_MIN
      LOGICAL SOL_MIN
C
C PHI_MIN IS THE current PHI of the 1st neutrino ET
C WEIGHT is the parton model weight for each configuration
C DEL_MASS is the difference in mass between the solutions at minimum
C Lambda_min are the lambda values at minimum
C SOL_MIN is true if minimum solution exists
C PNUT1_MIN etc are the 4 vectors for each solution
