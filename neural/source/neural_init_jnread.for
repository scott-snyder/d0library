      SUBROUTINE NEURAL_INIT_JNREAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the Neural Network parameters from
C-   NEURAL_RCP into the JETNET common block /JNDAT1/ TO OVERRIDE
C-   PARAMETERS FROM INPUT WEIGHT FILE
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   CREATED 1-DEC-1994   Chip Stewart   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL NEURAL_INIT_JNDAT2
      CALL NEURAL_INIT_JNDAT1
  999 RETURN
      END
