      REAL FUNCTION MUJETS_TAG_SERR(MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INTERPOLATES MUJETS_TAG SIGMA.BRANCHING RATIO AS A
C-   FUNCTION OF MASS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    MASS,DIVDIF
      INCLUDE 'D0$INC:SIG_EFF.INC'
C----------------------------------------------------------------------
C
      MUJETS_TAG_SERR = DIVDIF(MUJETS_TAG_ERR,TM_EFF,NMASS,MASS,MPOL)
C
      RETURN
      END
