      REAL FUNCTION EE_SERR(MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INTERPOLATES EE SIGMA.BRANCHING RATIO AS A
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
      EE_SERR = DIVDIF(EE_ERR,TM_EFF,NMASS,MASS,MPOL)
C
      RETURN
      END
