      REAL FUNCTION EJETS_TAG_SERR(MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INTERPOLATES EJETS_TAG SIGMA.BRANCHING RATIO AS A
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
      EJETS_TAG_SERR = DIVDIF(EJETS_TAG_ERR,TM_EFF,NMASS,MASS,MPOL)
C
      RETURN
      END
