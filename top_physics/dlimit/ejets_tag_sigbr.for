      REAL FUNCTION EJETS_TAG_SIGBR(MASS)
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
      EJETS_TAG_SIGBR = DIVDIF(EJETS_TAG_SBR,TM_EFF,NMASS,MASS,MPOL)
C
      RETURN
      END
