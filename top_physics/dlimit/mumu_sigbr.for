      REAL FUNCTION MUMU_SIGBR(MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INTERPOLATES MUMU SIGMA.BRANCHING RATIO AS A
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
      MUMU_SIGBR = DIVDIF(MUMU_SBR,TM_EFF,NMASS,MASS,MPOL)
C
      RETURN
      END
