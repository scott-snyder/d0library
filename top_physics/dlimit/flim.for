      REAL FUNCTION FLIM(MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SIG_EFF.INC'
      REAL    MASS
      REAL    TOT_SIGBR
      REAL    CROSS_SECT
C----------------------------------------------------------------------
      FLIM = TOT_SIGBR(MASS)*
     &  CROSS_SECT(MASS) - NEVENTS_DISC/LUMIN
  999 RETURN
      END
