      REAL FUNCTION TOT_SIGBR(MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOTAL EFF*BR
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-FEB-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL EE_SIGBR,EMU_SIGBR,EJETS_SIGBR,EJETS_TAG_SIGBR,
     &               MUMU_SIGBR,MUJETS_SIGBR,MUJETS_TAG_SIGBR
      REAL    MASS
C----------------------------------------------------------------------
      TOT_SIGBR = EE_SIGBR(MASS)+EMU_SIGBR(MASS)+EJETS_SIGBR(MASS)+
     &            EJETS_TAG_SIGBR(MASS)+
     &            MUMU_SIGBR(MASS)+MUJETS_SIGBR(MASS)+
     &            MUJETS_TAG_SIGBR(MASS)
  999 RETURN
      END
