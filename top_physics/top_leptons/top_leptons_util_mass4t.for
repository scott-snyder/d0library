      FUNCTION TOP_LEPTONS_UTIL_MASST(PT1,PT2,DPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Transverse Mass of 2 particles
C-
C-   Returned value  : Transverse Mass
C-
C-   Inputs  : PT of particle 1, PT particle 2, & dPhi between
C-
C-   Outputs : Tranverse mass of pair
C-
C-   Controls: None
C-
C-   Created   25-Aug-1992   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PT1,PT2,DPHI,MTSQ,TOP_LEPTONS_UTIL_MASST
C
      MTSQ=2*PT1*PT2*(1-COS(DPHI))
      IF(MTSQ.GT.0.) THEN
        TOP_LEPTONS_UTIL_MASST=SQRT(MTSQ)
      ELSE
        TOP_LEPTONS_UTIL_MASST=0.0
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
