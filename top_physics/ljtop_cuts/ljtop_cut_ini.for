      LOGICAL FUNCTION LJTOP_CUT_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      initialize TOP_CUTS package
C-
C-   Created   4-MAY-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      LJTOP_CUT_INI=.TRUE.
      CALL LJTOP_READ_CUTS
  999 RETURN
      END
