      SUBROUTINE GUICAL(I,J,K)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Init Calorimeter ZCEDEX menu items
C-                            Called by ====> GUINTI
C-
C-   Inputs  : I = Menu #
C-             J = First item #
C-   Outputs : K = Last item # used
C-
C-   Created  8-MAY-1987   S.Linn
C-   Updated  23-JUL-1987   A.M.Jonckkhere   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
      INTEGER I,J,K
C----------------------------------------------------------------------
C
C
      K = J
      CALL ZCBOOK(K,'SHOWER-PARAMETERS$',I)
C?      K = K + 1
C?      CALL ZCBOOK(K,'SAVE-GEOMETRY$',I)
C?      K = K + 1
C?      CALL ZCBOOK(K,'GET-MAP$',I)
C?      K = K + 1
C?      CALL ZCBOOK(K,'OPEN-WINDOW$',I)
C?      K = K + 1
C?      CALL ZCBOOK(K,'CLOSE-WINDOW$',I)
C
  999 RETURN
      END
