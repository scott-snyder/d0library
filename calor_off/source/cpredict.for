      SUBROUTINE CPREDICT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PREDICT FH,CRACK AND CRYOSTAT ENERGIES
C-                         USING H MATRIX
C-
C-   Inputs  :
C-   Outputs : PRED,DELPRED . PREDICTED AND PREDICTED-AVERAGE VALUES
C-             FOR FH, CRACK AND CRYOSTAT ENERGIES.
C-   Controls:
C-
C-   Created  11-AUG-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      CALL CPREDICT_LONG                ! Longit. prediction. Crack+cryo
      CALL CPREDICT_FULL                ! Full prediction .Crack,cryo,position
C
  999 RETURN
      END
