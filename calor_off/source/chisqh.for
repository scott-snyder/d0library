      SUBROUTINE CHISQH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WORK OUT CHISQUARED USING HMATRIX
C-
C-   Inputs  : QUANT,QUANTL IN /CHMATR/
C-   Routine assumes call to CHQUAN(1)
C-
C-   Outputs : CHSQF,PROBF Chisquared and probability for Full Hmatrix
C-             CHSQL,PROBL Chisquared and probability for Longitudinal
C-             H matrix only . In CHMATR.INC
C-   Controls:
C-
C-   Created   4-JUN-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
C----------------------------------------------------------------------
      CALL CHISQ_LONG      ! LONGITUDINAL CHISQUARED
      CALL CHISQ_FULL      ! FULL CHISQUARED
C
  999 RETURN
      END
