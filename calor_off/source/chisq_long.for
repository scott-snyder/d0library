      SUBROUTINE CHISQ_LONG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : work out chisquared for 
C-                         longitudinal matrix only
C-
C-   Inputs : 
C-   Outputs  : CHSQL = Longitudinal chisqured
C-              PROBL = Probability to exceed CHSQL. In CHMATR.INC
C-   Controls: 
C-
C-   Created  25-OCT-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      INTEGER I,J
      REAL    DI,DJ,PROB
      INTEGER IDEGL
C----------------------------------------------------------------------
      CHSQL = 0.0
      IDEGL = NDIMVL
      DO 300 I = 1,NDIMVL              ! Summing over visible energy.
        DI = QUANTL(I)-AVERL(I)
        DO 400 J = 1,NDIMVL
          DJ = QUANTL(J)-AVERL(J)
          CHSQL = CHSQL + DI*HMATRL_VIS(J,I)*DJ
  400   CONTINUE
  300 CONTINUE
      IF(CHSQL .LT. 0)  CHSQL = 9999.
      PROBL = PROB(CHSQL,IDEGL)
  999 RETURN
      END
