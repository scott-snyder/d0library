      SUBROUTINE CHISQ_FULL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Work out chisquared for Full H matrix
C-
C-   Inputs  : 
C-   Outputs : CHSQF  = Chisquared
C-             PROBF  = Probability to exceed CHSQF. In CHMATR.INC
C-   Controls: 
C-
C-   Created  25-OCT-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      INTEGER I,J
      REAL    DI,DJ,PROB
      INTEGER IDEGF,NVISIBLE
C----------------------------------------------------------------------
      CHSQF = 0.0
      IDEGF = 0
      NVISIBLE = NDIMVFR
C
      DO 100 I = 1 , NVISIBLE             ! Sum over visible energy
        IDEGF = IDEGF + 1
        DI = QUAN(I) - AVR(I)
        DO 200 J = 1, NVISIBLE
          DJ = QUAN(J) - AVR(J)
          CHSQF = CHSQF + DI*HMAT_VIS(J,I)*DJ
  200   CONTINUE
  100 CONTINUE
C
      PROBF = PROB(CHSQF,IDEGF)
C
  999 RETURN
      END
