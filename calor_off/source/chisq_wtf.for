      SUBROUTINE CHISQ_WTF(ENERGY,WCHSQF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Work out chisquared for Full H matrix
C-
C-   Inputs  : ENERGY of cluster
C-   Outputs : WCHSQF  = Chisquared
C-   Controls: NONE
C-
C-   Created  11-FEB-1990 N.A. GRAF
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      INTEGER I,J
      REAL    DI,DJ,ENERGY,WCHSQF,WEIGHT
      INTEGER IDEGF,IER
      LOGICAL FIRST
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('CHISQUARE_WEIGHT',WEIGHT,IER)
        CALL EZRSET
      ENDIF
      WCHSQF = 0.0
      DO 100 I = 1 , NDIMVFR             ! Sum over visible energy
        DI = QUAN(I) - AVR(I)
        DO 200 J = 1, NDIMVFR
          DJ = QUAN(J) - AVR(J)
          WCHSQF = WCHSQF + DI*HMAT_VIS(J,I)*DJ
  200   CONTINUE
  100 CONTINUE
C
      WCHSQF = WCHSQF*(ENERGY**WEIGHT)
C
  999 RETURN
      END
