      SUBROUTINE FWMOMENT(NJET,P4,C)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine to calculate 2D Fox-Wolfram moments for
C-                         NJET jets with 4-momenta  P4(1:4,1:NJET)
C-                         ( Nuclear Physics, B149(1979), p413 )
C-
C-   Inputs:
C-   NJET      = number of jets
C-   P4(4,NJET)= jets 4-momenta
C-
C-   Outputs : C(10)  :    2D moments normalized to C(0) (sum(pT))**2
C-
C-   Created  10-FEB-1991  Daria Zieminska 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NJET
      REAL P4(4,*),C(10)
      REAL COSIJ,P1,P2,P1P2,PHI,NUM,DEN 
      INTEGER I1,I2,IC
C----------------------------------------------------------------------
C
      CALL VZERO(C,10)
      DEN=0.
      IF (NJET.LT.2) GO TO 999
      DO 100 I1=1,NJET
        P1=P4(1,I1)**2+P4(2,I1)**2
        DEN=DEN+SQRT(P1) 
        DO 200 I2=1,NJET
          P2=P4(1,I2)**2+P4(2,I2)**2
          P1P2=SQRT(P1*P2)
          IF (I1.NE.I2) THEN
            COSIJ=P4(1,I1)*P4(1,I2)+P4(2,I1)*P4(2,I2) 
            C(1)=C(1)+COSIJ
            COSIJ=COSIJ/P1P2
            IF (ABS(COSIJ).GE.1) COSIJ=COSIJ/1.00001
            PHI=ACOS(COSIJ)
            DO 300 IC=2,10
              C(IC)=C(IC)+P1P2*COS(IC*PHI)
  300       CONTINUE
          ELSE
            DO 400 IC=2,10
              C(IC)=C(IC)+P1 
  400       CONTINUE
          END IF
  200   CONTINUE
  100 CONTINUE
      DO 500 IC=1,10
        C(IC)=C(IC)/DEN**2
  500 CONTINUE
  999 RETURN
      END
C
