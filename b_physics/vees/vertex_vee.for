      SUBROUTINE VERTEX_VEE(V0,V1,DC1,V2,DC2,VX,EVX,PHI,THE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-DEC-1993   V. Burtovoy
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL V1(3),DC1(3),V2(3),DC2(3),VX(3),EVX(3),RVX1(3),C(3)
      INTEGER IER
      REAL VDOT, DS1(3),DS2(3),RVX2(3),X(3) 
      REAL V0(3),D(3),PHI,THE
      DOUBLE PRECISION N1N2,CN1,CN2,DET,S1,S2
      INCLUDE 'D0$INC:PI.DEF/LIST'

      IER = 0
      CALL VSUB(V2,V1,C,3)
      N1N2 = VDOT(DC1(1),DC2(1),3)
      CN1  = VDOT(  C(1),DC1(1),3)
      CN2  = VDOT(  C(1),DC2(1),3)
      DET  = 1.D0 - N1N2*N1N2
      IF(DET .EQ. 0.D0) THEN
        IER = -1
        GO TO 999
      ENDIF
      S1 = ( CN1 - CN2*N1N2 )/DET
      S2 = (-CN2 + CN1*N1N2 )/DET
      CALL VSCALE(DC1, S1, DS1, 3)
      CALL VSCALE(DC2, S2, DS2, 3)
      CALL VADD(V1,DS1,RVX1,3)
      CALL VADD(V2,DS2,RVX2,3)
      CALL VSUB(RVX2,RVX1,X,3)
      CALL VSCALE(X, 0.5d0, EVX, 3)
      CALL VADD(RVX1,EVX,VX,3)
      CALL VSUB(VX,V0,D,3)
      PHI = ATAN2(D(2), D(1))
      IF( PHI .LT. 0.0) PHI = PHI + TWOPI
      THE = ATAN2(SQRT(D(1)**2 + D(2)**2), D(3))
  999 RETURN
      END
