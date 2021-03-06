      SUBROUTINE MAKE_DECAY_VECTOR(DECAY_MASS,CTHETA,PHI,
     &  MASS1,MASS2,P41,P42)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ASSUMING A PARTICLE OF MASS DECAY_MASS
C-   DECAYS INTO TWO PARTICLES, OF MASS =MASS1,MASS2 AND  THE FIRST OF
C-   WHICH  HAS COS THETA AND PHI IN DECAY REST FRAME OF CTHETA,PHI
C-   RETURNS THE 4 VECTOR OF THE DECAY PRODUCT IN THE REST FRAME
C-   OF THE DECAYING PARTICLES
C-
C-   Inputs  : DECAY_MASS = MASS OF DECAYING PARTICLE
C-             CTHETA,PHI = COS THETA AND PHI OF 1ST DECAY PRODUCT
C-             MASS1,MASS2 = MASS OF DECAY PRODUCTS
C-   Outputs : P41 = 4 VECTOR OF 1ST DECAY PRODUCT
C-             P42 = 4 VECTOR OF 2ND DECAY PRODUCT
C-   Controls:
C-
C-   Created  15-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION    DECAY_MASS,CTHETA,PHI,MASS,P41(*),P42(*)
      DOUBLE PRECISION    MASS1,MASS2
      DOUBLE PRECISION    S,M1S,M2S
      DOUBLE PRECISION    LAMBDA
      DOUBLE PRECISION    P2,P
      DOUBLE PRECISION    STHETA
      DOUBLE PRECISION    X,Y,Z
      INTEGER I
C----------------------------------------------------------------------
      LAMBDA(X,Y,Z) = (X-Y-Z)**2 -4*Y*Z
      S=DECAY_MASS**2
      M1S=MASS1**2
      M2S=MASS2**2
      STHETA = SQRT(1.-CTHETA**2)
C
      P2 = LAMBDA(S,M1S,M2S)/(4.0*S)
      P  = SQRT(P2)   !DECAY MOMENTUM IN CMS
C
      P41(1) = P*STHETA*COS(PHI)
      P41(2) = P*STHETA*SIN(PHI)
      P41(3) = P*CTHETA
C
      CALL MAKE_ON_SHELL(P41,MASS1,P41,2)
C
      DO I = 1 , 3
        P42(I) = -P41(I)
      ENDDO
      CALL MAKE_ON_SHELL(P42,MASS2,P42,2)
C
999   RETURN
      END
