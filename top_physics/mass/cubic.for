      SUBROUTINE CUBIC(A,Z1,Z2,Z3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SOLVE THE CUBIC EQUATION
C-      
C-             A(1)*X**3 + A(2)*X**2 + A(3)*X + A(4) = 0
C-
C-   Inputs  : A
C-   Outputs : Z1,Z2,Z3 ARE COMPLEX NUMBERS WHICH ARE THE ROOTS
C-   
C-   Controls: 
C-
C-   Created  17-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    A(*)
      COMPLEX U1,V1,Z1,Z2,Z3,ZZ,U3,AI
      REAL    G,H,DEL
      COMPLEX DEL1,DEL2,DEL3
      DATA AI/(0.,1.0)/
C----------------------------------------------------------------------
C
      H = (3.0*A(1)*A(3)-A(2)*A(2))/(9.0*A(1)*A(1))
      G =  2.0*A(2)*A(2)*A(2)-9.0*A(1)*A(2)*A(3) + 27.0*A(1)*A(1)*A(4)
      G =  G /(27.0*A(1)*A(1)*A(1))
C
      ZZ = G*G + 4.0*H*H*H
      U3 = (-G+SQRT(ZZ))/2.0
C
      U1 = U3**0.333333333333
C
      V1 = -H/U1
C
      Z1 = U1 + V1
      Z2 = (-(U1+V1) + (U1-V1)*SQRT(3.0)*AI)/2.
      Z3 = (-(U1+V1) - (U1-V1)*SQRT(3.0)*AI)/2.0
C
      DEL = A(2)/(3.0*A(1))
      Z1 = Z1 - DEL
      Z2 = Z2 - DEL
      Z3 = Z3 - DEL
C
      DEL1 = A(1)*Z1**3 + A(2)*Z1**2 + A(3)*Z1 + A(4)
      DEL2 = A(1)*Z2**3 + A(2)*Z2**2 + A(3)*Z2 + A(4)
      DEL3 = A(1)*Z3**3 + A(2)*Z3**2 + A(3)*Z3 + A(4)
  999 RETURN
      END
