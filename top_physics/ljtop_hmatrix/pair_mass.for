      SUBROUTINE PAIR_MASS(P1,P2,PSUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-    add a pair of 4-vectors and calculate mass
C-
C-   Returned value  : 
C-   Inputs  : 
C-     P1(4)  = 1st 4-vector
C-     P2(4)  = 2nd 4_vector
C-   Output :
C-     PSUM(5)= 4-vector + mass of p1+p2, -sqrt(-m**2) if m**2 < 0 
C-
C-   Created  26-NOV-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    P1(4),P2(4),PSUM(5),MSQ
C----------------------------------------------------------------------
      CALL VADD(P1,P2,PSUM,4)
      MSQ=PSUM(4)**2-PSUM(1)**2-PSUM(2)**2-PSUM(3)**2
      IF ( MSQ.GT.0 ) THEN
        PSUM(5)=SQRT(MSQ)
      ELSE
        PSUM(5)=-SQRT(-MSQ+.00001)
      ENDIF
  999 RETURN
      END
