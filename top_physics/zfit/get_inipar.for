      SUBROUTINE GET_INIPAR(XP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-FEB-1993   Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'D0$INC:FIT_TWO.INC'
      PARAMETER (NPARM=5)
C      REAL  P1(4),P2(4),P(4),CMSTH,CMSPHI,PT,XP(NPARM)
      DIMENSION P1(4),P2(4),P(4),P11(4),XP(NPARM)
C      REAL  P11(4),ZMASS,EMASS,PI
      DATA ZMASS/91.17/
      DATA EMASS/.000511/
      DATA PI/3.14285/
C----------------------------------------------------------------------
      DO I=1,3
      P1(I)=XM(I)
      P2(I)=XM(3+I)
      ENDDO
      P1(4)=SQRT(P1(1)**2+P1(2)**2+P1(3)**2+EMASS**2)
      P(1)=XM(7)
      P(2)=XM(8)
      P(3)=P1(3)+P2(3)
      P(4)=SQRT(P(1)**2+P(2)**2+P(3)**2+ZMASS**2)
      CALL LORENTZ4(P,P1,P11)
C
      XP(1)=P(1)
      XP(2)=P(2)
      XP(3)=P(3)
      PT=SQRT(P11(1)**2+P11(2)**2)
      P11(4)=SQRT(PT**2+P11(3)**2+P11(4)**2)
C      XP(4)=ATAN(PT/P11(3))
C      XP(5)=ATAN(P11(2)/P11(1))
      XP(4)=ACOS(P11(3)/P11(4))
      XP(5)=ATAN2(P11(2),P11(1))
      IF(XP(5) .LT. 0.)XP(5) = 2.*PI +XP(5)
  999 RETURN
      END
