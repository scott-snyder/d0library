      SUBROUTINE STEP1_MASS(MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : MASS 
C-   Controls: 
C-
C-   Created  13-NOV-1991   Chris Murphy
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      REAL Q1(3),Q2(3),Q(3),E1,E2,S2,MASS
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC/LIST'
C
      Q1(1)=STR(1,1)*SIN(STR(2,1))*COS(STR(3,1))
      Q1(2)=STR(1,1)*SIN(STR(2,1))*SIN(STR(3,1))
      Q1(3)=STR(1,1)*COS(STR(2,1))
      Q2(1)=STR(1,2)*SIN(STR(2,2))*COS(STR(3,2))
      Q2(2)=STR(1,2)*SIN(STR(2,2))*SIN(STR(3,2))
      Q2(3)=STR(1,2)*COS(STR(2,2))
      E1=SQRT(STR(1,1)**2+STR(4,1)**2)
      E2=SQRT(STR(1,2)**2+STR(4,2)**2)
      S2=(E1+E2)**2-(Q1(1)+Q2(1))**2-(Q1(2)+Q2(2))**2
     +             -(Q1(3)+Q2(3))**2 
      MASS=SQRT(S2)
C
      Q(1)=Q1(1)+Q2(1)
      Q(2)=Q1(2)+Q2(2)
      Q(3)=Q1(3)+Q2(3)
      STR(1,3)=SQRT(Q(1)**2+Q(2)**2+Q(3)**2)
      STR(2,3)=ACOS(Q(3)/STR(1,3))
      STR(3,3)=ATAN2(Q(2),Q(1))
      IF (STR(3,3).LT.0.) STR(3,3)=STR(3,3)+TWOPI
  999 RETURN
      END
