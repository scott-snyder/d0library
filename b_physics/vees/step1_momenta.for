      SUBROUTINE STEP1_MOMENTA(P1, P2, P, PT, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate momenta of decaying particle   
C-                         and products, using measured spherical 
C-                         angles for tracks with supposition, that 
C-                         all three tracks are in one plane 
C-   Inputs  :             Theta angles in STR(2,k)
C-                         Phi   angles in STR(3,K)
C-                         Masses       in STR(4,k)
C-                         p1,p2 - momenta of products
C-   Outputs :             p - momentum of decaying particle   
C-                         p1,p2 - momenta of products
C-                         pt - pt-momentum  of decay particle with 
C-                         respect to the decaying particle
C-   Controls: 
C-
C-   Created  30-SEP-1991   Vladimir Burtovoy
C-   Updated  21-OCT-1991   Daria Zieminska: add the option of using the
C-                          calorimeter information; D0 standards 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      REAL Q1(3),Q2(3),Q(3),CL1,CL2,CO,SL1,SL2,AB,PP,P,P1,P2,PT
      LOGICAL OK
C
      Q1(1) = SIN(STR(2,1))*COS(STR(3,1))
      Q1(2) = SIN(STR(2,1))*SIN(STR(3,1))
      Q1(3) = COS(STR(2,1))
      Q2(1) = SIN(STR(2,2))*COS(STR(3,2))
      Q2(2) = SIN(STR(2,2))*SIN(STR(3,2))
      Q2(3) = COS(STR(2,2))
      Q(1)  = SIN(STR(2,3))*COS(STR(3,3))
      Q(2)  = SIN(STR(2,3))*SIN(STR(3,3))
      Q(3)  = COS(STR(2,3))
      CO    = Q1(1)*Q2(1) + Q1(2)*Q2(2) + Q1(3)*Q2(3) 
      CL1   =  Q(1)*Q1(1) +  Q(2)*Q1(2) +  Q(3)*Q1(3) 
      CL2   =  Q(1)*Q2(1) +  Q(2)*Q2(2) +  Q(3)*Q2(3) 
      SL1   = SQRT(1. - CL1**2)
      SL2   = SQRT(1. - CL2**2)
      IF (P1 .GT. 0.0 .AND. P2 .GT. 0.0) THEN
        P   = SQRT(P1**2 + P2**2 + 2.*P1*P2*CO)
        PT  = SL1*P1
        STR(1,3) = P 
        OK  = .TRUE.
        RETURN
      ELSEIF(P1 .GT. 0.0 .AND. P2 .LE. 0.0) THEN
        IF(SL2 .EQ. 0.0) THEN
          OK= .FALSE.
          RETURN
        ENDIF
        PT  = P1*SL1
        P2  = PT/SL2
        P   = P1*CL1 + P2*CL2
        STR(1,2) = P2  
        STR(1,3) = P 
        OK  = .TRUE.
        RETURN
      ELSEIF(P1 .LE. 0.0 .AND. P2 .GT. 0.0) THEN
        IF(SL1 .EQ. 0.0) THEN
          OK= .FALSE.
          RETURN
        ENDIF
        PT  = P2*SL2
        P1  = PT/SL1
        P   = P1*CL1 + P2*CL2
        STR(1,1) = P1
        STR(1,3) = P 
        OK  = .TRUE.
        RETURN
      ELSE
        AB  = 2.*SL1*SL2*(1. - CO)
        IF( AB .LE. 0.0) THEN
          OK= .FALSE.
          RETURN
        ENDIF  
        PP  = SQRT((STR(4,3)**2 - STR(4,1)**2 - STR(4,2)**2)/AB)       
        P   = PP*ABS(SL1*CL2 + SL2*CL1)
        P1  = SL2*PP
        P2  = SL1*PP
        PT  = SL1*P1
        STR(1,1) = P1
        STR(1,2) = P2
        STR(1,3) = P
        OK  = .TRUE.
        RETURN
      ENDIF
      END
