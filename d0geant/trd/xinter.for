      FUNCTION XINTER (V,TET2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used for X ray generation according to Garibian et
C-                         al. (irregular radiator).Called in DWDEGR
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created                B. Mansoulie   
C-   Updated  12-JUL-1989   A. Zylberstejn   
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C      IMPLICIT REAL*8 (A-H,O-Z)
C
c     COMMON/PIPI/PI,  TWOPI
      INCLUDE 'D0$INC:GRUTIL.INC'
C
      REAL *8  AM(2),PSI(2),RH(2),ZH(2)
      REAL*8 AGA ,AGH, AHAHB,AHAHBN,ARG ,ARGU,C1,C2,C3,F1,F2,FA
      REAL*8 HAHB,HAHBN,HAHBNL,TET2,V,XINTER
      INTEGER I
      REAL*8 RHAHB,RQ,RU,RUP,RVP,RW,RWP,S1,S2,S3,WPSQ,WSQ,ZHAHB
      REAL*8 ZQ,ZU,ZUP,ZVP
      REAL*8 ZW,ZWP
C
            DO 1 I=1,2
        AGA=AG(I)
        FA=V*(TT(I)+TET2)
        AGH=AGA/2.D0
        ARGU=C1MA(I)+(FA*C2MA(I))**2
C        AML=AGH*LOG(ARGU)
        IF(AGH.LT.100.) THEN
          AM(I)=ARGU**(-AGH)
          ARG=FA*CPSI(I)
          PSI(I)=-AGA*ATAN(ARG)
        ELSE
          AM(I)=AMLIM(I)
          PSI(I)=-FA*D(I)
        ENDIF
        RH(I)=AM(I)*COS(PSI(I))
        ZH(I)=AM(I)*SIN(PSI(I))
    1 CONTINUE
C
      HAHB=AM(1)*AM(2)
      AHAHB=PSI(1)+PSI(2)
      RHAHB=RH(1)*RH(2)-ZH(1)*ZH(2)
      ZHAHB=RH(1)*ZH(2)+RH(2)*ZH(1)
C
      RU=1.D0-RH(1)
      ZU=-ZH(1)
      RUP=PA(1)-RH(1)
      ZUP=-ZH(1)
C
      RQ=PAB1H*(1.+RHAHB)-RH(1)-PAB*RH(2)
      ZQ=PAB1H*ZHAHB-ZH(1)-PAB*ZH(2)
C
      RW=1.D0-RHAHB
      ZW=-ZHAHB
      RWP=PAB-RHAHB
      ZWP=-ZHAHB
C
      if(hahb.le.0.)then
        print*,' entree xinter avec v,tet2',v,tet2
        print*,' am',am,' hahb',hahb
        end if
      HAHBNL=XFOIL*LOG(HAHB)
      HAHBN=0.
      IF(HAHBNL.GT.-100.)HAHBN=EXP(HAHBNL)
      AHAHBN=AHAHB*XFOIL
C
      AHAHBN=MOD(AHAHBN,dtwOPI)
      RVP=PABN-HAHBN*COS(AHAHBN)
      ZVP=-HAHBN*SIN(AHAHBN)
C
      WPSQ=RWP**2+ZWP**2
      WSQ=RW**2+ZW**2
C
      C1=RU*RUP-ZU*ZUP
      C2=RW*RWP-ZW*ZWP
      S1=ZU*RUP-ZUP*RU
      S2=ZW*RWP-ZWP*RW
      C3=RH(2)*RVP-ZH(2)*ZVP
      S3=ZH(2)*RVP+RH(2)*ZVP
C
      F1=ANE*(RQ*RW+ZQ*ZW)
      F2=(C3*(C1*C2+S1*S2)-S3*(S1*C2-C1*S2))/WPSQ
C
      XINTER=2.D0*(F1+F2)/WSQ
C
      RETURN
      END

