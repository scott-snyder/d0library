      SUBROUTINE PYTBDY(XM)
      REAL*8 XM(5)
*
*     generates 3-body decays of gaugions
*
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)
      SAVE /LUJETS/,/LUDAT1/,/LUDAT2/,/LUDAT3/
      SAVE /PYSUBS/,/PYPARS/,/PYINT1/,/PYINT2/,/PYINT4/      
C
      REAL*8 S12_MIN,S12_MAX,YJACO1,S23_AVE,S23_DF1,S23_DF2
      REAL*8 E1,e2,e3,p1,p2,p3,cthe1,sthe1,cthe3,sthe3,cphi1,sphi1
      REAL*8 s23_DEL,EPS
      DATA EPS/1.D-6/
      REAL*8 golden,ax,bx,cx,tol,xmin,R,C
      PARAMETER (R=.61803399,C=1.-R,TOL=1.D-3)
      REAL*8 f1,f2,x0,x1,x2,x3
C
C GENERATE S12
       S12_MIN=(XM(1)+XM(2))**2
       S12_MAX=(XM(5)-XM(3))**2
       YJACO1=S12_MAX-S12_MIN
C FIND S12*
C      FUNCTION golden(ax,bx,cx,f,tol,xmin)
       AX=S12_MIN
       CX=S12_MAX
       BX=S12_MIN+.5*YJACO1
       x0=ax
       x3=cx
       if(abs(cx-bx).gt.abs(bx-ax))then
        x1=bx
        x2=bx+C*(cx-bx)
       else
        x2=bx
        x1=bx-C*(bx-ax)
       endif
C......Solve for F1 and F2
       S23_DF1=(X1-XM(2)**2-XM(1)**2)**2
     $ -(2.0*XM(1)*XM(2))**2
       S23_DF2=(X1-XM(3)**2-XM(5)**2)**2
     $ -(2.0*XM(3)*XM(5))**2
       s23_df1=s23_df1*eps
       s23_df2=s23_df2*eps
       S23_DEL=SQRT(S23_DF1*S23_DF2)/(2.0*X1)
       F1=-2.*S23_DEL/eps
       S23_DF1=(X2-XM(2)**2-XM(1)**2)**2
     $ -(2.0*XM(1)*XM(2))**2
       S23_DF2=(X2-XM(3)**2-XM(5)**2)**2
     $ -(2.0*XM(3)*XM(5))**2
       s23_df1=s23_df1*eps
       s23_df2=s23_df2*eps
       S23_DEL=SQRT(S23_DF1*S23_DF2)/(2.0*X2)
       F2=-2.*S23_DEL/eps
 1     if(abs(x3-x0).gt.tol*(abs(x1)+abs(x2)))then
        if(f2.lt.f1)then
         x0=x1
         x1=x2
         x2=R*x1+C*x3
         f1=f2
         S23_DF1=(X2-XM(2)**2-XM(1)**2)**2
     $      -(2.0*XM(1)*XM(2))**2
         S23_DF2=(X2-XM(3)**2-XM(5)**2)**2
     $      -(2.0*XM(3)*XM(5))**2
         s23_df1=s23_df1*eps
         s23_df2=s23_df2*eps
         S23_DEL=SQRT(S23_DF1*S23_DF2)/(2.0*X2)
         F2=-2.*S23_DEL/eps
        else
         x3=x2
         x2=x1
         x1=R*x2+C*x0
         f2=f1
         S23_DF1=(X1-XM(2)**2-XM(1)**2)**2
     $      -(2.0*XM(1)*XM(2))**2
         S23_DF2=(X1-XM(3)**2-XM(5)**2)**2
     $      -(2.0*XM(3)*XM(5))**2
         s23_df1=s23_df1*eps
         s23_df2=s23_df2*eps
         S23_DEL=SQRT(S23_DF1*S23_DF2)/(2.0*X1)
         F1=-2.*S23_DEL/eps
        endif
        goto 1
       endif
C......We want the maximum, not the minimum
       if(f1.lt.f2)then
        golden=-f1
        xmin=x1
       else
        golden=-f2
        xmin=x2
       endif
C......
       IKNT=0
 2     S12=S12_MIN+RLU(0)*YJACO1
       IKNT=IKNT+1
C GENERATE S23
       S23_AVE=XM(2)**2+XM(3)**2-(S12+XM(2)**2-XM(1)**2)
     >    *(S12+XM(3)**2-XM(5)**2)/(2.0*S12)
       S23_DF1=(S12-XM(2)**2-XM(1)**2)**2
     $ -(2.0*XM(1)*XM(2))**2
       S23_DF2=(S12-XM(3)**2-XM(5)**2)**2
     $ -(2.0*XM(3)*XM(5))**2
       s23_df1=s23_df1*eps
       s23_df2=s23_df2*eps
       S23_DEL=SQRT(S23_DF1*S23_DF2)/(2.0*S12)
       S23_DEL=S23_DEL/eps
       S23_MIN=S23_AVE-S23_DEL
       S23_MAX=S23_AVE+S23_DEL
       YJACO2=S23_MAX-S23_MIN
       S23=S23_MIN+RLU(0)*YJACO2
C......Check the sampling
       IF(IKNT.GT.100) GOTO 3
       IF(YJACO2.LT.RLU(0)*GOLDEN) GOTO 2
 3     E3=(XM(5)**2+XM(3)**2-S12)/(2.0*XM(5))
       E1=(XM(5)**2+XM(1)**2-S23)/(2.0*XM(5))
       E2=XM(5)-E1-E3
       P1=SQRT(E1*E1-XM(1)**2)
       P2=SQRT(E2*E2-XM(2)**2)
       P3=SQRT(E3*E3-XM(3)**2)
       CTHE1=2.0*RLU(0)-1.0
       ANG1=2.*RLU(0)*PARU(1)
       CPHI1=COS(ANG1)
       SPHI1=SIN(ANG1)
       ARG=1.0-CTHE1**2
       IF(ARG.LT.0.0.AND.ARG.GT.-1.E-3) ARG=0.0
       STHE1=SQRT(ARG)
       P(N+1,1)=P1*STHE1*CPHI1
       P(N+1,2)=P1*STHE1*SPHI1
       P(N+1,3)=P1*CTHE1
       P(N+1,4)=E1
C GET CPHI3
       ANG3=2.*RLU(0)*PARU(1)
       CPHI3=COS(ANG3)
       SPHI3=SIN(ANG3)
       CTHE3=(P2**2-P1**2-P3**2)/2.0/P1/P3
       ARG=1.0-CTHE3**2
       IF(ARG.LT.0.D0.AND.ARG.GT.-1.E-3) ARG=0.0      
       STHE3=SQRT(ARG)
       P(N+3,1)=-P3*STHE3*CPHI3*CTHE1*CPHI1
     >    +P3*STHE3*SPHI3*SPHI1
     >    +P3*CTHE3*STHE1*CPHI1
       P(N+3,2)=-P3*STHE3*CPHI3*CTHE1*SPHI1
     >    -P3*STHE3*SPHI3*CPHI1
     >    +P3*CTHE3*STHE1*SPHI1
       P(N+3,3)=P3*STHE3*CPHI3*STHE1
     >    +P3*CTHE3*CTHE1
       P(N+3,4)=E3
       DO I=1,3
        P(N+2,I)=-P(N+1,I)-P(N+3,I)
       ENDDO
       P(N+2,4)=E2

       RETURN
       END
