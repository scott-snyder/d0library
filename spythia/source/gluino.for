      SUBROUTINE gluino(KCIN,XLAM,IDLAM,IKNT)
      implicit none
C.....LUND VARIABLES
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:SUSYPAR.INC'
C.....Local variables
      integer KCIN
      REAL XMI,XMJ,XMF,XMSF1,XMSF2,XMW,XMW2,XMZ,XMZ2,AXMJ,AXMI
      REAL xmi2,xmi3,xmj2,XMA2,XMB2,XMFP
      REAL LAMFUN,XL
      REAL TANW,XW,AEM,C1,AS,s12max,s12min
      REAL CA,CB,AL,AR,BL,BR
      REAL XLAM(0:100)
      INTEGER IDLAM(100,3)
      INTEGER LKNT,IX,IC,ILR,IDU,J,IJ,I,IKNT,IFL
      REAL SR2
      DATA SR2/1.4142136/

      REAL ULALEM,PI,ULALPS,EI
      DATA PI/3.141592654/

      REAL XXM(20)
      COMMON/PARINT/XXM
      REAL GAUSS
      external gauss,x2xz5,x2xw5,x2xz2
      REAL PREC
      DATA PREC/1.E-2/
c--------------------------
C.....count the number of decay modes
      LKNT=0
      IF(KCIN.NE.65) RETURN

      XMW=PMAS(24,1)
      XMW2=XMW**2
      XMZ=PMAS(23,1)
      XMZ2=XMZ**2
      XW=1.-XMW2/XMZ2
      TANW = SQRT(XW/(1.-XW))
C
      XMI=pmas(KCIN,1)
      AXMI=ABS(XMI)
      XMI2=XMI**2
      AEM=ULALEM(XMI2)
      AS =ULALPS(XMI2)
      C1=AEM/XW
      XMI3=XMI**3

C
C.....2-BODY DECAYS of gluino -> quark squark
C
C.....IC = 1,12 dL...t2
      DO IC=1,12
       XMFP=0.0
       XMF=0.0
       IF(IC.ge.11.and.IC.LE.12) XMF=PMAS(6,1)
       XMJ=pmas(40+IC,1)
       AXMJ=ABS(XMJ)
       ILR=3-(1+MOD(IC,2))
       IFL=1+(IC-1)/2
       IDU=3-(1+MOD(IFL,2))
       IF(XMI.GE.AXMJ+XMF) THEN
        AL=1.0
        BL=0.0
        AR=1.0
        BR=0.0
        IF(IC.GE.11.and.IC.LE.12) THEN
         AL=COSST
         BL=-SINST
         AR=SINST
         BR=COSST
        ENDIF
C........f1 -> f chi
        IF(ILR.EQ.1) THEN
         CA=AL
         CB=BL
C........f2 -> f chi
        ELSE
         CA=AR
         CB=BR
        ENDIF
        LKNT=LKNT+1
        XMA2=XMJ**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=4./8.*AS/4./XMI3*SQRT(XL)*((XMI2+XMB2-XMA2)*
     $     (CA**2+CB**2)+4.*CA*CB*XMI*XMF)
        IDLAM(LKNT,1)=40+IC
        IDLAM(LKNT,2)=-IFL
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=-IDLAM(LKNT-1,1)
        IDLAM(LKNT,2)=-IDLAM(LKNT-1,2)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO

C.....3-body decays to gaugino fermion-fermion
      DO IX=1,4
       XMJ=smz(ix)
       axmj=abs(xmj)
       if(XMI.GE.AXMJ) THEN
        XXM(1)=0.0
        XXM(2)=XMJ
        XXM(3)=0.0
        XXM(4)=XMI
        xxm(5)=pmas(41,1)
        xxm(6)=pmas(42,1)
        xxm(7)=1.E6
        xxm(8)=0.0
        xxm(9)=0.0
        xxm(10)=0.0
        s12min=0.0
        s12max=(xmi-axmj)**2
C......all quarks but t
        xxm(11)=0.0
        xxm(12)=0.0
        xxm(13)=1.0
        xxm(14)=-SR2*(-.5*z(ix,2)+TANW*z(ix,1)/6.)
        xxm(15)=1.0
        xxm(16)=SR2*(-TANW*z(ix,1)/3.)
        IF( xxm(5).lt.axmi .or. xxm(6).lt.axmi ) goto 70
        LKNT=LKNT+1
        XLAM(LKNT)=.5*c1*as/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-2)
        IDLAM(LKNT,1)=65+IX
        IDLAM(LKNT,2)=1
        IDLAM(LKNT,3)=-1
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=65+IX
        IDLAM(LKNT,2)=3
        IDLAM(LKNT,3)=-3
 70     CONTINUE
        xxm(5)=pmas(49,1)
        xxm(6)=pmas(50,1)
        IF( xxm(5).lt.axmi .or. xxm(6).lt.axmi ) goto 700
        lknt=lknt+1
        XLAM(LKNT)=.5*c1*as/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-2)
        IDLAM(LKNT,1)=65+IX
        IDLAM(LKNT,2)=5
        IDLAM(LKNT,3)=-5
C......u-type quarks
 700    continue
        xxm(5)=pmas(43,1)
        xxm(6)=pmas(44,1)
        xxm(13)=1.0
        xxm(14)=-SR2*(.5*z(ix,2)+TANW*z(ix,1)/6.)
        xxm(15)=1.0
        xxm(16)=SR2*(2.*TANW*z(ix,1)/3.)
        IF( xxm(5).lt.axmi .or. xxm(6).lt.axmi ) goto 80
        LKNT=LKNT+1
        XLAM(LKNT)=.5*c1*as/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-2)
        IDLAM(LKNT,1)=65+IX
        IDLAM(LKNT,2)=2
        IDLAM(LKNT,3)=-2
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=65+IX
        IDLAM(LKNT,2)=4
        IDLAM(LKNT,3)=-4
 80     continue
       ENDIF
      ENDDO
C......Same for chargino decay
      DO IX=1,2
       XMJ=smw(ix)
       axmj=abs(xmj)
       IF(XMI.GE.AXMJ) THEN
        S12MIN=0.0
        S12MAX=(axmi-axmj)**2
        XXM(1)=0.0
        XXM(2)=XMJ
        XXM(3)=0.0
        XXM(4)=XMI
        XXM(5)=0.0
        XXM(6)=0.0
        XXM(9)=1.E6
        XXM(10)=0.0
        XXM(7)=U(ix,1)*SR2
        XXM(8)=V(ix,1)*SR2
        XXM(11)=PMAS(41,1)
        XXM(12)=PMAS(43,1)
        if( xxm(11).lt.axmi .or. xxm(12).lt.axmi ) goto 61
        LKNT=LKNT+1
        XLAM(LKNT)=.5*c1*as/XMI3/(16.*PI)*
     $     GAUSS(X2XW5,S12MIN,S12MAX,PREC)
        IDLAM(LKNT,1)=69+IX
        IDLAM(LKNT,2)=1
        IDLAM(LKNT,3)=-2
        LKNT=LKNT+1
        xlam(lknt)=xlam(lknt-1)
        idlam(lknt,1)=-idlam(lknt-1,1)
        idlam(lknt,2)=-idlam(lknt-1,2)
        idlam(lknt,3)=-idlam(lknt-1,3)
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=69+IX
        IDLAM(LKNT,2)=3
        IDLAM(LKNT,3)=-4
        lknt=lknt+1
        xlam(lknt)=xlam(lknt-1)
        idlam(lknt,1)=-idlam(lknt-1,1)
        idlam(lknt,2)=-idlam(lknt-1,2)
        idlam(lknt,3)=-idlam(lknt-1,3)
 61     CONTINUE
       ENDIF
      ENDDO
C
      IKNT=LKNT
      XLAM(0)=0.0
      DO I=1,IKNT
       XLAM(0)=XLAM(0)+XLAM(I)
       if(XLAM(I).LT.0.0) THEN
        print*,' XLAM(I) = ',XLAM(I),I,IDLAM(I,1),IDLAM(I,2)
       ENDIF
      ENDDO
      IF(XLAM(0).eq.0.0) XLAM(0)=1.E-6
C
      RETURN
      END







