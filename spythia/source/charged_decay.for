      SUBROUTINE charged_decay(KCIN,XLAM,IDLAM,IKNT)
      implicit none
C.....LUND VARIABLES
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:SUSYPAR.INC'
C.....Local variables
      integer KCIN
      REAL XMI,XMJ,XMF,XMSF1,XMSF2,XMW,XMW2,XMZ,XMZ2,AXMJ,AXMI
      REAL XMFP,XMF1,XMF2,xmsl,xmg
      real s12min,s12max
      REAL xmi2,xmi3,xmj2,XMH,XMH2,XMHP,XMHP2,XMA2,XMB2,xmk
      REAL LAMFUN,XL
      REAL TANW,XW,AEM,C1,AS,EI,T3
      REAL X2XH,X2XG
      REAL XLAM(0:100)
      INTEGER IDLAM(100,3)
      INTEGER LKNT,IX,IH,J,IJ,I,IKNT,FID
      INTEGER ITH(3)
      DATA ITH/72,73,74/
      INTEGER ITHC
      data ITHC/75/
      REAL ETAH(3),CH(3),DH(3),EH(3)
      DATA ETAH/1.,1.,-1./
      REAL SR2
      DATA SR2/1.4142136/
      REAL CBETA,SBETA,GR,GL,F12k,F21k

      REAL ULALEM,PI,ULALPS
      DATA PI/3.141592654/
      REAL AL,BL,AR,BR,ALP,BLP,ARP,BRP

      REAL XXM(20)
      COMMON/PARINT/XXM
      REAL GAUSS
      external gauss,x2xz5,x2xw5,x2xz2
      REAL PREC
      DATA PREC/1.E-2/

C.....count the number of decay modes
      LKNT=0
      XMW=PMAS(24,1)
      XMW2=XMW**2
      XMZ=PMAS(23,1)
      XMZ2=XMZ**2
      XW=1.-XMW2/XMZ2
      TANW = SQRT(XW/(1.-XW))

C.....1 or 2 depending on 70 or 71
      IX=KCIN-69
C
C.....70,71
C
      XMI=smw(ix)
      XMI2=XMI**2
      AXMI=ABS(XMI)
      AEM=ULALEM(XMI2)
      AS =ULALPS(XMI2)
      C1=AEM/XW
      XMI3=abs(XMI**3)

      SBETA=sin(beta)
      CBETA=cos(beta)

C.....CHECK ALL 2-BODY DECAYS TO GAUGE and HIGGS bosons
      IF(IX.EQ.1) GOTO 1000
      XMJ=smw(1)
      AXMJ=ABS(XMJ)
      XMJ2=XMJ**2
C.....chi_2+ -> chi_1+ + Z0
      IF(aXMI.GE.AXMJ+XMZ) THEN
       LKNT=LKNT+1
       GL=v(2,1)*v(1,1)+.5*v(2,2)*v(1,2)
       GR=u(2,1)*u(1,1)+.5*u(2,2)*u(1,2)
       XLAM(LKNT)=x2xg(c1/XMW2,xmi,xmj,xmz,gl,gr)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=23
       IDLAM(LKNT,3)=0
      ELSEIF(aXMI.GE.AXMJ) THEN
       xxm(5)=-(v(2,1)*v(1,1)+.5*v(2,2)*v(1,2))
       xxm(6)=-(u(2,1)*u(1,1)+.5*u(2,2)*u(1,2))
       XXM(9)=XMZ
       XXM(10)=PMAS(23,2)
       XXM(1)=0.0
       XXM(2)=XMJ
       XXM(3)=0.0
       XXM(4)=XMI
       s12min=0.0
       s12max=(axmj-axmi)**2
C......charged leptons
       xxm(7)= (-.5+XW)/(1.-XW)
       xxm(8)= XW/(1.-XW)
       XXM(11)=pmas(55,1)
       XXM(12)=v(2,1)*v(1,1)
       if( xxm(11).lt.axmi ) goto 50
       LKNT=LKNT+1
       XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $    GAUSS(X2XZ2,S12MIN,S12MAX,PREC)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=11
       IDLAM(LKNT,3)=-11
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=13
       IDLAM(LKNT,3)=-13
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=15
       IDLAM(LKNT,3)=-15
C......Neutrinos
 50    continue
       xxm(7)= (.5)/(1.-XW)
       xxm(8)= 0.0
       XXM(11)=pmas(53,1)
       XXM(12)=u(2,1)*u(1,1)
       if( xxm(11).lt.axmi ) goto 51
       LKNT=LKNT+1
       XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $    GAUSS(X2XZ2,S12MIN,S12MAX,PREC)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=12
       IDLAM(LKNT,3)=-12
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=14
       IDLAM(LKNT,3)=-14
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=16
       IDLAM(LKNT,3)=-16
C......d-type quarks
 51    continue
       xxm(7)= (-.5+XW/3.)/(1.-XW)
       xxm(8)= XW/3./(1.-XW)
       XXM(11)=pmas(43,1)
       XXM(12)=v(2,1)*v(1,1)
       if( xxm(11).lt.axmi ) goto 52
       LKNT=LKNT+1
       XLAM(LKNT)=3.*c1**2/XMI3/(16.*PI)*
     $    GAUSS(X2XZ2,S12MIN,S12MAX,PREC)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=1
       IDLAM(LKNT,3)=-1
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=3
       IDLAM(LKNT,3)=-3
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=5
       IDLAM(LKNT,3)=-5
C......u-type quarks
 52    continue
       xxm(7)= (.5-2.*XW/3.)/(1.-XW)
       xxm(8)= -2.*XW/3./(1.-XW)
       XXM(11)=pmas(41,1)
       XXM(12)=u(2,1)*u(1,1)
       if( xxm(11).lt.axmi ) goto 53
       LKNT=LKNT+1
       XLAM(LKNT)=3.*c1**2/XMI3/(16.*PI)*
     $    GAUSS(X2XZ2,S12MIN,S12MAX,PREC)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=2
       IDLAM(LKNT,3)=-2
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=70
       IDLAM(LKNT,2)=4
       IDLAM(LKNT,3)=-4
 53    continue
      ENDIF
C.....chi_2+ -> chi_1+ + H0_k
      EH(1)=cos(alfa)
      EH(2)=sin(alfa)
      EH(3)=-SBETA
      DH(1)=-sin(alfa)
      DH(2)=cos(alfa)
      DH(3)=cos(beta)
      DO IH=1,3
       XMH=pmas(ith(ih),1)
       XMH2=XMH**2
C......No 3-body option
       IF(aXMI.GE.AXMJ+XMH) THEN
        LKNT=LKNT+1
        XL=LAMFUN(XMI2,XMJ2,XMH2)
        F21K=(v(2,1)*u(1,2)*eh(ih) - v(2,2)*u(1,1)*dh(ih))/SR2
        F12K=(v(1,1)*u(2,2)*eh(ih) - v(1,2)*u(2,1)*dh(ih))/SR2
        XMK=XMJ*ETAH(IH)
        XLAM(LKNT)=X2XH(c1,XMI,XMK,XMH,F12K,F21K)
        IDLAM(LKNT,1)=70
        IDLAM(LKNT,2)=ITH(IH)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
C.....CHI1 JUMPS to HERE
 1000 CONTINUE
C.....chi+_i -> chi0_j + W+
      DO IJ=1,4
       XMJ=smz(ij)
       AXMJ=ABS(XMJ)
       XMJ2=XMJ**2
       IF(aXMI.GE.AXMJ+XMW) THEN
        LKNT=LKNT+1
        GL=z(ij,2)*v(ix,1)-z(ij,4)*v(ix,2)/SR2
        GR=z(ij,2)*u(ix,1)+z(ij,3)*u(ix,2)/SR2
        XLAM(LKNT)=x2xg(c1/XMW2,xmi,xmj,xmw,gl,gr)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=24
        IDLAM(LKNT,3)=0
       ELSEIF(aXMI.GE.AXMJ) THEN
        XMF1=0.0
        XMF2=0.0
        S12MIN=(XMF1+XMF2)**2
        S12MAX=(axmj-axmi)**2
        XXM(5)=-1./SR2*Z(ij,4)*V(ix,2)+Z(ij,2)*V(ix,1)
        XXM(6)= 1./SR2*Z(ij,3)*U(ix,2)+Z(ij,2)*U(ix,1)
C.......leptons
        FID=11
        EI=KCHG(FID,1)/3.
        T3=-.5
        xxm(7)=-SR2*(T3*z(ij,2)-TANW*(T3-EI)*z(ij,1))*U(ix,1)
        FID=12
        EI=KCHG(FID,1)/3.
        T3=.5
        xxm(8)=-SR2*(T3*z(ij,2)-TANW*(T3-EI)*z(ij,1))*V(ix,1)

        XXM(4)=XMI
        XXM(1)=XMF1
        XXM(2)=XMJ
        XXM(3)=XMF2
        XXM(9)=PMAS(24,1)
        XXM(10)=PMAS(24,2)
        XXM(11)=PMAS(53,1)
        XXM(12)=PMAS(55,1)
C
C 1/(2pi)**3*/(32*M**3)*g^4, g^2/(4*pi)= aem/xw, --> 1/(16pi)/M**3*(aem/xw)**2
C
        if( xxm(11).lt.axmi .and. xxm(12).lt.axmi ) goto 61
        if(xxm(11).lt.axmi) then
         xxm(11)=1.E6
        elseif(xxm(12).lt.axmi) then
         xxm(12)=1.E6
        endif
        lknt=lknt+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $   GAUSS(X2XW5,S12MIN,S12MAX,PREC)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=-11
        IDLAM(LKNT,3)=12
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=-13
        IDLAM(LKNT,3)=14
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=-15
        IDLAM(LKNT,3)=16
C.......Now, do the quarks
 61     continue
        FID=1
        EI=KCHG(FID,1)/3.
        T3=-.5
        xxm(7)=-SR2*(T3*z(ij,2)-TANW*(T3-EI)*z(ij,1))*U(ix,1)
        FID=1
        EI=KCHG(FID,1)/3.
        T3=.5
        xxm(8)=-SR2*(T3*z(ij,2)-TANW*(T3-EI)*z(ij,1))*V(ix,1)

        XXM(11)=PMAS(41,1)
        XXM(12)=PMAS(43,1)
        if( xxm(11).lt.axmi .and. xxm(12).lt.axmi ) goto 62
        if(xxm(11).lt.axmi) then
         xxm(11)=1.E6
        elseif(xxm(12).lt.axmi) then
         xxm(12)=1.E6
        endif
        lknt=lknt+1
        XLAM(LKNT)=3.*c1**2/XMI3/(16.*PI)*
     $   GAUSS(X2XW5,S12MIN,S12MAX,PREC)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=-1
        IDLAM(LKNT,3)=2
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=-3
        IDLAM(LKNT,3)=4
 62     continue
       ENDIF
      ENDDO
C.....chi+_i -> chi0_j + H+
      DO IJ=1,4
       XMJ=smz(ij)
       AXMJ=ABS(XMJ)
       XMJ2=XMJ**2
       XMHP=pmas(ithc,1)
       XMHP2=XMHP**2
       IF(aXMI.GE.AXMJ+XMHP) THEN
        LKNT=LKNT+1
        GL=cbeta*(z(ij,4)*v(ix,1)+(z(ij,2)+z(ij,1)*TANW)*v(ix,2)/SR2)
        GR=sbeta*(z(ij,3)*u(ix,1)-(z(ij,2)+z(ij,1)*TANW)*u(ix,2)/SR2)
        XLAM(LKNT)=x2xh(c1,xmi,xmj,xmhp,gl,gr)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=ITHC
        IDLAM(LKNT,3)=0
       ELSE

       ENDIF
      ENDDO
C.....2-BODY DECAYS to fermion sfermion
      DO J=1,5,2
       XMF=PMAS(J,1)
       XMFP=PMAS(J+1,1)
       AL=v(ix,1)
       BL=-XMF*u(ix,2)/SR2/XMW/CBETA
       AR=-XMFP*v(ix,2)/SR2/XMW/SBETA
       BR=0.0
C.......For stop, the mass eigenstates are different
C.......For most cases, qL = q1, qR = q2
       IF(J.EQ.5) THEN
        ALP=SINST*AR + COSST*AL
        BLP=SINST*BR + COSST*BL
        ARP=-SINST*AL + COSST*AR
        BRP=-SINST*BL + COSST*BR
        AL=ALP
        BL=BLP
        AR=ARP
        BR=BRP
       ENDIF
C......d~ u_1
       XMSF1=PMAS(41+2*J,1)
       IF(aXMI.GE.XMF+XMSF1) THEN
        LKNT=LKNT+1
        XMA2=XMSF1**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=3.*c1/8./XMI3*SQRT(XL)*((XMI2+XMB2-XMA2)*
     $   (AL**2+BL**2)+4.*BL*AL*XMI*XMF)
        IDLAM(LKNT,1)=41+2*J
        IDLAM(LKNT,2)=-J
        IDLAM(LKNT,3)=0
       ENDIF
C......d~ u_2
       XMSF2=PMAS(42+2*J,1)
       IF(aXMI.GE.XMF+XMSF2) THEN
        LKNT=LKNT+1
        XMA2=XMSF1**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=3.*c1/8./XMI3*SQRT(XL)*((XMI2+XMB2-XMA2)*
     $  (AR**2+BR**2)+4.*BR*AR*XMI*XMF)
        IDLAM(LKNT,1)=(42+2*J)
        IDLAM(LKNT,2)=-J
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
      DO J=2,6,2
       XMF=PMAS(J,1)
       XMFP=PMAS(J-1,1)
       AL=u(ix,1)
       BL=-XMF*v(ix,2)/SR2/XMW/SBETA
       AR=-XMFP*u(ix,2)/SR2/XMW/CBETA
       BR=0.0
C......u d_L~
       XMSF1=PMAS(41+2*(J-2),1)
       IF(aXMI.GE.XMF+XMSF1) THEN
        LKNT=LKNT+1
        XMA2=XMSF1**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=3.*c1/8./XMI3*SQRT(XL)*((XMI2+XMB2-XMA2)*
     $   (AL**2+BL**2)+4.*BL*AL*XMI*XMF)
        IDLAM(LKNT,1)=-(41+2*(J-2))
        IDLAM(LKNT,2)=J
        IDLAM(LKNT,3)=0
       ENDIF
C......u d_R~
       XMSF2=PMAS(42+2*(J-2),1)
       IF(aXMI.GE.XMF+XMSF2) THEN
        LKNT=LKNT+1
        XMA2=XMSF2**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=3.*c1/8./XMI3*SQRT(XL)*((XMI2+XMB2-XMA2)*
     $  (AR**2+BR**2)+4.*BR*AR*XMI*XMF)
        IDLAM(LKNT,1)=-(42+2*(J-2))
        IDLAM(LKNT,2)=J
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
      DO J=11,15,2
       XMF=PMAS(J,1)
C......e+ nu_L
       XMSF1=PMAS(55+2*(J-11),1)
       IF(aXMI.GE.XMF+XMSF1) THEN
        LKNT=LKNT+1
        XMA2=XMSF1**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=c1/8./XMI3*SQRT(XL)*((XMI2+XMB2-XMA2)*
     $   (v(ix,1)**2+.5*xmb2*u(ix,2)**2/XMW2/cbeta**2)-
     $   2.*SR2*XMB2*v(ix,1)*u(ix,2)*XMI/XMW/cbeta)
        IDLAM(LKNT,1)=55+2*(J-11)
        IDLAM(LKNT,2)=-J
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
      DO J=12,16,2
       XMF=PMAS(J,1)
C......nu e_L~
       XMSF1=PMAS(53+2*(J-12),1)
       IF(aXMI.GE.XMF+XMSF1) THEN
        LKNT=LKNT+1
        XMA2=XMSF1**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=c1/8./XMI3*SQRT(XL)*((XMI2+XMB2-XMA2)*
     $   (u(ix,1)**2+.5*xmb2*v(ix,2)**2/XMW2/sbeta**2)-
     $   2.*SR2*XMB2*u(ix,1)*v(ix,2)*XMI/XMW/sbeta)
        IDLAM(LKNT,1)=-(53+2*(J-12))
        IDLAM(LKNT,2)=J
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO

C.....3-BODY DECAY TO q q~' gluino, only if it cannot proceed through
C........a 2-body -- 2-body chain
      IF(AXMI.GE.PMAS(65,1)) THEN
       XMJ=pmas(65,1)
       axmj=abs(xmj)
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
       if( xxm(11).lt.axmi .or. xxm(12).lt.axmi ) goto 6100
       LKNT=LKNT+1
       XLAM(LKNT)=4.*c1*as/XMI3/(16.*PI)*
     $    GAUSS(X2XW5,S12MIN,S12MAX,PREC)
       IDLAM(LKNT,1)=65
       IDLAM(LKNT,2)=-1
       IDLAM(LKNT,3)=2
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=65
       IDLAM(LKNT,2)=-3
       IDLAM(LKNT,3)=4
 6100  CONTINUE
      ENDIF
C
      IKNT=LKNT
      XLAM(0)=0.0
      DO I=1,IKNT
       XLAM(0)=XLAM(0)+XLAM(I)
       if(XLAM(I).LT.0.0) THEN
        print*,' XLAM(I) = ',XLAM(I),KCIN,(IDLAM(I,J),J=1,3)
        XLAM(I)=0.0
       ENDIF
      ENDDO
      IF(XLAM(0).eq.0.0) THEN
       XLAM(0)=1.E-6
       print*,' xlam(0) = ',xlam(0)
       print*,lknt
       print*,(xlam(j),j=1,lknt)
      endif
C
      RETURN
      END
C
      FUNCTION X2XG(C1,xm1,xm2,xm3,GL,GR)
C.....calculate the decay rate for ino -> ino + gauge boson
      implicit none
      real x2xg,xm1,xm2,xm3,gl,gr
      real xl,lamfun,c1
      real xmi2,xmj2,xmv2,xmi3
C
      xmi2=xm1**2
      xmi3=abs(xm1**3)
      xmj2=xm2**2
      xmv2=xm3**2
      XL=LAMFUN(XMI2,XMJ2,XMv2)
      x2xG=c1/8./XMI3*SQRT(XL)
     $   *((GL**2+GR**2)*(XL+3.*XMV2*(XMI2+XMJ2-XMV2))-
     $   12.*GL*GR*XM1*XM2*XMV2)
      RETURN
      END
C
      FUNCTION X2XH(c1,xm1,xm2,xm3,GL,GR)
C.....calculate the decay rate for ino -> ino + H
      implicit none
      real x2xh,xm1,xm2,xm3,gl,gr
      real xl,lamfun,c1
      real xmi2,xmj2,xmv2,xmi3
C
      xmi2=xm1**2
      xmi3=abs(xm1**3)
      xmj2=xm2**2
      xmv2=xm3**2
      XL=LAMFUN(XMI2,XMJ2,XMv2)
      x2xH=c1/8./XMI3*SQRT(XL)
     $   *((GL**2+GR**2)*(XMI2+XMJ2-XMV2)+
     $   4.*GL*GR*XM1*XM2)
      RETURN
      END
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
      FUNCTION LAMFUN(X,Y,Z)
      IMPLICIT NONE
      real lamfun,x,y,z
      LAMFUN=(X+Y-Z)**2-4.*X*Y
      IF(LAMFUN.LT.0.) LAMFUN=0.0
      RETURN
      END
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      FUNCTION X2XW5(X)
      IMPLICIT NONE
C
C  chi0(+) -> chi+(0) f f~'
C
      REAL X2XW5,X
      real*8 xm12,xm22,xm32,s,s23,s13,s12,wprop2
      real*8 ww,wu,wd,wwu,wwd,wud
      real*8 sr2,OL,OR,FLD,FLU,XMV,XMG,XMSD,XMSU
      real*8 sij
      real*8 s23min,s23max,s23ave,s23del
      real*8 tint,tprop
      real*8 tint2,tint3,utint

      data sr2/1.4142136/
      REAL XXM(20)
      COMMON/PARINT/XXM
C
      XM12=xxm(1)**2
      XM22=xxm(2)**2
      XM32=xxm(3)**2
      S=XXM(4)**2
      S13=X
      IF(xxm(1).eq.0.and.xxm(3).eq.0.) THEN
       S23AVE=.5*(xm22+s-s13)
       S23DEL=.5*SQRT( (x-xm22-s)**2-4.*xm22*s )
      ELSE
       S23AVE=xm22+xm32-.5/X*(X+xm32-xm12)*(X+xm22-s)
       S23DEL=.5/X*sqrt( ( (x-xm12-xm32)**2-4.*xm12*xm32)*
     $                  ( (x-xm22-s)**2  -4.*xm22*s  ) )
      ENDIF
      S23MIN=(s23ave-s23del)
      s23max=(s23ave+s23del)
      if(s23del.lt.1.e-3) THEN
       x2xw5=0.0
       return
      endif
      XMV=xxm(9)
      XMG=XXM(10)
      XMSD=XXM(11)**2
      XMSU=XXM(12)**2
      OL=XXM(5)
      OR=XXM(6)
      FLD=XXM(7)
      FLU=XXM(8)
C
      WPROP2=((S13-XMV**2)**2+(XMV*XMG)**2)
      SIJ=S13*xxm(2)*xxm(4)
      IF(XMV.LE.1000.) THEN
       WW=(OR**2+OL**2)*TINT(S23MAX,s23min,xm22,s)
     $    -2.*OL*OR*SIJ*(S23max-S23min)
       WW=WW/WPROP2
       IF(XXM(11).LE.10000.) THEN
        WWD=OL*SIJ*TPROP(S23MAX,s23min,xmsd)
     $     -OR*TINT2(s23max,s23min,xm22,s,xmsd)
        WWD=-WWD*SR2*FLD
        WWD=WWD*(S13-XMV**2)/WPROP2
       ELSE
        WWD=0.0
       ENDIF
       IF(XXM(12).LE.10000.) THEN
        WWU=OR*SIJ*TPROP(S23MAX,s23min,xmsu)
     $     -OL*TINT2(s23max,s23min,xm22,s,xmsu)
        WWU=WWU*SR2*FLU
        WWU=WWU*(S13-XMV**2)/WPROP2
       ELSE
        WWU=0.0
       ENDIF
      ELSE
       WW=0.0
       WWD=0.0
       WWU=0.0
      ENDIF
      IF(XXM(12).LE.10000.) THEN
       WU=.5*FLU**2*TINT3(s23max,s23min,xm22,s,xmsu)
      ELSE
       WU=0.0
      ENDIF
      IF(XXM(11).LE.10000.) THEN
       WD=.5*FLD**2*TINT3(s23max,s23min,xm22,s,xmsd)
      ELSE
       WD=0.0
      ENDIF
      IF(XXM(11).le.10000..and.xxm(12).le.10000.) THEN
       WUD=FLU*FLD*SIJ*UTINT(S23max,s23min,xmsd,xm22+s-s13-xmsu)
      ELSE
       WUD=0.0
      ENDIF

      X2XW5=WW+WU+WD+WWU+WWD+WUD

      IF(X2XW5.lt.0.0) THEN
       print*,' negative wt in X2XW5 '
       print*,ww,wu,wd
       print*,wwd,wwu,wud
       print*,sqrt(s13)
       print*,tint(s23max,s23min,xm22,s)
       x2xw5=0.0
      endif
      RETURN
      END
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      FUNCTION X2XZ2(X)
      IMPLICIT NONE
C
C     chi+ -> chi+ + f f~
C
      REAL X2XZ2,X
      real*8 xm12,xm22,xm32,s,s23,s13,s12,wprop2
      real*8 ww,wu,wd,wwu,wwd,wud
      real*8 sr2,OL,OR,FLD,FLU,XMV,XMG,XMSL
      real*8 sij
      real*8 LE,RE,LE2,RE2,OL2,OR2,CT
      data sr2/1.4142136/
      real*8 s23min,s23max,s23ave,s23del
      real*8 tint,tprop
      real*8 tint2,tint3,utint
      integer I
      REAL XXM(20)
      COMMON/PARINT/XXM
C
      XM12=xxm(1)**2
      XM22=xxm(2)**2
      XM32=xxm(3)**2
      S=XXM(4)**2
      S13=X
      IF(xxm(1).eq.0.and.xxm(3).eq.0.) THEN
       S23AVE=.5*(xm22+s-s13)
       S23DEL=.5*SQRT( (x-xm22-s)**2-4.*xm22*s )
      ELSE
       S23AVE=xm22+xm32-.5/X*(X+xm32-xm12)*(X+xm22-s)
       S23DEL=.5/X*sqrt( ( (x-xm12-xm32)**2-4.*xm12*xm32)*
     $                  ( (x-xm22-s)**2  -4.*xm22*s  ) )
      ENDIF
      S23MIN=(s23ave-s23del)
      s23max=(s23ave+s23del)
      if(s23del.lt.1.e-3) THEN
       x2xz2=0.0
       return
      endif
C.....
      XMV=xxm(9)
      XMG=XXM(10)
      XMSL=XXM(11)**2
      OL=XXM(5)
      OR=XXM(6)
      OL2=OL**2
      OR2=OR**2
      LE=XXM(7)
      RE=XXM(8)
      LE2=LE**2
      RE2=RE**2
      CT=XXM(12)
C
      WPROP2=(S13-XMV**2)**2+(XMV*XMG)**2
      SIJ=xxm(2)*xxm(4)*S13
      WW=(LE2+RE2)*(OR2+OL2)*2.*TINT(s23max,s23min,xm22,s)
     $ - 4.*(LE2+RE2)*OL*OR*SIJ*(s23max-s23min)
      WW=WW/WPROP2
      WD=.5*CT**2*TINT3(s23max,s23min,xm22,s,XMSL)
      WWD=OL*TINT2(s23max,s23min,xm22,s,xmsl)-
     $ OR*SIJ*TPROP(S23max,s23min,xmsl)
      WWD=2.*WWD*LE*CT*(S13-XMV**2)/WPROP2

      X2XZ2=(WW+WD+WWD)
      IF(X2Xz2.lt.0.0) THEN
       print*,' negative wt in X2Xz2 '
       print*,ww,wd,wwd
       print*,s23min,s23max
       print*,(xxm(I),I=1,4)
       print*,(xxm(I),I=5,8)
       print*,(xxm(I),I=9,12)
       x2xz2=0.0
      endif
      RETURN
      END










