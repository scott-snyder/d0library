      SUBROUTINE neutral_decay(KCIN,XLAM,IDLAM,IKNT)
      implicit none
C.....LUND VARIABLES
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:SUSYPAR.INC'
C.....Local variables
      integer KCIN
      REAL XMI,XMJ,XMF,XMSF1,XMSF2,XMW,XMW2,XMZ,XMZ2,AXMJ,AXMI
      REAL XMFP,XMF1,XMF2,xmsl,xmg,xmk
      real s12min,s12max
      REAL xmi2,xmi3,xmj2,XMH,XMH2,XMHP,XMHP2,XMA2,XMB2
      REAL LAMFUN,XL
      REAL TANW,XW,AEM,C1,AS,EI,T3
      REAL X2XH,X2XG
      REAL XLAM(0:100)
      INTEGER IDLAM(100,3)
      INTEGER LKNT,IX,IH,J,IJ,I,IKNT,FID
      INTEGER ITH(3)
      DATA ITH/72,73,74/
      integer ithc
      data ithc/75/
      REAL ETAH(3),CH(3),DH(3),EH(3)
      DATA ETAH/1.,1.,-1./
      REAL SR2
      DATA SR2/1.4142136/
      REAL CBETA,SBETA,GR,GL,F12k,F21k

      REAL ULALEM,PI,ULALPS
      DATA PI/3.141592654/
      REAL AL,BL,AR,BR,ALP,ARP,BLP,BRP

      REAL XXM(20)
      COMMON/PARINT/XXM
      REAL GAUSS
      external x2xw5,gauss,x2xz5
      REAL PREC
      DATA PREC/1.E-2/
c--------------------------
C.....count the number of decay modes
      LKNT=0

      XMW=PMAS(24,1)
      XMW2=XMW**2
      XMZ=PMAS(23,1)
      XMZ2=XMZ**2
      XW=1.-XMW2/XMZ2
      TANW = SQRT(XW/(1.-XW))

C.....1 - 4 depending on 66 - 69
      IX=KCIN-65

C.....66-69

      XMI=smz(ix)
      XMI2=XMI**2
      AXMI=ABS(XMI)
      AEM=ULALEM(XMI2)
      AS =ULALPS(XMI2)
      C1=AEM/XW
      XMI3=abs(XMI**3)

      SBETA=sin(beta)
      CBETA=cos(beta)

C.....CHECK ALL 2-BODY DECAYS TO GAUGE and HIGGS bosons
      IF(IX.EQ.1) THEN
       RETURN
      ENDIF
C.....chi0_i -> chi0_j + Z0
      DO IJ=1,IX-1
       XMJ=smz(ij)
       AXMJ=ABS(XMJ)
       XMJ2=XMJ**2
       IF(aXMI.GE.AXMJ+XMZ) THEN
        LKNT=LKNT+1
        GL=-.5*(z(ix,3)*z(ij,3)-z(ix,4)*z(ij,4))
        GR=-GL
        XLAM(LKNT)=x2xg(c1/XMW2,xmi,xmj,xmz,gl,gr)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=23
        IDLAM(LKNT,3)=0
       ELSEIF(aXMI.GE.AXMJ) THEN
        FID=11
        EI=KCHG(FID,1)/3.
        T3=-.5
        XXM(1)=0.0
        XXM(2)=XMJ
        XXM(3)=0.0
        XXM(4)=XMI
        xxm(5)=pmas(53,1)
        xxm(6)=pmas(54,1)
        xxm(7)=xmz
        xxm(8)=pmas(23,2)
        xxm(9)=-.5*(z(ix,3)*z(ij,3)-z(ix,4)*z(ij,4))
        xxm(10)=-xxm(9)
        xxm(11)=(T3-EI*XW)/(1.-XW)
        xxm(12)=-EI*XW/(1.-XW)
        xxm(13)=-SR2*(T3*z(ix,2)-TANW*(T3-EI)*z(ix,1))
        xxm(14)=-SR2*(T3*z(ij,2)-TANW*(T3-EI)*z(ij,1))
        xxm(15)=SR2*TANW*(EI*z(ix,1))
        xxm(16)=SR2*TANW*(EI*z(ij,1))
        s12min=0.0
        s12max=(axmi-axmj)**2
C......charged leptons
        IF( xxm(5).lt.axmi .and. xxm(6).lt.axmi ) goto 50
        if( xxm(5).lt.axmi ) then
         xxm(5)=1.E6
        elseif(xxm(6).lt.axmi ) then
         xxm(6)=1.E6
        endif
        LKNT=LKNT+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=11
        IDLAM(LKNT,3)=-11
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=13
        IDLAM(LKNT,3)=-13
 50     CONTINUE
        xxm(5)=pmas(61,1)
        xxm(6)=pmas(62,1)
        IF( xxm(5).lt.axmi .and. xxm(6).lt.axmi ) goto 500
        if( xxm(5).lt.axmi ) then
         xxm(5)=1.E6
        elseif(xxm(6).lt.axmi ) then
         xxm(6)=1.E6
        endif
        LKNT=LKNT+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=15
        IDLAM(LKNT,3)=-15
C......Neutrinos
 500    continue
        FID=12
        EI=KCHG(FID,1)/3.
        T3=.5
        xxm(5)=pmas(55,1)
        xxm(6)=1.E6
        xxm(11)=(T3-EI*XW)/(1.-XW)
        xxm(12)=-EI*XW/(1.-XW)
        xxm(13)=-SR2*(T3*z(ix,2)-TANW*(T3-EI)*z(ix,1))
        xxm(14)=-SR2*(T3*z(ij,2)-TANW*(T3-EI)*z(ij,1))
        xxm(15)=SR2*TANW*(EI*z(ix,1))
        xxm(16)=SR2*TANW*(EI*z(ij,1))

        IF( xxm(5).lt.axmi ) goto 60
        LKNT=LKNT+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)

        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=12
        IDLAM(LKNT,3)=-12
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=14
        IDLAM(LKNT,3)=-14
 60     CONTINUE
        xxm(5)=pmas(63,1)
        IF( xxm(5).lt.axmi ) goto 600
        LKNT=lknt+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=16
        IDLAM(LKNT,3)=-16
C......d-type quarks
 600    CONTINUE
        xxm(5)=pmas(41,1)
        xxm(6)=pmas(42,1)
        FID=1
        EI=KCHG(FID,1)/3.
        T3=-.5

        xxm(11)=(T3-EI*XW)/(1.-XW)
        xxm(12)=-EI*XW/(1.-XW)
        xxm(13)=-SR2*(T3*z(ix,2)-TANW*(T3-EI)*z(ix,1))
        xxm(14)=-SR2*(T3*z(ij,2)-TANW*(T3-EI)*z(ij,1))
        xxm(15)=SR2*TANW*(EI*z(ix,1))
        xxm(16)=SR2*TANW*(EI*z(ij,1))

        IF( xxm(5).lt.axmi .and. xxm(6).lt.axmi ) goto 70
        if( xxm(5).lt.axmi ) then
         xxm(5)=1.E6
        elseif( xxm(6).lt.axmi ) then
         xxm(6)=1.E6
        endif
        LKNT=LKNT+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)*3.
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=1
        IDLAM(LKNT,3)=-1
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=3
        IDLAM(LKNT,3)=-3
 70     CONTINUE
        xxm(5)=pmas(49,1)
        xxm(6)=pmas(50,1)
        IF( xxm(5).lt.axmi .and. xxm(6).lt.axmi ) goto 700
        if(xxm(5).lt.axmi) then
         xxm(5)=1.E6
        elseif(xxm(6).lt.axmi) then
         xxm(6)=1.E6
        endif
        lknt=lknt+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)*3.
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=5
        IDLAM(LKNT,3)=-5
C......u-type quarks
 700    continue
        xxm(5)=pmas(43,1)
        xxm(6)=pmas(44,1)
        FID=2
        EI=KCHG(FID,1)/3.
        T3=.5

        xxm(11)=(T3-EI*XW)/(1.-XW)
        xxm(12)=-EI*XW/(1.-XW)
        xxm(13)=-SR2*(T3*z(ix,2)-TANW*(T3-EI)*z(ix,1))
        xxm(14)=-SR2*(T3*z(ij,2)-TANW*(T3-EI)*z(ij,1))
        xxm(15)=SR2*TANW*(EI*z(ix,1))
        xxm(16)=SR2*TANW*(EI*z(ij,1))

        IF( xxm(5).lt.axmi .and. xxm(6).lt.axmi ) goto 80
        if(xxm(5).lt.axmi) then
         xxm(5)=1.e6
        elseif(xxm(6).lt.axmi) then
         xxm(6)=1.e6
        endif
        LKNT=LKNT+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $     GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)*3.
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=2
        IDLAM(LKNT,3)=-2
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=65+IJ
        IDLAM(LKNT,2)=4
        IDLAM(LKNT,3)=-4
 80     continue
       ENDIF
C.....chi0_i -> chi0_j + H0_k
       EH(1)=cos(alfa)
       EH(2)=sin(alfa)
       EH(3)=-SBETA
       DH(1)=-sin(alfa)
       DH(2)=cos(alfa)
       DH(3)=cos(beta)
       DO IH=1,3
        XMH=pmas(ith(ih),1)
        XMH2=XMH**2
        IF(aXMI.GE.AXMJ+XMH) THEN
         LKNT=LKNT+1
         XL=LAMFUN(XMI2,XMJ2,XMH2)
         F21K=.5*eh(ih)*(z(ix,3)*z(ij,2)+z(ij,3)*z(ix,2)-TANW*
     $    (z(ix,3)*z(ij,1)+z(ij,3)*z(ix,1)))+.5*dh(ih)*(
     $    z(ix,4)*z(ij,2)+z(ij,4)*z(ix,2)-TANW*
     $    (z(ix,4)*z(ij,1)+z(ij,4)*z(ix,1)))
         F12K=.5*eh(ih)*(z(ij,3)*z(ix,2)+z(ix,3)*z(ij,2)-TANW*
     $    (z(ij,3)*z(ix,1)+z(ix,3)*z(ij,1)))+.5*dh(ih)*(
     $    z(ij,4)*z(ix,2)+z(ix,4)*z(ij,2)-TANW*
     $    (z(ij,4)*z(ix,1)+z(ix,4)*z(ij,1)))
C......Sign of masses i,j
         XMK=XMJ*ETAH(IH)
         XLAM(LKNT)=X2XH(c1,XMI,XMK,XMH,F12K,F21K)
         IDLAM(LKNT,1)=65+IJ
         IDLAM(LKNT,2)=ITH(IH)
         IDLAM(LKNT,3)=0
        ENDIF
       ENDDO
      ENDDO
C.....chi0_i -> chi+_j + W-
      DO IJ=1,2
       XMJ=smw(ij)
       AXMJ=ABS(XMJ)
       XMJ2=XMJ**2
       IF(aXMI.GE.AXMJ+XMW) THEN
        LKNT=LKNT+1
        GL=z(ix,2)*v(ij,1)-z(ix,4)*v(ij,2)/SR2
        GR=z(ix,2)*u(ij,1)+z(ix,3)*u(ij,2)/SR2
        XLAM(LKNT)=x2xg(c1/XMW2,xmi,xmj,xmw,gl,gr)
        IDLAM(LKNT,1)=69+IJ
        IDLAM(LKNT,2)=-24
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        XLAM(LKNT)=xlam(lknt-1)
        IDLAM(LKNT,1)=-(69+IJ)
        IDLAM(LKNT,2)=24
        IDLAM(LKNT,3)=0
       ELSEIF(aXMI.GE.AXMJ) THEN
        S12MIN=0.0
        S12MAX=(axmi-axmj)**2
        XXM(5)=z(ix,2)*v(ij,1)-z(ix,4)*v(ij,2)/SR2
        XXM(6)=z(ix,2)*u(ij,1)+z(ix,3)*u(ij,2)/SR2
C.......leptons
        FID=11
        EI=KCHG(FID,1)/3.
        T3=-.5
        xxm(7)=-SR2*(T3*z(ix,2)-TANW*(T3-EI)*z(ix,1))*U(ij,1)
        FID=12
        EI=KCHG(FID,1)/3.
        T3=.5
        xxm(8)=-SR2*(T3*z(ix,2)-TANW*(T3-EI)*z(ix,1))*V(ij,1)

        XXM(1)=0.0
        XXM(2)=XMJ
        XXM(3)=0.0
        XXM(4)=XMI
        XXM(9)=PMAS(24,1)
        XXM(10)=PMAS(24,2)
        XXM(11)=PMAS(53,1)
        xxm(12)=PMAS(55,1)
        IF( xxm(11).lt.axmi .and. xxm(12).lt.axmi ) goto 51
        if(xxm(11).lt.axmi) then
         xxm(11)=1.E6
        elseif(xxm(12).lt.axmi) then
         xxm(12)=1.E6
        endif
        LKNT=LKNT+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $   GAUSS(X2XW5,S12MIN,S12MAX,PREC)
        IDLAM(LKNT,1)=69+IJ
        IDLAM(LKNT,2)=11
        IDLAM(LKNT,3)=-12
        lknt=lknt+1
        xlam(lknt)=xlam(lknt-1)
        idlam(lknt,1)=-idlam(lknt-1,1)
        idlam(lknt,2)=-idlam(lknt-1,2)
        idlam(lknt,3)=-idlam(lknt-1,3)
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=69+IJ
        IDLAM(LKNT,2)=13
        IDLAM(LKNT,3)=-14
        lknt=lknt+1
        xlam(lknt)=xlam(lknt-1)
        idlam(lknt,1)=-idlam(lknt-1,1)
        idlam(lknt,2)=-idlam(lknt-1,2)
        idlam(lknt,3)=-idlam(lknt-1,3)
 51     CONTINUE
        XXM(11)=PMAS(61,1)
        xxm(12)=PMAS(63,1)
        if(xxm(11).lt.axmi.and.xxm(12).lt.axmi) goto 511
        if(xxm(11).lt.axmi) then
         xxm(11)=1.E6
        elseif(xxm(12).lt.axmi) then
         xxm(12)=1.E6
        endif
        LKNT=LKNT+1
        XLAM(LKNT)=c1**2/XMI3/(16.*PI)*
     $   GAUSS(X2XW5,S12MIN,S12MAX,PREC)
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=69+IJ
        IDLAM(LKNT,2)=15
        IDLAM(LKNT,3)=-16
        lknt=lknt+1
        xlam(lknt)=xlam(lknt-1)
        idlam(lknt,1)=-idlam(lknt-1,1)
        idlam(lknt,2)=-idlam(lknt-1,2)
        idlam(lknt,3)=-idlam(lknt-1,3)
C.......Now, do the quarks
 511    CONTINUE
        FID=1
        EI=KCHG(FID,1)/3.
        T3=-.5
        xxm(7)=-SR2*(T3*z(ix,2)-TANW*(T3-EI)*z(ix,1))*U(ij,1)
        FID=2
        EI=KCHG(FID,1)/3.
        T3=.5
        xxm(8)=-SR2*(T3*z(ix,2)-TANW*(T3-EI)*z(ix,1))*V(ij,1)

        XXM(11)=PMAS(41,1)
        XXM(12)=PMAS(43,1)
        if( xxm(11).lt.axmi .and. xxm(12).lt.axmi ) goto 61
        if(xxm(11).lt.axmi) then
         xxm(11)=1.E6
        elseif(xxm(12).lt.axmi) then
         xxm(12)=1.E6
        endif
        LKNT=LKNT+1
        XLAM(LKNT)=3.*c1**2/XMI3/(16.*PI)*
     $   GAUSS(X2XW5,S12MIN,S12MAX,PREC)
        IDLAM(LKNT,1)=69+IJ
        IDLAM(LKNT,2)=1
        IDLAM(LKNT,3)=-2
        LKNT=LKNT+1
        xlam(lknt)=xlam(lknt-1)
        idlam(lknt,1)=-idlam(lknt-1,1)
        idlam(lknt,2)=-idlam(lknt-1,2)
        idlam(lknt,3)=-idlam(lknt-1,3)
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=69+IJ
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
 1002 continue
C.....chi0_i -> chi+_i + H-
      DO IJ=1,2
       XMJ=smw(ij)
       AXMJ=ABS(XMJ)
       XMJ2=XMJ**2
       XMHP=pmas(ithc,1)
       XMHP2=XMHP**2
       IF(aXMI.GE.AXMJ+XMHP) THEN
        LKNT=LKNT+1
        GL=cbeta*(z(ix,4)*v(ij,1)+(z(ix,2)+z(ix,1)*TANW)*v(ij,2)/SR2)
        GR=sbeta*(z(ix,3)*u(ij,1)-(z(ix,2)+z(ix,1)*TANW)*u(ij,2)/SR2)
        XLAM(LKNT)=x2xh(c1,xmi,xmj,xmhp,gl,gr)
        IDLAM(LKNT,1)=69+IJ
        IDLAM(LKNT,2)=-ithc
        IDLAM(LKNT,3)=0
        lknt=lknt+1
        xlam(lknt)=xlam(lknt-1)
        idlam(lknt,1)=-idlam(lknt-1,1)
        idlam(lknt,2)=-idlam(lknt-1,2)
        idlam(lknt,3)=-idlam(lknt-1,3)
       ELSE

       ENDIF
      ENDDO
C.....2-BODY DECAYS to fermion sfermion
      DO J=1,5,2
C       XMF=PMAS(J,1)
       XMF=0.0
C......d~ d_L
       XMSF1=PMAS(41+2*(J-1),1)
       EI=KCHG(J,1)/3.
       AL=-z(ix,2)+TANW*z(ix,1)*(2.*EI+1)
       BL=XMF*z(ix,3)/XMW/CBETA
       AR=2.*EI*TANW*z(ix,1)
       BR=-BL
       IF(aXMI.GE.XMF+XMSF1) THEN
        LKNT=LKNT+1
        XMA2=XMSF1**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=.5*3.*c1/8./XMI3*SQRT(XL)*( (XMI2+XMB2-XMA2)*
     $   (AL**2+BL**2)+4.*AL*BL*XMF*XMI)
        IDLAM(LKNT,1)=41+2*(J-1)
        IDLAM(LKNT,2)=-J
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=-IDLAM(LKNT-1,1)
        IDLAM(LKNT,2)=-IDLAM(LKNT-1,2)
        IDLAM(LKNT,3)=0
       ENDIF
C......d~ d_R
       XMSF2=PMAS(40+2*J,1)
       IF(aXMI.GE.XMF+XMSF2) THEN
        LKNT=LKNT+1
        XMA2=XMSF2**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=.5*3.*c1/8./XMI3*SQRT(XL)*( (XMI2+XMB2-XMA2)*
     $   (AR**2+BR**2)+4.*AR*BR*XMF*XMI)
        IDLAM(LKNT,1)=(40+2*J)
        IDLAM(LKNT,2)=-J
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=-IDLAM(LKNT-1,1)
        IDLAM(LKNT,2)=-IDLAM(LKNT-1,2)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
      DO J=2,6,2
c       XMF=PMAS(J,1)
c       XMFP=PMAS(J-1,1)

       XMF=0.0
       IF(J.EQ.6) XMF=PMAS(6,1)
       XMFP=0.0
C......u u_1~
       XMSF1=PMAS(43+2*(J-2),1)
       EI=KCHG(J,1)/3.
       AL=z(ix,2)+TANW*z(ix,1)*(2.*EI-1)
       BL=XMF*z(ix,4)/XMW/SBETA
       AR=2.*EI*TANW*z(ix,1)
       BR=-BL
C.......For stop, the mass eigenstates are different.
C.......For most cases, qL = q1, qR = q2
       IF(J.EQ.6) THEN
        ALP=SINST*AR + COSST*AL
        BLP=SINST*BR + COSST*BL
        ARP=-SINST*AL + COSST*AR
        BRP=-SINST*BL + COSST*BR
        AL=ALP
        BL=BLP
        AR=ARP
        BR=BRP
       ENDIF
       IF(aXMI.GE.XMF+XMSF1) THEN
        LKNT=LKNT+1
        XMA2=XMSF1**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=.5*3.*c1/8./XMI3*SQRT(XL)*( (XMI2+XMB2-XMA2)*
     $   (AL**2+BL**2) + 4.*AL*BL*XMF*XMI)
        IDLAM(LKNT,1)=-(43+2*(J-2))
        IDLAM(LKNT,2)=J
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=-IDLAM(LKNT-1,1)
        IDLAM(LKNT,2)=-IDLAM(LKNT-1,2)
        IDLAM(LKNT,3)=0
       ENDIF
C......u u_2~
       XMSF2=PMAS(44+2*(J-2),1)
       IF(aXMI.GE.XMF+XMSF2) THEN
        LKNT=LKNT+1
        XMA2=XMSF2**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=.5*3.*c1/8./XMI3*SQRT(XL)*( (XMI2+XMB2-XMA2)*
     $   (AR**2+BR**2) + 4.*AR*BR*XMF*XMI )
        IDLAM(LKNT,1)=-(44+2*(J-2))
        IDLAM(LKNT,2)=J
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=-IDLAM(LKNT-1,1)
        IDLAM(LKNT,2)=-IDLAM(LKNT-1,2)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
      DO J=11,15,2
       XMF=0.0d0
C......e+ e_L-
       XMSF1=PMAS(53+2*(J-11),1)
       EI=KCHG(J,1)/3.
       AL=-z(ix,2)+TANW*z(ix,1)*(2.*EI+1)
       BL=XMF*z(ix,3)/XMW/CBETA
       AR=2.*EI*TANW*z(ix,1)
       BR=-BL
       IF(aXMI.GE.XMF+XMSF1) THEN
        LKNT=LKNT+1
        XMA2=XMSF1**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=.5*c1/8./XMI3*SQRT(XL)*( (XMI2+XMB2-XMA2)*
     $   (AL**2+BL**2)+4.*AL*BL*XMF*XMI)
        IDLAM(LKNT,1)=53+2*(J-11)
        IDLAM(LKNT,2)=-J
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=-IDLAM(LKNT-1,1)
        IDLAM(LKNT,2)=-IDLAM(LKNT-1,2)
        IDLAM(LKNT,3)=0
       ENDIF
C......e+ e_R-
       XMSF2=PMAS(54+2*(J-11),1)
       IF(AXMI.GE.XMF+XMSF2) THEN
        LKNT=LKNT+1
        XMA2=XMSF2**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=.5*c1/8./XMI3*SQRT(XL)*( (XMI2+XMB2-XMA2)*
     $   (AR**2+BR**2)+4.*AR*BR*XMF*XMI)
        IDLAM(LKNT,1)=(54+2*(J-11))
        IDLAM(LKNT,2)=-J
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=-IDLAM(LKNT-1,1)
        IDLAM(LKNT,2)=-IDLAM(LKNT-1,2)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
      DO J=12,16,2
       XMF=0.0
C......nu nu_L~
       XMSF1=PMAS(55+2*(J-12),1)
       EI=KCHG(J,1)/3.
       AL=z(ix,2)+TANW*z(ix,1)*(2.*EI-1)
       BL=XMF*z(ix,4)/XMW/SBETA
       AR=2.*EI*TANW*z(ix,1)
       BR=-BL
       IF(aXMI.GE.XMF+XMSF1) THEN
        LKNT=LKNT+1
        XMA2=XMSF1**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
        XLAM(LKNT)=.5*c1/8./XMI3*SQRT(XL)*( (XMI2+XMB2-XMA2)*
     $   (AL**2+BL**2)+4.*AL*BL*XMF*XMI)
        IDLAM(LKNT,1)=-(55+2*(J-12))
        IDLAM(LKNT,2)=J
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        XLAM(LKNT)=XLAM(LKNT-1)
        IDLAM(LKNT,1)=-IDLAM(LKNT-1,1)
        IDLAM(LKNT,2)=-IDLAM(LKNT-1,2)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
C.....3-BODY DECAY TO q q~ gluino
      IF(AXMI.GE.pmas(65,1)) THEN
       XMJ=pmas(65,1)
       axmj=abs(xmj)
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
       s12max=(axmi-axmj)**2
C......all quarks but t
       xxm(11)=0.0
       xxm(12)=0.0
       xxm(13)=1.0
       xxm(14)=-SR2*(-.5*z(ix,2)+TANW*z(ix,1)/6.)
       xxm(15)=1.0
       xxm(16)=SR2*(-TANW*z(ix,1)/3.)
       IF( xxm(5).lt.axmi .or. xxm(6).lt.axmi ) goto 7000
       LKNT=LKNT+1
       XLAM(LKNT)=4.*c1*as/XMI3/(16.*PI)*
     $    GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)
       IDLAM(LKNT,1)=65
       IDLAM(LKNT,2)=1
       IDLAM(LKNT,3)=-1
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=65
       IDLAM(LKNT,2)=3
       IDLAM(LKNT,3)=-3
 7000  CONTINUE
       xxm(5)=pmas(49,1)
       xxm(6)=pmas(50,1)
       IF( xxm(5).lt.axmi .or. xxm(6).lt.axmi ) goto 7001
       lknt=lknt+1
       XLAM(LKNT)=.5*c1*as/XMI3/(16.*PI)*
     $    GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)
       IDLAM(LKNT,1)=65
       IDLAM(LKNT,2)=5
       IDLAM(LKNT,3)=-5
C......u-type quarks
 7001  continue
       xxm(5)=pmas(43,1)
       xxm(6)=pmas(44,1)
       xxm(13)=1.0
       xxm(14)=-SR2*(.5*z(ix,2)+TANW*z(ix,1)/6.)
       xxm(15)=1.0
       xxm(16)=SR2*(2.*TANW*z(ix,1)/3.)
       IF( xxm(5).lt.axmi .or. xxm(6).lt.axmi ) goto 8000
       LKNT=LKNT+1
       XLAM(LKNT)=.5*c1*as/XMI3/(16.*PI)*
     $    GAUSS(X2XZ5,S12MIN,S12MAX,1.E-3)
       IDLAM(LKNT,1)=65
       IDLAM(LKNT,2)=2
       IDLAM(LKNT,3)=-2
       LKNT=LKNT+1
       XLAM(LKNT)=XLAM(LKNT-1)
       IDLAM(LKNT,1)=65
       IDLAM(LKNT,2)=4
       IDLAM(LKNT,3)=-4
 8000  continue
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
      IF(XLAM(0).eq.0.0) XLAM(0)=1.E-6
C
      RETURN
      END
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      FUNCTION X2XZ5(X)
      IMPLICIT NONE
C
C     chi0 -> chi0 + f f~
C
      REAL X2XZ5,X
      real*8 xm12,xm22,xm32,s,s23,s13,wprop2
      real*8 ww,wf1,wf2,wfl1,wfl2
      real*8 sij
      real*8 sr2,OL,OR,FLD,FLU,XMV,XMG,XMSU,XMSD
      real*8 LE,RE,LE2,RE2,OL2,OR2,FLI,FLJ,FRI,FRJ
      real*8 s23min,s23max,s23ave,s23del
      real*8 tint,tprop
      real*8 tint2,tint3,utint
      integer I

      data sr2/1.4142136/
      REAL XXM(20)
      COMMON/PARINT/XXM
C
      XM12=xxm(1)**2
      XM22=xxm(2)**2
      XM32=xxm(3)**2
      S=XXM(4)**2
      S13=X

      S23AVE=xm22+xm32-.5/X*(X+xm32-xm12)*(X+xm22-s)
      S23DEL=.5/X*sqrt( ( (x-xm12-xm32)**2-4.*xm12*xm32)*
     $                  ( (x-xm22-s)**2  -4.*xm22*s  ) )

      S23MIN=(s23ave-s23del)
      s23max=(s23ave+s23del)

      XMV=xxm(7)
      XMG=XXM(8)
      XMSD=XXM(5)**2
      XMSU=XXM(6)**2
      OL=XXM(9)
      OR=XXM(10)
      OL2=OL**2
      OR2=OR**2
      LE=XXM(11)
      RE=XXM(12)
      LE2=LE**2
      RE2=RE**2
      FLI=xxm(13)
      FLJ=xxm(14)
      FRI=xxm(15)
      FRJ=xxm(16)
C
      WPROP2=(S13-XMV**2)**2+(XMV*XMG)**2
      SIJ=2.*xxm(2)*xxm(4)*S13
C.....
      IF(XMV.LE.1000.) THEN
       WW=2.*(LE2+RE2)*(OL2)*( 2.*TINT(S23MAX,s23min,xm22,s)
     $    +SIJ*(S23Max-s23min) )/WPROP2
       IF(XXM(5).LE.10000.) THEN
        WFL1=2.*FLI*FLJ*OL*LE*( 2.*TINT2(s23max,s23min,xm22,s,xmsd)
     $     + SIJ*TPROP(s23max,s23min,xmsd) )
        WFL1=WFL1*(S13-XMV**2)/WPROP2
       ELSE
        WFL1=0.0
       ENDIF
       IF(XXM(6).LE.10000.) THEN
        WFL2=2.*FRI*FRJ*or*re*( 2.*tint2(s23max,s23min,xm22,s,xmsu)
     $     + SIJ*TPROP(s23max,s23min,xmsu) )
        WFL2=WFL2*(S13-XMV**2)/WPROP2
       ELSE
        WFL2=0.0
       ENDIF
      ELSE
       WW=0.0
       WFL1=0.0
       WFL2=0.0
      ENDIF
      IF(XXM(5).LE.10000.) THEN
       WF1=.5*(FLI*FLJ)**2*( 2.*TINT3(s23max,s23min,xm22,s,xmsd)
     $    + SIJ*UTINT(s23max,s23min,xmsd,xm22+s-s13-xmsd) )
      ELSE
       WF1=0.0
      ENDIF
      IF(XXM(6).LE.10000.) THEN
       WF2=.5*(FRI*FRJ)**2*( 2.*TINT3(s23max,s23min,xm22,s,xmsu)
     $    + SIJ*UTINT(s23max,s23min,xmsu,xm22+s-s13-xmsu) )
      ELSE
       WF2=0.0
      ENDIF
C
c      WFl1=0.0
c      WFl2=0.0
      X2XZ5=(WW+WF1+WF2+WFL1+WFL2)
      IF(X2XZ5.lt.0.0) THEN
       print*,' negative wt in x2xz5 '
       print*,xxm(1),xxm(2),xxm(3),XXM(4)
       print*,(XXM(I),I=5,8)
       print*,(XXM(I),I=9,12)
       print*,(XXM(I),I=13,16)
       print*,ww,wf1,wf2,wfl1,wfl2
       print*,s23min,s23max
       x2xz5=0.0
      ENDIF
      RETURN
      END
C.....
      FUNCTION TINT(X,y,a,b)
      implicit none
      real*8 tint
      real*8 x,a,b,y
C      / x
C      | (t-a)*(b-t) dt
C     / y
      TINT=(X-Y)*(-(X**2+X*Y+Y**2)/3.+(B+A)*(X+Y)/2.-A*B)
      RETURN
      END
C.....
      FUNCTION UTINT(X,y,a,b)
      implicit none
      real*8 utint,v
      real*8 x,y,a,b
C      / x
C      | 1/(t-a)*1/(b-t) dt
C     / y
      V=ABS((x-a)/(b-x)*(b-y)/(y-a))
      UTINT=DLOG(v)/(b-a)
      RETURN
      END
C.....
      FUNCTION TINT2(x,y,a,b,c)
      implicit none
      real*8 tint2,v
      real*8 x,y,a,b,c
C      /
C      | (t-a)*(b-t)/(t-c) dt
C     /
      V=ABS((X-c)/(y-c))
      TINT2=(X-Y)*(-.5*(X+Y)+(b+a-c))-
     $ DLOG(v)*(c-b)*(c-a)
      RETURN
      END
C.....
      FUNCTION TINT3(x,y,a,b,c)
      implicit none
      real*8 x,y,a,b,c
      real*8 tint3,V
C      /
C      | (t-a)*(b-t)/(t-c)**2 dt
C     /
      V=ABS((x-c)/(y-c))
      TINT3=-(x-y)+(c-a)*(c-b)*(y-x)/(x-c)/(y-c)+
     $ (b+a-2.d0*c)*dlog(V)
      RETURN
      END
C
      FUNCTION TPROP(x,y,a)
      implicit none
      real*8 tprop,v
      real*8 x,a,y
C      /
C      | 1/(t-a) dt
C     /
      V=abs((x-a)/(y-a))
      TPROP=DLOG(V)
      RETURN
      END








