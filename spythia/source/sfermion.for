      SUBROUTINE sfermion(KCIN,XLAM,IDLAM,IKNT)
      implicit none
C.....LUND VARIABLES
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:SUSYPAR.INC'
C.....Local variables
      integer KCIN
      REAL XMI,XMJ,XMF,XMSF1,XMSF2,XMW,XMW2,XMZ,XMZ2,AXMJ,AXMI
      REAL xmi2,xmi3,xmj2,XMA2,XMB2,XMFP
      REAL LAMFUN,XL
      REAL TANW,XW,AEM,C1,AS
      REAL CA,CB,AL,AR,BL,BR,ALP,ARP,BLP,BRP
      REAL XLAM(0:100)
      INTEGER IDLAM(100,3)
      INTEGER LKNT,IX,IC,ILR,IDU,J,IJ,I,IKNT,IFL
      REAL SR2
      DATA SR2/1.4142136/
      REAL CBETA,SBETA,GR,GL,F12k,F21k
      REAL CW
      real cosa,sina
      REAL ULALEM,PI,ULALPS,EI
      DATA PI/3.141592654/

      REAL GHRR,GHLL,GHLR,CF

c--------------------------
C.....count the number of decay modes
      LKNT=0

      XMW=PMAS(24,1)
      XMW2=XMW**2
      XMZ=PMAS(23,1)
      XMZ2=XMZ**2
      XW=1.-XMW2/XMZ2
      TANW = SQRT(XW/(1.-XW))
      CW=sqrt(1.-XW)
C
      IC=KCIN-40
C.....No nu_R decays
      IF(IC.eq.16.or.ic.eq.20.or.ic.eq.24) RETURN
      ILR=3-(1+MOD(IC,2))
      IF(KCIN.LE.52) THEN
       IFL=1+(IC-1)/2
      ELSE
       IFL=5+(IC-1)/2
      ENDIF
      IDU=3-(1+MOD(IFL,2))
C
      XMI=pmas(KCIN,1)
      XMI2=XMI**2
      AEM=ULALEM(XMI2)
      AS =ULALPS(XMI2)
      C1=AEM/XW
      XMI3=XMI**3

      SBETA=sin(beta)
      CBETA=cos(beta)
      sina=sin(alfa)
      cosa=cos(alfa)
C
C.....2-BODY DECAYS of fermion -> sfermion + gauge/gaugino
C
C......Charged Decays:
      DO IX=1,2
C......di -> u chi1-,chi2-
       IF(IDU.eq.1) THEN
        XMFP=0.0
        XMF=0.0
        IF(IC.ge.9.and.IC.LE.10) XMF=PMAS(6,1)
C......ui -> d chi1+,chi2+
       ELSE
        XMF=0.0
        XMFP=0.0
        IF(IC.ge.11.and.IC.LE.12) XMFP=PMAS(6,1)
       ENDIF
       XMJ=smw(ix)
       AXMJ=ABS(XMJ)
       IF(XMI.GE.AXMJ+XMF) THEN
        IF(IDU.eq.2) THEN
         AL=v(ix,1)
         BL=-XMF*u(ix,2)/SR2/XMW/CBETA
         AR=-XMFP*v(ix,2)/SR2/XMW/SBETA
         BR=0.0
        ELSE
         AL=u(ix,1)
         BL=-XMF*v(ix,2)/SR2/XMW/SBETA
         AR=-XMFP*u(ix,2)/SR2/XMW/CBETA
         BR=0.0
        ENDIF
        IF(IC.GE.9.and.IC.LE.12) THEN
         ALP=COSST*AL + SINST*AR
         BLP=COSST*BL + SINST*BR
         ARP=COSST*AR - SINST*AL
         BRP=COSST*BR - SINST*BL
         AL=ALP
         BL=BLP
         AR=ARP
         BR=BRP
        ENDIF
C........f1 -> f` chi
        IF(ILR.EQ.1) THEN
         CA=AL
         CB=BL
C........f2 -> f` chi
        ELSE
         CA=AR
         CB=BR
        ENDIF
        LKNT=LKNT+1
        XMA2=XMJ**2
        XMB2=XMF**2
        XL=LAMFUN(XMI2,XMA2,XMB2)
C.......Spin average = 1/1 not 1/2....No color enhancement
        XLAM(LKNT)=2.*c1/8./XMI3*SQRT(XL)*((XMI2-XMB2-XMA2)*
     $     (CA**2+CB**2)+4.*CA*CB*XMJ*XMF)
        IDLAM(LKNT,3)=0
        IF(IDU.EQ.1) THEN
         IDLAM(LKNT,1)=-(69+IX)
         IDLAM(LKNT,2)=IFL+1
        ELSE
         IDLAM(LKNT,1)=(69+IX)
         IDLAM(LKNT,2)=IFL-1
        ENDIF
       ENDIF
      ENDDO
C
C......Neutral Decays
      DO IX=1,4
C......di -> d chi10
       IF(IDU.eq.1) THEN
        XMF=0.0
C......ui -> u chi10
       ELSE
        XMF=0.0
        IF(IC.ge.9.and.IC.LE.12) XMF=PMAS(6,1)
       ENDIF
       XMJ=smz(ix)
       AXMJ=ABS(XMJ)
       IF(XMI.GE.AXMJ+XMF) THEN
        EI=KCHG(KCIN,1)/3.
        IF(IDU.eq.1) THEN
         AL=-z(ix,2)+TANW*z(ix,1)*(2.*EI+1)
         BL=XMF*z(ix,3)/XMW/CBETA
         AR=2.*EI*TANW*z(ix,1)
         BR=-BL
        ELSE
         AL=z(ix,2)+TANW*z(ix,1)*(2.*EI-1)
         BL=XMF*z(ix,4)/XMW/SBETA
         AR=2.*EI*TANW*z(ix,1)
         BR=-BL
        ENDIF
        IF(IC.GE.11.and.IC.LE.12) THEN
         ALP=COSST*AL + SINST*AR
         BLP=COSST*BL + SINST*BR
         ARP=COSST*AR - SINST*AL
         BRP=COSST*BR - SINST*BL
         AL=ALP
         BL=BLP
         AR=ARP
         BR=BRP
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
C.......Spin average = 1/1 not 1/2....No color enhancement
        XLAM(LKNT)=c1/8./XMI3*SQRT(XL)*((XMI2-XMB2-XMA2)*
     $     (CA**2+CB**2)+4.*CA*CB*XMJ*XMF)
        IDLAM(LKNT,1)=65+IX
        IDLAM(LKNT,2)=IFL
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
C.....2-body decays of fL -> fL' + W
      IF(ILR.EQ.1) THEN
       IF(IDU.eq.1) THEN
        XMSF1=PMAS(KCIN+2,1)
       ELSE
        XMSF1=PMAS(KCIN-2,1)
       ENDIF
       IF(XMI.GE.XMSF1+XMW) THEN
        XL=LAMFUN(XMI2,XMSF1**2,XMW**2)
        LKNT=LKNT+1
        XLAM(LKNT)=c1/8./XMI3*XL**1.5/XMW2
        IF(IC.EQ.11.or.IC.EQ.9) THEN
         XLAM(LKNT)=XLAM(LKNT)*COSST**2
        ENDIF
        IDLAM(LKNT,3)=0
        IF(IDU.eq.1) THEN
         IDLAM(LKNT,1)=KCIN+2
         IDLAM(LKNT,2)=-24
        ELSE
         IDLAM(LKNT,1)=KCIN-2
         IDLAM(LKNT,2)=24
        ENDIF
       ENDIF
      ENDIF
C.....t2 -> bL + W
      IF(IC.eq.12) THEN
       XMSF1=pmas(49,1)
       IF(XMI.GE.XMSF1+XMW) THEN
        XL=LAMFUN(XMI2,XMSF1**2,XMW2)
        LKNT=LKNT+1
        XLAM(LKNT)=c1/8./XMI3*XL**1.5/XMW2*SINST**2
        IDLAM(LKNT,3)=0
        IDLAM(LKNT,1)=KCIN-2
        IDLAM(LKNT,2)=24
       ENDIF
       XMSF1=pmas(51,1)
C......t2 -> t1 + Z0
       IF(XMI.GE.XMSF1+XMZ) THEN
        XL=LAMFUN(XMI2,XMSF1**2,XMZ2)
        LKNT=LKNT+1
        XLAM(LKNT)=c1/8./XMI3*XL**1.5/XMZ2*(SINST*COSST/2.)**2
        IDLAM(LKNT,3)=0
        IDLAM(LKNT,1)=51
        IDLAM(LKNT,2)=23
       ENDIF
C......t2 -> t1 + H0_1
       XMJ=PMAS(25,1)
       IF(XMI.GE.XMSF1+XMJ) THEN
        CF=3.
        XMJ2=XMJ**2
        XL=LAMFUN(XMI2,XMJ2,XMSF1**2)
        XMF=pmas(6,1)
        EI=KCHG(6,1)/3.
        LKNT=LKNT+1
        ghll=XMZ/CW*(.5-EI*xw)*SIN(ALfa+BEta)-
     $     XMF**2/XMW*cosa/sbeta
        ghrr=XMZ/CW*(EI*xw)*SIN(ALfa+BEta)-
     $     XMF**2/XMW*cosa/sbeta
        ghlr=XMF/2./XMW/sbeta*(MUZ*sina-
     $     ATRI_T*cosa)
        XLAM(LKNT)=CF*SQRT(XL)/4.*c1/XMI3*
     $     (SINST*COSST*(ghrr-ghll)+ghlr*(COSST**2-SINST**2))**2
        IDLAM(LKNT,1)=51
        IDLAM(LKNT,2)=72
        IDLAM(LKNT,3)=0
       ENDIF
      ENDIF
C
C.....2-BODY DECAYS of squark -> quark gluino
C
C.....IC = 1,12 dL...t2
C.....
      IF(IC.LE.12) THEN
       XMFP=0.0
       XMF=0.0
       IF(IC.ge.11.and.IC.LE.12) XMF=PMAS(6,1)
       XMJ=pmas(65,1)
       AXMJ=ABS(XMJ)
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
        XLAM(LKNT)=4./3.*AS/2./XMI3*SQRT(XL)*((XMI2-XMB2-XMA2)*
     $     (CA**2+CB**2)+4.*CA*CB*XMJ*XMF)
        IDLAM(LKNT,1)=65
        IDLAM(LKNT,2)=IFL
        IDLAM(LKNT,3)=0
       ENDIF
      ENDIF
C.....If nothing else for t1, then t1* -> c+chi0
      IF(LKNT.EQ.0.AND.KCIN.EQ.51) THEN
C......This is a back-of-the-envelope estimate
C......M = 1/(16pi**2)g**3 = g*2/(4pi) g/(4pi) = c1 * g/(4pi)
C......M*M = c1**2 * g**2/(16pi**2)
C......G = 1/(8pi)p/MI**2 * M*M = c1**3/(32pi**2)*lam/(2*MI**3)
       LKNT=LKNT+1
       XL=LAMFUN(XMI2,0.,PMAS(66,1)**2)
       XLAM(LKNT)=c1**3/64./pi**2/XMI3*SQRT(XL)
       IDLAM(LKNT,1)=66
       IDLAM(LKNT,2)=4
       IDLAM(LKNT,3)=0
      ENDIF
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







