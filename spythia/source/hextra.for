      SUBROUTINE HEXTRA(KCIN,XLAM,IDLAM,IKNT)
      implicit none
C.....LUND VARIABLES
      include 'D0$SPYTHIA$INC:LUDAT1.INC'
      include 'D0$SPYTHIA$INC:LUDAT2.INC'
      include 'D0$SPYTHIA$INC:PYPARS.INC'
      include 'D0$SPYTHIA$INC:SUSYPAR.INC'
C.....Local variables
      integer KCIN
      REAL XMI,XMJ,XMF,XMSF1,XMSF2,XMW,XMW2,XMZ,XMZ2,AXMJ,AXMI
      REAL XMFP,XMF1,XMF2,xmsl,xmg
      real s12min,s12max
      REAL xmi2,xmi3,xmj2,XMH,XMH2,XMHP,XMHP2,XMA2,XMB2
      REAL LAMFUN,XL,CF,EI
      integer idu,ic,ilr,ifl
      REAL TANW,XW,AEM,C1,AS
      REAL H2XX,ghll,ghrr,ghlr
      REAL XLAM(0:100)
      INTEGER IDLAM(100,3)
      INTEGER LKNT,IX,IH,J,IJ,I,IKNT,IK
      INTEGER ITH(3)
      DATA ITH/72,73,74/
      REAL ETAH(3),CH(3),DH(3),EH(3)
      DATA ETAH/1.,1.,-1./
      REAL SR2
      DATA SR2/1.4142136/
      REAL CBETA,SBETA,GR,GL,F12k,F21k,tanb
      REAL ULALEM,PI,ULALPS
      DATA PI/3.141592654/
      REAL AL,BL,AR,BR,ALP,ARP,BLP,BRP
      REAL XMK,AXMK,XMK2,COSA,SINA,CW,XML

C.....count the number of decay modes
      LKNT=0

      XMW=PMAS(24,1)
      XMW2=XMW**2
      XMZ=PMAS(23,1)
      XMZ2=XMZ**2
      XW=1.-XMW2/XMZ2
      TANW = SQRT(XW/(1.-XW))
      CW=SQRT(1.-XW)

C.....1 - 4 depending on 72 - 74
      IH=KCIN-71
C.....66-69
      XMI=pmas(kcin,1)
      XMI2=XMI**2
      AXMI=ABS(XMI)
      AEM=ULALEM(XMI2)
      AS =ULALPS(XMI2)
      C1=AEM/XW
      XMI3=abs(XMI**3)

      TANB=TAN(BETA)
      SBETA=sin(beta)
      CBETA=cos(beta)
      COSA=COS(ALFA)
      SINA=SIN(ALFA)

      IF(IH.EQ.4) GOTO 2000
C.....CHECK ALL 2-BODY DECAYS TO GAUGE and HIGGS bosons
C.....H0_k -> chi0_i + chi0_j
      EH(1)=cosa
      EH(2)=sina
      EH(3)=-SBETA
      DH(1)=-sina
      DH(2)=cosa
      DH(3)=cbeta
      DO IJ=1,4
       XMJ=smz(ij)
       AXMJ=ABS(XMJ)
       DO IK=1,IJ
        XMK=smz(ik)
        AXMK=abs(XMK)
        IF(aXMI.GE.AXMJ+AXMK) THEN
         LKNT=LKNT+1
         F21K=.5*eh(ih)*( z(ik,3)*z(ij,2)+z(ij,3)*z(ik,2)
     $ -TANW*(z(ik,3)*z(ij,1)+z(ij,3)*z(ik,1)) )+
     $        .5*dh(ih)*( z(ik,4)*z(ij,2)+z(ij,4)*z(ik,2)
     $ -TANW*(z(ik,4)*z(ij,1)+z(ij,4)*z(ik,1)) )
         F21K=.5*eh(ih)*(z(ij,3)*z(ik,2)+z(ik,3)*z(ij,2)
     $ -TANW*(z(ij,3)*z(ik,1)+z(ik,3)*z(ij,1)))+
     $        .5*dh(ih)*( z(ij,4)*z(ik,2)+z(ik,4)*z(ij,2)
     $ -TANW*(z(ij,4)*z(ik,1)+z(ik,4)*z(ij,1)) )
C......Sign of masses i,j
         XML=XMK*ETAH(IH)
         XLAM(LKNT)=H2XX(c1,XMI,XMJ,XML,F12K,F21K)
         IF(IJ.eq.IK) XLAM(LKNT)=XLAM(LKNT)*.5
         IDLAM(LKNT,1)=65+IJ
         IDLAM(LKNT,2)=65+IK
         IDLAM(LKNT,3)=0
        ENDIF
       ENDDO
      ENDDO
C.....H0_k -> chi+_i chi-_j
      DO IJ=1,2
       XMJ=smw(ij)
       AXMJ=ABS(XMJ)
       DO IK=1,2
        XMK=smw(ik)
        AXMK=abs(XMK)
        IF(aXMI.GE.AXMJ+AXMK) THEN
         LKNT=LKNT+1
         F21K=(v(ij,1)*u(ik,2)*eh(ih) - v(ij,2)*u(ik,1)*dh(ih))/SR2
         F12K=(v(ik,1)*u(ij,2)*eh(ih) - v(ik,2)*u(ij,1)*dh(ih))/SR2
         XML=XMK*ETAH(IH)
         XLAM(LKNT)=H2XX(c1,XMI,XMJ,XML,F12K,F21K)
         IDLAM(LKNT,1)=69+IJ
         IDLAM(LKNT,2)=-(69+IK)
         IDLAM(LKNT,3)=0
        ENDIF
       ENDDO
      ENDDO
C.....Higgs to sfermion sfermion
      IF(IH.EQ.3) GOTO 1997
      DO IJ=41,63
       if(ij.eq.56.or.ij.eq.60) goto 1998
       IF(AXMI.ge.2.*pmas(IJ,1)) THEN
        LKNT=LKNT+1
        XMJ=PMAS(IJ,1)
        XMJ2=XMJ**2
        XL=LAMFUN(XMI2,XMJ2,XMJ2)
        ILR=3-(1+MOD(IJ,2))
        IF(IJ.LE.52) THEN
         IFL=1+(IJ-41)/2
        ELSE
         IFL=5+(IJ-41)/2
        ENDIF
        XMF=PMAS(IFL,1)
        EI=KCHG(IFL,1)/3.
        IDU=3-(1+MOD(IFL,2))
        IF(IH.EQ.1) THEN
         IF(IDU.EQ.1) THEN
          ghll=-XMZ/CW*(.5+EI*xw)*SIN(ALfa+BEta)+
     $       XMF**2/XMW*sina/cbeta
          ghrr=XMZ/CW*(EI*xw)*SIN(ALfa+BEta)+
     $       XMF**2/XMW*sina/cbeta
          if(IJ.LE.52.and.IJ.GE.51) THEN
           ghlr=-XMF/2./XMW/cbeta*(MUZ*cosa-
     $        ATRI_T*sina)
          elseif(IJ.LE.50.and.IJ.GE.49) THEN
           ghlr=-XMF/2./XMW/cbeta*(MUZ*cosa-
     $        ATRI_B*sina)
          elseif(IJ.LE.62.and.IJ.LE.61) THEN
           ghlr=-XMF/2./XMW/cbeta*(MUZ*cosa-
     $        ATRI_L*sina)
          else
           ghlr=0.0
          endif
         ELSE
          ghll=XMZ/CW*(.5-EI*xw)*SIN(ALfa+BEta)-
     $       XMF**2/XMW*cosa/sbeta
          ghrr=XMZ/CW*(EI*xw)*SIN(ALfa+BEta)-
     $       XMF**2/XMW*cosa/sbeta
          if(IJ.LE.52.and.IJ.GE.51) THEN
           ghlr=XMF/2./XMW/sbeta*(MUZ*sina-
     $        ATRI_T*cosa)
          elseif(IJ.LE.50.and.IJ.GE.49) THEN
           ghlr=XMF/2./XMW/sbeta*(MUZ*sina-
     $        ATRI_B*cosa)
          elseif(IJ.LE.62.and.IJ.LE.61) THEN
           ghlr=XMF/2./XMW/sbeta*(MUZ*sina-
     $        ATRI_L*cosa)
          else
           ghlr=0.0
          endif
         ENDIF
        ELSEIF(IH.EQ.2) THEN
         IF(IDU.eq.1) THEN
          ghll=XMZ/CW*(.5+EI*xw)*cos(ALfa+BEta)-
     $       XMF**2/XMW*cosa/cbeta
          ghrr=-XMZ/CW*(EI*xw)*cos(ALfa+BEta)-
     $       XMF**2/XMW*cosa/cbeta
          if(IJ.LE.52.and.IJ.GE.51) THEN
           ghlr=-XMF/2./XMW/cbeta*(MUZ*sina+
     $        ATRI_T*cosa)
          elseif(IJ.LE.50.and.IJ.GE.49) THEN
           ghlr=-XMF/2./XMW/cbeta*(MUZ*sina+
     $        ATRI_B*cosa)
          elseif(IJ.LE.62.and.IJ.LE.61) THEN
           ghlr=-XMF/2./XMW/cbeta*(MUZ*sina+
     $        ATRI_L*cosa)
          else
           ghlr=0.0
          endif
         ELSE
          ghll=-XMZ/CW*(.5-EI*xw)*cos(ALfa+BEta)-
     $       XMF**2/XMW*sina/sbeta
          ghrr=-XMZ/CW*(EI*xw)*cos(ALfa+BEta)-
     $       XMF**2/XMW*sina/sbeta
          if(IJ.LE.52.and.IJ.GE.51) THEN
          ghlr=-XMF/2./XMW/sbeta*(MUZ*cosa+
     $       ATRI_T*sina)
          elseif(IJ.LE.50.and.IJ.GE.49) THEN
          ghlr=-XMF/2./XMW/sbeta*(MUZ*cosa+
     $       ATRI_B*sina)
          elseif(IJ.LE.62.and.IJ.LE.61) THEN
          ghlr=-XMF/2./XMW/sbeta*(MUZ*cosa+
     $       ATRI_L*sina)
          else
           ghlr=0.0
          endif
         ENDIF
        ENDIF
        IF(ILR.EQ.1) THEN
         IF(IFL.NE.6) THEN
          AL=1.
          AR=0.
         ELSEIF(IFL.EQ.6) THEN
          IF(IJ.EQ.51) THEN
           AL=COSST
           AR=SINST
          ELSE
           AL=-SINST
           AR=COSST
          ENDIF
         ENDIF
        ELSE
         IF(IFL.NE.6) THEN
          AL=0.
          AR=1.
         ELSEIF(IFL.EQ.6) THEN
          IF(IJ.EQ.51) THEN
           AL=COSST
           AR=SINST
          ELSE
           AL=-SINST
           AR=COSST
          ENDIF
         ENDIF
        ENDIF
        IF(IJ.LE.52) THEN
         CF=3.
        ELSE
         CF=1.
        ENDIF
        XLAM(LKNT)=CF*SQRT(XL)/4.*c1/XMI3*
     $     (ghll*AL**2+ghrr*AR**2
     $     +2.*ghlr*AL*AR)**2
        IDLAM(LKNT,1)=IJ
        IDLAM(LKNT,2)=-IJ
        IDLAM(LKNT,3)=0
C.......H0_i -> t2 t1* + t1 t2*
        IF(IJ.eq.51.and.AXMI.GE.(pmas(51,1)+pmas(52,1))) THEN
         LKNT=LKNT+1
         XL=LAMFUN(XMI2,pmas(51,1)**2,pmas(52,1)**2)
         XLAM(LKNT)=3.*SQRT(XL)/4.*c1/XMI3*
     $     (SINST*COSST*(ghrr-ghll)+
     $     ghlr*(COSST**2-SINST**2))**2
         IDLAM(LKNT,1)=51
         IDLAM(LKNT,2)=-52
         IDLAM(LKNT,3)=0
         LKNT=LKNT+1
         xlam(lknt)=xlam(lknt-1)
         idlam(lknt,1)=-idlam(lknt-1,1)
         idlam(lknt,2)=-idlam(lknt-1,2)
         idlam(lknt,3)=-idlam(lknt-1,3)
        ENDIF
       ENDIF
 1998  CONTINUE
      ENDDO
 1997 CONTINUE
      IF(IH.eq.3) THEN
       GL=pmas(6,1)/2./XMW*(MUZ-ATRI_T/tan(beta))
C......A0 -> t1 t1*
       IF(AXMI.GE.2.*pmas(51,1)) THEN
        LKNT=LKNT+1
        XL=LAMFUN(XMI2,PMAS(51,1)**2,pmas(51,1)**2)
        XLAM(LKNT)=3.*SQRT(XL)/4.*c1/XMI3*(GL*SINST*COSST)**2
        IDLAM(LKNT,1)=51
        IDLAM(LKNT,2)=-51
        IDLAM(LKNT,3)=0
       ENDIF
C......A0 -> t2 t2*
       IF(AXMI.GE.2.*pmas(52,1)) THEN
        lknt=lknt+1
        XL=LAMFUN(XMI2,PMAS(52,1)**2,pmas(52,1)**2)
        XLAM(LKNT)=3.*SQRT(XL)/4.*c1/XMI3*(GL*SINST*COSST)**2
        IDLAM(LKNT,1)=52
        IDLAM(LKNT,2)=-52
        IDLAM(LKNT,3)=0
       ENDIF
C......A0 -> t1 t2* + t2 t1*
       IF(AXMI.GE.pmas(52,1)+pmas(51,1)) THEN
        lknt=lknt+1
        XL=LAMFUN(XMI2,PMAS(52,1)**2,pmas(51,1)**2)
        XLAM(LKNT)=3.*SQRT(XL)/4.*c1/XMI3*GL**2*
     $   (COSST**2-SINST**2)**2
        IDLAM(LKNT,1)=51
        IDLAM(LKNT,2)=-52
        IDLAM(LKNT,3)=0
        LKNT=LKNT+1
        xlam(lknt)=xlam(lknt-1)
        idlam(lknt,1)=-idlam(lknt-1,1)
        idlam(lknt,2)=-idlam(lknt-1,2)
        idlam(lknt,3)=-idlam(lknt-1,3)
       ENDIF
      ENDIF
      GOTO 1999
 2000 CONTINUE
C.....charged higgs goes here
C.....H+ -> t b~
      CF=3.
      DO IJ=2,6,2
       IF(XMI.GE.PMAS(IJ,1)+PMAS(IJ-1,1)) THEN
        LKNT=LKNT+1
        XMJ2=PMAS(IJ,1)**2
        XMK2=PMAS(IJ-1,1)**2
        XL=LAMFUN(XMI2,XMJ2,XMK2)
C.......Running masses
        IF(MSTP(37).NE.0) THEN
C........PARU(117)=Lambda_QCD, MSTU(118)=number of quark flavors
        XMJ2=XMJ2*
     &    (LOG(MAX(4.,PARP(37)**2*XMJ2/PARU(117)**2))/
     &    LOG(MAX(4.,XMI2/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
        XMK2=XMK2*
     &    (LOG(MAX(4.,PARP(37)**2*XMK2/PARU(117)**2))/
     &    LOG(MAX(4.,XMI2/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
        ENDIF
        XLAM(LKNT)=CF*c1/8.*SQRT(XL)/XMW2/XMI3*
     $ ( (XMI2-XMJ2-XMK2)*(XMJ2/tanb**2+XMK2*tanb**2) -4.*XMJ2*XMK2)
        IDLAM(LKNT,1)=IJ
        IDLAM(LKNT,2)=-(IJ-1)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
C.....H+ -> tau nu_tau~
      CF=1.
      DO IJ=12,16,2
       IF(XMI.GE.PMAS(IJ,1)+PMAS(IJ-1,1)) THEN
        LKNT=LKNT+1
        XMJ2=PMAS(IJ,1)**2
        XMK2=PMAS(IJ-1,1)**2
        XL=LAMFUN(XMI2,XMJ2,XMK2)
        XLAM(LKNT)=CF*c1/8.*SQRT(XL)/XMW2/XMI3*
     $ ( (XMI2-XMJ2-XMK2)*(XMJ2/tanb**2+XMK2*tanb**2) -4.*XMJ2*XMK2)
        IDLAM(LKNT,1)=IJ
        IDLAM(LKNT,2)=-(IJ-1)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
C.....H+ -> W+ h
      IF(XMI.GE.XMW+PMAS(72,1)) THEN
       LKNT=LKNT+1
       XMJ2=PMAS(72,1)**2
       XL=LAMFUN(XMI2,XMW2,XMJ2)
       XLAM(LKNT)=c1/16./XMI3*SQRT(XL)*(cos(beta-alfa))**2*
     $ (XMW2-2.*(XMI2+XMJ2)+(XMI2-XMJ2)**2/XMW2)
       IDLAM(LKNT,1)=72
       IDLAM(LKNT,2)=24
       IDLAM(LKNT,3)=0
      ENDIF
C.....H+ -> chi+_i + chi0_j
      DO IJ=1,4
       XMJ=smz(ij)
       AXMJ=ABS(XMJ)
       XMJ2=XMJ**2
       DO IK=1,2
        XMK=smw(ik)
        AXMK=abs(XMK)
        XMK2=XMK**2
        IF(aXMI.GE.AXMJ+AXMK) THEN
         LKNT=LKNT+1
         GL=cbeta*(z(ij,4)*v(ik,1)+(z(ij,2)+z(ij,1)*TANW)*v(ik,2)/SR2)
         GR=sbeta*(z(ij,3)*u(ik,1)-(z(ij,2)+z(ij,1)*TANW)*u(ik,2)/SR2)
         XLAM(LKNT)=h2xx(c1,xmi,xmj,xmk,gl,gr)
         IDLAM(LKNT,1)=IJ+65
         IDLAM(LKNT,2)=IK+69
         IDLAM(LKNT,3)=0
        ENDIF
       ENDDO
      ENDDO
      GL=-XMW/SR2*(sin(2.*beta)-pmas(6,1)**2/tan(beta)/XMW2)
      GR=-PMAS(6,1)/SR2/XMW*(MUZ-ATRI_T/tan(beta))
      CF=3.
C.....H+ -> t_1 b_L
      IF(XMI.GE.pmas(51,1)+pmas(49,1)) THEN
       XL=LAMFUN(XMI2,pmas(51,1)**2,pmas(49,1)**2)
       LKNT=LKNT+1
       XLAM(LKNT)=CF*SQRT(XL)/4.*c1/XMI3*(GL*COSST)**2
       IDLAM(LKNT,1)=51
       IDLAM(LKNT,2)=49
       IDLAM(LKNT,3)=0
C.....H+ -> t_2 b_L
      ELSEIF(XMI.GE.pmas(52,1)+pmas(49,1)) THEN
       XL=LAMFUN(XMI2,pmas(52,1)**2,pmas(49,1)**2)
       LKNT=LKNT+1
       XLAM(LKNT)=CF*SQRT(XL)/4.*c1/XMI3*(GL*SINST)**2
       IDLAM(LKNT,1)=52
       IDLAM(LKNT,2)=49
       IDLAM(LKNT,3)=0
      ENDIF
      GL=-XMW/SR2*sin(2.*beta)
      DO IJ=41,45,4
       IF(XMI.GE.pmas(IJ,1)+PMas(IJ+2,1)) THEN
        LKNT=LKNT+1
        XL=LAMFUN(XMI2,pmas(IJ,1)**2,pmas(IJ+2,1)**2)
        XLAM(LKNT)=CF*SQRT(XL)/4.*c1/XMI3*(GL)**2
        IDLAM(LKNT,1)=-IJ
        IDLAM(LKNT,2)=(IJ+2)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
      CF=1.
      DO IJ=53,61,4
       IF(XMI.GE.pmas(IJ,1)+PMas(IJ+2,1)) THEN
        LKNT=LKNT+1
        XL=LAMFUN(XMI2,pmas(IJ,1)**2,pmas(IJ+1,1)**2)
        XLAM(LKNT)=CF*SQRT(XL)/4.*c1/XMI3*(GL)**2
        IDLAM(LKNT,1)=-IJ
        IDLAM(LKNT,2)=(IJ+2)
        IDLAM(LKNT,3)=0
       ENDIF
      ENDDO
C.....
 1999 CONTINUE
      IKNT=LKNT
      XLAM(0)=0.0
      DO I=1,IKNT
       XLAM(0)=XLAM(0)+XLAM(I)
       if(XLAM(I).LT.0.0) THEN
        print*,' IN HEXTRA '
        print*,' XLAM(I) = ',XLAM(I),I,IDLAM(I,1),IDLAM(I,2)
       ENDIF
      ENDDO
      IF(XLAM(0).eq.0.0) XLAM(0)=1.E-6
C
      RETURN
      END
      FUNCTION H2XX(c1,xm1,xm2,xm3,GL,GR)
C.....calculate the decay rate for ino -> ino + H
      implicit none
      real h2xx,xm1,xm2,xm3,gl,gr
      real xl,lamfun,c1
      real xmi2,xmj2,xmk2,xmi3
C
      xmi2=xm1**2
      xmi3=abs(xm1**3)
      xmj2=xm2**2
      xmk2=xm3**2
      XL=LAMFUN(XMI2,XMJ2,XMk2)
      H2XX=c1/4./XMI3*SQRT(XL)
     $   *((GL**2+GR**2)*(XMI2-XMJ2-XMK2)-
     $   4.*GL*GR*XM3*XM2)
      IF(H2XX.Lt.0.0) THEN
       print*,' negative width in h2xx '
       print*,XMI2,XMJ2,XMK2,GL,GR,xm1,XM2,XM3
       stop
      endif
      RETURN
      END






