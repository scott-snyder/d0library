      FUNCTION DWDEGR(Z1,Z2,Z3,Z4,Z5,Z6,Z7,NFOIL,Z8,MATF,MATG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Calculate energy spectrum radiated by an irregular radiator
C-      according to Garibian NIM 125 (1975) 133
C-      ***   This tuning of integration step only valid for
C-      ***   delta(gap) ge gap/2  and delta(foil) ge foil/10
C
C-   Inputs  :
C     Z1  energy in KeV
C     Z2  plasma frequency of foil in KeV
C     Z3     "      "          gap   "
C     Z4  average thickness of foil (cm)
C     Z5     "      "          gap   "
C     Z6  sigma of foil thickness distribution (cm)
C     Z7     "     gap     "         "
C     NFOIL   number of radiator foils
C     Z8  gamma of incident particle
C     MATF,MATG  material id. of foil and gap as in TRDFMY
C
C-    Output : DWDEGR energy radiated in KeV in a one KeV bin
C              around Z1
C-
C-   Created   7-OCT-1987   B. MANSOULIE
C-   Updated  12-JUL-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
C
C        all real variables in double precision but those beginning
C        by Z which are used as single precision arguments,and single
C        precision function names.
C
      IMPLICIT NONE
      REAL*8 AGA,ARG,CSI1,CSI1SQ,CSI2,CSI2M1,CSI2SQ,DD1,DD2,DDWDE,DNDE
      REAL*8 DWDE1,DWDE2,DWDE3,DX1,DX2
      REAL*8 E0,EBIN,FF,FF1,GAM2,GAM2I,GAMMA,HBARC
      REAL*8 OMSUR2,OMSURC,PABNL,SIG,SIG1,SIG2,TH2,TH20,TH2M1,TH2M2
      REAL*8 TH2NEW,TH2OLD,TH2ST,TH2STA,TH2STB,THETA
      REAL*8 xinter,X2,XNEFF,PI137
      REAL Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8
      INTEGER I,I1,I3,ITH,LUNOUT,NFOIL
      CHARACTER*3 MATF,MATG
      REAL*4 DWDEGR,TRDFMY
      REAL WV(2),WP(2),AMU(2)
      LOGICAL FIRST
C
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:GRUTIL.INC'
C
C      HBARC IN KEV.CM
      DATA HBARC/1.973286D-8/
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        DPI=4.D0*ATAN(1.D0)
        DTWOPI=2.D0*DPI
        PI137=1./(DPI*137.036)
        FIRST=.FALSE.
      END IF
C
      EBIN=Z1
      WP(1)=Z2
      WP(2)=Z3
      D(1)=Z4
      D(2)=Z5
      DX1=Z6
      DX2=Z7
      GAMMA=Z8
C
C         This tuning of integration step only valid for
C         delta(gap) ge gap/2  and delta(foil) ge foil/10
C
      DD1=D(1)*.09D0
      DD2=D(2)*.4D0
C      WRITE(LUNOUT,*) ' DD1,DD2 ',DD1,DD2
      IF(DX1.LT.DD1.OR.DX2.LT.DD2) THEN
        WRITE (LUNOUT,1000) DX2,DX1
 1000   FORMAT(///,'  DWDEGR ,DELTA GAP ',G12.3,'  OR DELTA FOIL ',
     >  G12.3,' TOO SMALL')
        STOP
      ENDIF
C
      OMSURC=EBIN/HBARC
      OMSUR2=OMSURC/2.D0
C
      AMU(1)=TRDFMY(EBIN,MATF)
      AMU(2)=TRDFMY(EBIN,MATG)
C
      SIG1=D(1)*AMU(1)
      SIG2=D(2)*AMU(2)
      SIG=SIG1+SIG2
C
      XFOIL=NFOIL
      GAM2=GAMMA**2
      GAM2I=1.D0/GAM2
      CSI1=WP(1)/EBIN
      CSI2=WP(2)/EBIN
      CSI1SQ=CSI1**2
      CSI2SQ=CSI2**2
      CSI2M1=CSI1SQ-CSI2SQ
C
      WV(1)=CSI1SQ
      WV(2)=CSI2SQ
      AG(1)=(D(1)/DX1)**2
      AG(2)=(D(2)/DX2)**2
C
      DO 2 I=1,2
        AGA=AG(I)
        AMLIM(I)=EXP(-AMU(I)*D(I)/2.D0)
        C1MA(I)=(1.D0+AMU(I)*D(I)/(2.D0*AGA))**2
        C2MA(I)=D(I)/AGA
        CPSI(I)=D(I)/(AGA+AMU(I)*D(I)/2.D0)
        ARG=AMU(I)*D(I)
        IF(AGA.LT.100.) THEN
          PA(I)=(1.D0+ARG/AGA)**(-AGA)
        ELSE
          PA(I)=EXP(-ARG)
        ENDIF
        TT(I)=GAM2I+WV(I)
    2 CONTINUE
      PAB=PA(1)*PA(2)
      PABNL=XFOIL*LOG(PAB)
      PABN=0.
      IF(PABNL.GT.-100.) PABN=EXP(PABNL)
      ANE=XFOIL
      IF(PAB.NE.1.D0) ANE=(1.D0-PABN)/(1.D0-PAB)
      PAB1H=(1.D0+PAB)/2.D0
C
      DWDE1=0.D0
      TH2STA=(GAM2I+CSI2SQ)/5.D0
      TH2STB=2.D0*DTWOPI/((D(1)+D(2))*OMSURC*3.D0*XFOIL)
C      TH2STB=4.D0/((D(1)+D(2))*OMSURC*XFOIL)
      TH2ST=MIN(TH2STA,TH2STB)
      TH2M1=5.D0*(GAM2I+CSI2SQ)
      I1=TH2M1/TH2ST
C      WRITE(LUNOUT,*)'  TH2ST ',TH2ST,'  NBRE DE PAS 1 ',I1
C
C        FIRST LOOP ON THETA
C

      DO 9  ITH=1,I1
        TH2=TH2ST*ITH
C
        X2=GAM2I+TH2
        E0=CSI2M1/((X2+CSI1SQ)*(X2+CSI2SQ))
C
C   Compute interference term following Garibian
C
        FF=XINTER(OMSUR2,TH2)
        FF1=E0*E0*TH2
        DDWDE=FF1*FF
C
        DWDE1=DWDE1+DDWDE
 3333   FORMAT(1X,G12.4)
    9 CONTINUE
      DWDE1=DWDE1*TH2ST
      DWDE1=DWDE1*PI137
C      WRITE(LUNOUT,*)'   FIN 1E BOUCLE , DWDE1  ',DWDE1
C
C       START 2ND LOOP
C
      DWDE2=0.D0
      TH2M2=5.D0*(GAM2I+CSI1SQ)
C      TH2ST=1.D0*TH2ST
C ***********
C-      ***   This tuning of integration step only valid for
C-      ***   delta(gap) ge gap/2  and delta(foil) ge foil/10
      TH2ST=20.*TH2ST
C ***********
      I1=(TH2M2-TH2M1)/TH2ST
C      WRITE(LUNOUT,*)'  TH2ST ',TH2ST,'  NBRE DE PAS 2 ',I1
C
      DO 91  ITH=1,I1
        TH2=TH2ST*ITH+TH2M1
C
        X2=GAM2I+TH2
        E0=CSI2M1/((X2+CSI1SQ)*(X2+CSI2SQ))
C
C      CALCUL SELON GARIBIAN DU TERME D INTERFERENCE
C
C
        FF=XINTER(OMSUR2,TH2)
        FF1=E0*E0*TH2
        DDWDE=FF1*FF
C
        DWDE2=DWDE2+DDWDE
C
   91 CONTINUE
      DWDE2=DWDE2*TH2ST
      DWDE2=DWDE2*PI137
C
      TH20=TH2
      I3=SQRT(3.D-3/TH2ST)
      TH2OLD=TH20
      DWDE3=0.D0
      DO 15 ITH=1,I3
        TH2NEW=TH20+TH2ST*ITH*ITH
C  CHOISIT LE PT DE CALCUL EN ASSUMANT DEPENDANCE EN 1/TH2**3
        TH2=2.D0*TH2OLD*TH2NEW/(TH2OLD+TH2NEW)
        X2=GAM2I+TH2
        E0=CSI2M1/((X2+CSI1SQ)*(X2+CSI2SQ))
C
        DDWDE=E0*E0*TH2
        DWDE3=DWDE3+DDWDE*(TH2NEW-TH2OLD)
C      IF(MOD(ITH,1000).EQ.1) WRITE(LUNOUT,*)' THETA STEP',ITH,
C     >' DWDE3 ',DWDE3
        TH2OLD=TH2NEW
   15 CONTINUE
      XNEFF=XFOIL
      IF(SIG.NE.0.D0)XNEFF=(1.-EXP(-NFOIL*SIG))/SIG
C      WRITE(LUNOUT,*)'  NEFF ',XNEFF
      DWDE3=2.*DWDE3*XNEFF
      DWDE3=DWDE3*PI137
      DWDEGR=DWDE1+DWDE2+DWDE3
      DNDE=DWDEGR/EBIN
      THETA=SQRT(TH2)
C      WRITE(LUNOUT,*)'   THETA FINAL ',THETA
C      WRITE(LUNOUT,1010)EBIN,DWDEGR,DNDE
 1010 FORMAT('   ENERGIE ',G10.4,' DWDEGR ',G10.4,
     >' KEV/KEV   DNDE ',G10.4,' PHOTON/KEV')
      RETURN
      END
