      SUBROUTINE TRDYLD(STETA,GAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the X ray yield for a radiator
C                          with 'NFOIL' of material 'FOIL'
C                          regularly spaced and separated by distance
C                          'XGAP' filled with gas 'GAP' if DXGAP=0.
C                          If DXGAP >0. then use Garibian method
C      ED : ENERGY IN KEV
C      XSTEP : BIN WIDTH
C      NSTEP : NB. IF BINS
C-
C-   Inputs  : Variables in common blocks /RADDSC/ and /RADDCC/
C-   Outputs :
C-   Controls:
C-
C-   Created                A. ZYLBERSTEJN
C-   Updated  19-SEP-1988   J.R. HUBBARD/A. ZYLBERSTEJN
C-   Updated  18-JUL-1989   A. Zylberstejn  Generate X ray with irregular
C-                                          radiator 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ABSOR.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:RADDSC.INC/LIST'
      INCLUDE 'D0$INC:RADDCC.INC/LIST'
      INCLUDE 'D0$INC:TECDSC.INC/LIST'
      INCLUDE 'D0$INC:WINDSC.INC/LIST'
      INCLUDE 'D0$INC:XSPECT.INC/LIST'
      INCLUDE 'D0$INC:XRAY.INC/LIST'
C
      INTEGER IFOIS,IPRNT,I,NBI,J,NSTEPI
      REAL    EPL1,EPLMYL,EPLLIT,EPLBER,EPLCAR,EPLBOR,EPLNIT
      REAL    EPLPAE,EPL2
      REAL    EPLAIR,EPLHEL,XSTEPI,E,GAM,GAMMA,XFOI,XGP
      REAL    EPLOXY,EPLHYD
      REAL    XGS,XSKI,XMET,XSAN,XSTU,SYG,SYD,SIG1,SIG2,SIG,GD,YG
      REAL    DXF,DXG, GD1
      REAL    X1,X2,X3,X4,XXXX,YD
      REAL    TRDWDE,STETA
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
      DATA IFOIS,IPRNT/0,1/
C  DEFINE PLASMA FREQUENCY FOR DIFFERENT MATERIALS
      DATA  EPLPAE ,EPLMYL / .02153, .02443 /
      DATA  EPLCAR , EPLOXY , EPLHYD / .02696 , .7705E-3 , .2731E-3 /
      DATA  EPLLIT , EPLBER  / 0.01426 , 0.02611 /
      DATA  EPLBOR /0.03228/
      DATA  EPLAIR , EPLHEL ,EPLNIT/ 0.732E-3 ,0.2802E-3 ,.7425E-03 /
C
      IFOIS=IFOIS+1
      IF(IFOIS.GE.2)IPRNT=0
C**** INITIALISATION ********
      IF(FIRST)THEN
        FIRST=.FALSE.
        IF(FOIL.EQ.'MYL')     EPL1= EPLMYL
        IF(FOIL.EQ.'LIT')     EPL1= EPLLIT
        IF(FOIL.EQ.'BER')     EPL1= EPLBER
        IF(FOIL.EQ.'CAR')     EPL1= EPLCAR
        IF(FOIL.EQ.'BOR')     EPL1= EPLBOR
        IF(FOIL.EQ.'PAE')     EPL1= EPLPAE
        IF(GAP.EQ.'AIR')     EPL2=EPLAIR
        IF(GAP.EQ.'HEL')     EPL2=EPLHEL
        IF(GAP.EQ.'NIT')     EPL2=EPLNIT
        NSTEPI=NSTEP
        XSTEPI=XSTEP
        IF(DXGAP.GT.0.)
     +   WRITE(LOUT,*)' ************************ Irregular TRD radiator*
     &    ******************'
        WRITE(LOUT,*)NFOIL,' FOILS OF ',FOIL,' GAS IN THE GAP ',GAP
        WRITE(LOUT,*)'  XFOIL',XFOIL,' DXFOIL',DXFOIL/XFOIL
        WRITE(LOUT,*)'  XGAP ',XGAP ,' DXGAP ',DXGAP/XGAP
      END IF
C
C      PRINT 6555,GAM,STETA
      IF(GAM.LT.1000.)RETURN
 6555 FORMAT(' GAM,STETA',2G10.4)
C
      GAMMA=GAM
      XFOI=XFOIL/STETA
      XGP=XGAP/STETA
      XGS=XGAS/STETA
      XSKI=XSKIN/STETA
      XMET=XMETAL/STETA
      XSAN=XSAND/STETA
      XSTU=XSTUFF/STETA
      DXF=XFOI*DXFOIL
      DXG=XGP*DXGAP
C      PRINT*,' DXFOIL,DXGAP',DXFOIL,DXGAP,' xfoil',xfoil,' xgap',xgap
C      PRINT*,'DXFOIL,DXGAP',DXFOIL,DXGAP
      NBI=0
      SYG=0.
      SYD=0.
C      WRITE(16,3900)
 3900 FORMAT(' BIN',' ENERGY',2X,'GD',8X,'YG',8X,'YD',8X,'SYG',7X,'SYD')
      X1=0.
      X2=0.
      X3=0.
      X4=0.
      IF(DXGAP.GT.0.1)THEN  ! Prepare Garibian spectrum if dx gap >0
C        PRINT*, ' EPL1,EPL2',EPL1,EPL2,'NFOIL',NFOIL,' GAMMA',GAMMA
C        PRINT*,' DXF,DXG',DXF,DXG
        CALL SPLGAR(EPL1,EPL2,XFOI,XGP,DXF,DXG,NFOIL,GAMMA,
     &          FOIL,GAP)
      END IF
      DO 24 I=1,NSTEPI
        E=ED(I)
C  PRODUCED ENERGY DENSITY    (IN THE ENERGY BIN) :GD
C  NUMBER OF PRODUCED X RAYS  (IN THE ENERGY BIN) :YG
        IF(DXGAP.GT.0.)THEN
          GD=GDGAP(I)
        ELSE
          SIG1=XFOI*ABFOIL(I)
          SIG2=XGP *ABGAP(I)
          SIG  = SIG1 + SIG2
          IF(SIG.EQ.0.)THEN
            WRITE(LOUT,*)' XFOI ',XFOI,' XGP ',XGP,' ED',E
            WRITE(LOUT,*)'I ',I, 'SIG1,SIG2',SIG1,SIG2
            WRITE(LOUT,*)' XFOIL',XFOIL,' ABFOIL',ABFOIL(I)
            WRITE(LOUT,*)' XGAP',XGAP,' ABGAP',ABGAP(I)
            GO TO 24
          END IF
          GD = TRDWDE(E,EPL1,EPL2,XFOI,XGP,NFOIL,GAMMA,SIG)
        END IF
        YG=GD*XSTEPI/E
        SYG=SYG+YG
C*** DETECTED SPECTRUM ******
C  XXXX=ABSORPTION LENGTH IN THE ENTRANCE WINDOW
        X1=-XSKI*ABSKI(I)
        X2=-XMET*ABMET(I)
        IF(XSTUFF.GT.0.)X3=-XSTU  *ABSTUF(I)
        IF(XSAND.GT.0. )X4=-XSAN  *ABSAN(I)
        XXXX = AMAX1(X1+X2+X3+X4,-30.)
        YD=YG*EXP(XXXX)
C  THE PROBABILITY TO BE ABSORBED IN THE TEC HAS BEEN REMOVED IN THE
C  ABOVE FORMULA COMPARED WTIH PREVIOUS VERSION(23-10-86)
        SYD=SYD+YD
        XFONC(I)=YD
C  PRINTS
C      IF(MOD(I,10).NE.0)GO TO 24
C      WRITE(16,3920)I,E,GD,YG,YD,SYG,SYD
 3920   FORMAT(I4,F6.2,2X,5(G10.4,' '))
   24 CONTINUE
C
      XNOBT=SYD
      J=0
C       WRITE(LOUT,*)NSTEP
      DO 364 I=1,NSTEPI,2
        J=J+1
        XFONC(J)=XFONC(I)+XFONC(I+1)
  364 CONTINUE
      XSTEP=XSTEPI*FLOAT(NSTEPI)/FLOAT(J)
      NSTEP=J
C      IF(DTRK.EQ.1)THEN
C      WRITE(LOUT,*)' STETA',STETA,' GAMMA',GAMMA,'XNOBT',XNOBT
C      WRITE(LOUT,*)' XFONC'
C      WRITE(LOUT,*)' -----'
C      WRITE(LOUT,7764)(XFONC(IJK),IJK=1,NSTEP)
C      END IF
C
 3000 FORMAT(1H ,8X,'XSTEP=',F8.6,' ED(1)=',F8.6,' E(NSTEP)=',F10.6)
 3001 FORMAT(1H ,8X,18('-@@@-'))
 3002 FORMAT(1H ,8X,'    MITTLERE ERZEUGTE ENERGIE = ',4(G13.5,4X))
 3003 FORMAT(1H ,8X,' MITTL. NACHGEWIESENE ENERGIE : ',4(D13.8,4X))
 3200 FORMAT(1H ,13X,'ED',13X,'GD',13X,'EM',13X,'EN',13X,
     1  'EG',9X,'EMEAN1',/)
 3201 FORMAT(1H ,13X,'ED',13X,'WD',13X,'WM',13X,'WN',13X,
     1  'WG',12X,'WMEAN',/)
 3210 FORMAT (1H ,(7X,6(4X,G11.5)))
 4001 FORMAT(1H ,/,9X,' CHAMBRE : ',F6.3,'  ET ',F6.3,' CM',1X,A3,
     1  9X,'FENETRE D ENTREE',F8.4,'CM',1X,A3,
     2  /,9X,F8.5,'CM   ALU  ',
     3  ' PANNEAU',1X,F8.5,1X,A3,' + ',
     4  F6.4,' CM D AIR')
 5020 FORMAT(1H ,'CLUSTERS DANS LA ZONE D AMPLIFICATION')
 6863 FORMAT(5G12.5)
 6866 FORMAT(10F8.2)
 6767 FORMAT(' STACK ',I2,' NB. DE X: ZONE DE DERIVE:',F4.1,' ZONE AMPLI
     +CATION:',F4.1)
 7764 FORMAT(10G12.4)
      RETURN
      END
