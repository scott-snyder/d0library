      FUNCTION DIMUON()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reconstruct decays X  ->mu+mu-;
C-   now uses MUON banks
C-
C-   Inputs  : PMUO banks
C-   Outputs :
C-   Controls:
C-
C-   Created   8-NOV-1991   Daria Zieminska
C-   Updated  24-MAR-1992   C.R.Murphy - modified manner in which 
C-   ISAJET muons can be introduced into fitting subroutines.
C-   Updated  20-APR-1993   Daria Zieminska   add condition on PMUO quality
C-   Updated   3-JUN-1993   Daria Zieminska  reove dependence on CD match
C-   Updated   3-JUN-1993   Herb Greenlee fixes for transportability
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DIMUON
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
      INCLUDE 'D0$LINKS:IZMUON.LINK'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
      INCLUDE 'D0$LINKS:IZPVES.LINK'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INTEGER NUMEVT
      REAL HISDAT
      INTEGER NR,LPDIL,IER,ICALL,ISETVN,LPARH,GZPARH,LPMUO,GZPMUO
      INTEGER NPDIL,I1,I2,GZPDIL,LTRAK,PRUNIT,USUNIT
      INTEGER LMTRH,GZMTRH,LMUOT,GZMUOT,LMUON,GZMUON,LPVES
      INTEGER NZBANK,NMUON,NMUOT,NPMUO,NISAL,NUMMUONS,NVEE
      INTEGER NV,K,K1,K2,K3,IEND,I
      CHARACTER*8 NAME,LABEL
      INTEGER L1,L2,LBIT
      REAL PRB,CHI
      INTEGER LVTX1,LVTX2,LCDC1,LCDC2,LFDC1,LFDC2
      INTEGER ISTAT1(4),LZTRK1,LZTRK2,ISTAT2(4)
      INTEGER LEPTONS,DILBOSON_ID
      REAL CONE,MASS_LO,MASS_HI,PT_MIN1,PT_MIN2,PT_MIN
      REAL THETA,ETA,PHI,MOM,EEM(20),EHAD(20),ETOT(20),DRSQPSIKS
      REAL PHI1,PHI2,THE1,THE2,DELPHI_MUMU
      REAL EPHI1,EPHI2,ETHE1,ETHE2,DELPHI,DELPHIMX
      REAL P1,P2,PT1,PT2,MUON,PDG_MASS,MASS
      REAL PTLEPMAX,PTLEPMIN,AMAX1,AMIN1
      REAL PHI0,DPHI0,THE0,DTHE0
      REAL E1,E2,P1COMP(3),P2COMP(3)
      REAL DMOM,DPHI,DTHE,PTK,PTMINK,PHIK,ETAK,DR,DRSQ,DRMAX,PROB,LENGTH
      INTEGER IPSIKS,BEST
      REAL MPSIKS(5),EHADNEAR,EHADMIN,BESTPROB
      INTEGER SI
      INTEGER ID1,ID2
      INTEGER ITRACK1,ITRACK2,IDET1,IDET2
      REAL BIMP1,BIMP2,ZIMPACT
      INTEGER RUN,ID,RUNSAV,IDSAV
      SAVE RUNSAV,IDSAV
      DATA RUNSAV,IDSAV/-1,-1/
      DATA PTMINK,DRMAX/1.,1.0/
      LOGICAL THETA1_VTX,THETA2_VTX,FOUNDMUS,OK,MATCH
      LOGICAL DO_KIN_FIT,DO_KSHORT,MCDATA,FAKEMU
      DATA EHADMIN/4./
      DATA NUMEVT/0/
      SAVE ICALL
      DATA ICALL/0/
C------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('DILBOSON_RCP')
        CALL EZGET('DILBOSON_ID',DILBOSON_ID,IER)
        CALL EZGET('LEPTONS',LEPTONS,IER)
        CALL EZGET('PT_MIN1',PT_MIN1,IER)
        CALL EZGET('PT_MIN2',PT_MIN2,IER)
        CALL EZGET('PT_MIN',PT_MIN,IER)
        CALL EZGET('MASS_LO',MASS_LO,IER)
        CALL EZGET('MASS_HI',MASS_HI,IER)
        CALL EZGET('DO_KIN_FIT',DO_KIN_FIT,IER)
        CALL EZGET('DO_KSHORT',DO_KSHORT,IER)
        CALL EZGET('MCDATA',MCDATA,IER)
        CALL EZGET('FAKEMU',FAKEMU,IER)
        CALL EZRSET
        MUON=0.105658
        IF (DILBOSON_ID.EQ.441) PDG_MASS=3.09693
        IF (DILBOSON_ID.EQ.90)  PDG_MASS=91.17
        ICALL=1
        PRUNIT=USUNIT()
      END IF
C
      CALL EVNTID(RUN,ID)
      NUMEVT=NUMEVT+1
      DIMUON=.FALSE.
      IF (LEPTONS.NE.14) GO TO 1000
      IF (FAKEMU .EQ. .TRUE.) THEN
        CALL DIMUON_FAKE(NISAL)
        NUMMUONS=NISAL
        GO TO 500
      ENDIF
      LMTRH=GZMTRH()
      IF (LMTRH.EQ.0) GO TO 1000
      LMUOT=LQ(LMTRH-IZMUOT)
      LMUON=LQ(LMTRH-IZMUON)
      LPARH=GZPARH()
      LPMUO=LQ(LPARH-IZPMUO)
      NPMUO=NZBANK(IXCOM,LPMUO)
      NMUOT=NZBANK(IXCOM,LMUOT)
      NMUON=NZBANK(IXCOM,LMUON)
      NUMMUONS=NPMUO
  500 IF (NUMMUONS.LT.2) GO TO 1000
C
C  Double loop over PMUO banks
C
      CALL VZERO(MPSIKS,5)
      IPSIKS=0
      DO 100 I1=1,NUMMUONS-1
C
        DO 200 I2=I1+1,NUMMUONS
C
          IF (FAKEMU.EQ..TRUE.) THEN
            CALL FAKEMU1(I1,I2,FOUNDMUS,OK,P1,P2,THE1,THE2,PHI1,PHI2)
            IF (FOUNDMUS) THEN
              GO TO 600
            ELSE
              GO TO 200
            ENDIF
          ENDIF  
C
          L1=GZPMUO(I1)
          L2=GZPMUO(I2)
          ID1=IQ(L1+2)
          ID2=IQ(L2+2)
          IF (ID1*ID2.GT.0) GO TO 200
C
          P1=Q(L1+13)
          PT1=Q(L1+14)
          P2=Q(L2+13)
          PT2=Q(L2+14)
          PTLEPMAX=AMAX1(PT1,PT2)
          PTLEPMIN=AMIN1(PT1,PT2)
          IF (PTLEPMAX.LT.PT_MIN1.OR.PTLEPMIN.LT.PT_MIN2) GO TO 200
          PHI1=Q(L1+17)
          IF (PHI1.LT.0.) PHI1=PHI1+TWOPI
          EPHI1=0.01
          PHI2=Q(L2+17)
          IF (PHI2.LT.0.) PHI2=PHI2+TWOPI
          EPHI2=0.01
          THE1=Q(L1+15)
          ETHE1=0.001
          THE2=Q(L2+15)
          ETHE2=0.001
          CALL VZERO(STR,40)
          CALL VZERO(ETR,40)
          STR(1,1)=P1
          STR(1,2)=P2
          STR(4,1)=MUON
          STR(4,2)=MUON
          STR(4,3)=PDG_MASS
          STR(2,1)=THE1
          STR(3,1)=PHI1
          STR(2,2)=THE2
          STR(3,2)=PHI2
          ETR(1,1)=SQRT(Q(L1+21))
          ETR(1,2)=SQRT(Q(L2+21))
          ETR(2,1)=ETHE1
          ETR(3,1)=EPHI1
          ETR(2,2)=ETHE2
          ETR(3,2)=EPHI2
C
  600     IF (FAKEMU.EQ..TRUE.) THEN
            CALL FAKEMU2(I1,I2,FOUNDMUS,OK,P1,P2,THE1,THE2,PHI1,PHI2)
            IF (.NOT.OK) GO TO 200
          ENDIF  
          NTR=3
          NR=2
          IND(1)=1
          IND(2)=1
          IND(3)=6     
          CALL STEP1_MASS(MASS)
          CALL DILBOSON_HIS(1,MASS,1.)
          DELPHI_MUMU=ABS(PHI2-PHI1)
          IF (DELPHI_MUMU.GT.PI) DELPHI_MUMU=TWOPI-DELPHI_MUMU
          HISDAT=(180./PI)*DELPHI_MUMU
          CALL DILBOSON_HIS(2,HISDAT,1.)
C
          IF (MASS.LT.MASS_LO.OR.MASS.GT.MASS_HI) GO TO 200
C
C  Call the kinematic fitting.
C
          IEND=0
          IF (DO_KIN_FIT.EQ..TRUE.) CALL VKIN_FIT(2,IEND,PRB,CHI)
          IF (IEND.LT.0) GO TO 200
          CALL DILBOSON_HIS(10,PRB,1.)
          CALL DILBOSON_HIS(11,CHI,1.)
          IF (PRB.LT.0.02) GO TO 200
          DIMUON=.TRUE.
C
C  fit successful
C
C
C  compare with ISAJET if appropriate
C
          IF (MCDATA) THEN
            DO 400 I=1,2
              CALL DILBOSON_MC(I,DMOM,DPHI,DTHE,MATCH)
              IF (.NOT.MATCH) GO TO 400
              CALL DILBOSON_HIS(12,DMOM,1.)
              CALL DILBOSON_HIS(13,DPHI,1.)
              CALL DILBOSON_HIS(14,DTHE,1.)
  400       CONTINUE
          ENDIF
C
C  Find impact parameters of leptons
C
          IF (FAKEMU) GO TO 700
          LZTRK1=LQ(L1-5)                    
          IF (LZTRK1.GT.0) THEN
            BIMP1=ZIMPACT(LZTRK1,ITRACK1,IDET1)
            IF (IDET1.EQ.1) CALL DILBOSON_HIS(8,BIMP1,1.)
          END IF
          LZTRK2=LQ(L2-5)
          IF (LZTRK2.GT.0) THEN
            BIMP2=ZIMPACT(LZTRK2,ITRACK2,IDET2)
            IF (IDET2.EQ.1) CALL DILBOSON_HIS(8,BIMP2,1.)  
          END IF
C
C
C  Get Calorimeter energy in a cone around the J/psi
C
  700     CONTINUE
          MOM=STR(1,3)
          THETA=STR(2,3)
          ETA=-ALOG(TAN(THETA/2.))
          PHI=STR(3,3)
          IF (PHI.LT.0.) PHI=PHI+TWOPI
          CALL DILBOSON_HIS(3,MOM,1.)
          HISDAT=MOM*SIN(THETA)
          CALL DILBOSON_HIS(4,HISDAT,1.)
          CALL DILBOSON_HIS(5,ETA,1.)
C
          CALL CAL_ECONES(ETA,PHI,EEM,ETOT,IER)
          IF (IER.NE.1) THEN
            DO I=1,20
              DRSQ=FLOAT(2*I-1)*0.5/2.
              EHAD(I)=ETOT(I)-EEM(I)
              CALL DILBOSON_HIS(6,DRSQ,EHAD(I))
              CALL DILBOSON_HIS(7,DRSQ,ETOT(I))
            ENDDO
          ENDIF
C
          LPARH=GZPARH()
          NPDIL=IQ(LPARH+8)
          IF (NPDIL.GT.0) THEN
            LPDIL=GZPDIL(NPDIL)
            IQ(LPDIL+2)=DILBOSON_ID
            IQ(LPDIL+3)=LEPTONS
            Q(LPDIL+7)=PDG_MASS
            Q(LPDIL+9)=PT_MIN2
            Q(LPDIL+10)=PT_MIN
            Q(LPDIL+8)=PT_MIN1
            DO I=1,5
              Q(LPDIL+10+I)=ETOT(I)
            ENDDO
          END IF
C
C  search for Ks
C
          IF (DO_KSHORT.EQ..FALSE.) GO TO 300
C          EHADNEAR=ETOT(1)+ETOT(2)-EEM(1)-EEM(2)
          EHADNEAR=EHAD(1)+EHAD(2)-EHAD(3)-EHAD(4) 
          IF (EHADNEAR.LT.EHADMIN) GO TO 300
          PHI0=PHI
          DPHI0=3.1416
          THE0=THETA
          DTHE0=3.0
          CALL VEES_CONE(PHI0,DPHI0,THE0,DTHE0)  
C
          IF (MCDATA) THEN
            CALL VTXISA
          ENDIF  
C
C Calculate invariant mass of J/Psi + Ks
C
          STR(1,1)=MOM
          STR(2,1)=THETA
          STR(3,1)=PHI
          STR(4,1)=PDG_MASS
          LPARH=GZPARH()
          IF(LPARH.NE.0) THEN
            LPVES=LQ(LPARH-IZPVES)
          ELSE
            LPVES=0
          ENDIF
          BESTPROB=0.
          BEST=0
    1     IF(LPVES.GT.0) THEN     ! loop over Ks, cut on prob and length
            PROB=Q(LPVES+27)
            LENGTH=Q(LPVES+25)
            IF (PROB.LT.0.05.OR.LENGTH.GT.20.) THEN
              GO TO 800
            END IF
            STR(1,2)=Q(LPVES+19)  ! P Ks
            STR(2,2)=Q(LPVES+21)  ! THETA Ks
            STR(3,2)=Q(LPVES+23)  ! PHI Ks
            PTK=STR(1,2)*SIN(STR(2,2))
            IF (PTK.LT.PTMINK) THEN
              GO TO 800
            END IF
            ETAK=-ALOG(TAN(STR(2,2)/2.))
            PHIK=STR(3,2)
            DR=ABS(PHIK-PHI)
            IF (DR.GT.PI) DR=TWOPI-DR
            DELPHI=DR
            DRSQPSIKS=DR**2+(ETA-ETAK)**2
            CALL DILBOSON_HIS(15,DELPHI,1.)
            CALL DILBOSON_HIS(16,DRSQPSIKS,1.)
            DR=SQRT(DRSQPSIKS)
            IF (DR.GT.DRMAX) THEN
              GO TO 800
            END IF
            STR(4,2)=0.497
            IPSIKS=IPSIKS+1
            IF (PROB.GT.BESTPROB) THEN
              BESTPROB=PROB
              BEST=IPSIKS
              NVEE=IQ(LPVES-5)
            END IF
            CALL STEP1_MASS(MPSIKS(IPSIKS))
            IF (MPSIKS(IPSIKS).GT.0.) THEN
C
C              WRITE(0,101) RUN,ID,MASS,EHADNEAR,
C     1        MPSIKS(BEST),PTK,DR,PROB,LENGTH
  101         FORMAT(///,' EVENT',2I8,5F8.1,F9.2,F8.1)
C
            ENDIF  
  800       CONTINUE
            LPVES=LQ(LPVES)               ! pointer to next bank
            IF (IPSIKS.LT.5) GOTO 1
          ENDIF
C
          IF (BEST.GT.0.AND.MPSIKS(BEST).GT.0.) THEN
C
C              WRITE(0,102) NVEE,MPSIKS(BEST)
  102     FORMAT(' best mass',I5,F8.1) 
C
            CALL PRPVES(0,0,0,'ALL',0)
            CALL DILBOSON_HIS(9,MPSIKS(BEST),1.)
          END IF
C
  300     CONTINUE
C
  200   CONTINUE
  100 CONTINUE
C
C              IF (MCDATA) THEN
                CALL VEEHIS
C              ENDIF
 1000 RETURN
      END
