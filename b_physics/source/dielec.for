      FUNCTION DIELEC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : store NTUPLEs for upsilon  -> e+ e-
C-
C-   Inputs  : various DST banks 
C-   Controls:
C-
C-   Created 03-Apr-1993   Andrzej Zieminski
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DIELEC, EL2OK, MCDATA, FOUND
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INTEGER LPPHO,GZPPHO,LPARH,GZPARH,LPELC,GZPELC
C
      INTEGER IBITS,ISTAT1(4),ISTAT2(4)
C
C-  Counters
      INTEGER NTOT,N2EL,NACC,NPAIR
      INTEGER NZBANK,N1,NPELC,NELCS,NPPHO,NMUON,NJETS
C
C-  Pointers, indices
      INTEGER LZTRK1,LZTRK2,LVTX1,LVTX2,LCDC1,LCDC2,LFDC1,LFDC2
      INTEGER I1,I2, ID1, ID2, IDL1,IDL2
      INTEGER NR,IER,IEND,I,J, L1,L2,LBIT
      INTEGER KEE,KMU,KJT,KE1,KE2, KF1,KF2
      LOGICAL THETA1_VTX,THETA2_VTX,OK
C
C-  Electron/photon kinematic variables
      REAL P1,P2,PT1,PT2,PHI1,PHI2,THE1,THE2
      REAL MCMASS,MCE1,MCE2,MCPHI1,MCPHI2,MCTHE1,MCTHE2
      REAL EP1,EP2,EPHI1,EPHI2,ETHE1,ETHE2 
      REAL EM1,EM2,EM3,EM4,FH1, R1,R2
      REAL PCL1(8),PCL2(8)
      real xng1,yng1,zng1,xng2,yng2,zng2
      real xbar(3)
      real scexp
      integer LCASH,LCACL
C
C-  Di-electron variables
      REAL ELEC,PDG_MASS,MASS,OPANG 
      REAL PTMAX,PTMIN,AMAX1,AMIN1
      REAL PRB,CHI,P4PSI(4)
C
C-  Ntuple variables
      CHARACTER*8 TOP_DIRECTORY
      INTEGER LEN, LOC, IDN, NCVAR, NTVAR, STATUS, NT_ID
      PARAMETER (LEN=220)
      REAL QHIS(LEN)
C
C-  DILBOSON_RCP variables
      LOGICAL CUTOEE,CUTMSS,CUTPTE
      LOGICAL FIRST, DO_KIN_FIT 
      INTEGER LEPTONS,DILBOSON_ID
      REAL PTECUT,PTXCUT,MSSCUT
      REAL MASS_LO,MASS_HI 
C
      DATA TOP_DIRECTORY/'DILBOSON'/
      DATA FIRST/.TRUE./ 
      DATA KEE,KE1,KE2,KMU,KJT/15,40,110,180,190/
C------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('DILBOSON_RCP')
        CALL EZGET('DILBOSON_ID',DILBOSON_ID,IER)
        CALL EZGET('LEPTONS',LEPTONS,IER)
        CALL EZGET('CUTOEE',CUTOEE,IER)
        CALL EZGET('CUTPTE',CUTPTE,IER)
        CALL EZGET('CUTMSS',CUTMSS,IER)
        CALL EZGET('PTECUT',PTECUT,IER)
        CALL EZGET('PTXCUT',PTXCUT,IER)
        CALL EZGET('MSSCUT',MSSCUT,IER)
        CALL EZGET('MASS_LO',MASS_LO,IER)
        CALL EZGET('MASS_HI',MASS_HI,IER)
        CALL EZGET('MCDATA',MCDATA,IER)
        CALL EZGET('DO_KIN_FIT',DO_KIN_FIT,IER)
        CALL EZRSET
C
        NTOT = 0
        N2EL = 1
        NACC = 0
C
        ELEC=0.000511
        IF (DILBOSON_ID.EQ.441) PDG_MASS=3.097
        if (dilboson_id.eq.551) pdg_mass=9.46032
        IF (DILBOSON_ID.EQ.90)  PDG_MASS=91.17
        FIRST=.FALSE.
C
C-- Book ntuple
        CALL EL2_NT_BOOK(IDN) 
      ENDIF
C
C-  FoR every event
      NTOT = NTOT + 1
      DIELEC=.FALSE.
      IF(LEPTONS.NE.12) GO TO 999
C
C-  Check for di-electrons in the event header
      CALL EL2_STRIP(EL2OK) 
      IF(.NOT.EL2OK) GOTO 999
C
C-  Unpack electron banks and double check
      LPARH=GZPARH()
      LPELC=LQ(LPARH-IZPELC)
      NPELC=NZBANK(IXCOM,LPELC)
      LPPHO=LQ(LPARH-IZPPHO)
      NPPHO=NZBANK(IXCOM,LPPHO)
      NELCS=NPELC+NPPHO
      IF (NELCS.LT.2) GO TO 999
      IF (NPELC.LT.1) GO TO 999
      IF (CUTOEE.AND.NPELC.LT.2) GO TO 999
C
C-  Get information about the whole event
      CALL VZERO(QHIS,LEN) 
      LOC=0
      CALL EL2_HD_FILL(QHIS)
      QHIS(4)=FLOAT(NPELC)
      QHIS(5)=FLOAT(NPPHO)
      CALL EL2_MU_FILL(NMUON,QHIS(KMU+1))
      QHIS(7)=FLOAT(NMUON)
      CALL EL2_JNEP_FILL(NJETS,QHIS(KJT+1))
      QHIS(8)=FLOAT(NJETS)
C
C-  Start loop over first electron (only loop over PELC banks)
      NPAIR = 0
      N1 = NPELC
      IF(CUTOEE.OR.NPPHO.EQ.0) N1=NPELC-1
      DO 100 I1=1,N1 
        IDL1=0
        LPELC=GZPELC()
        DO I=1,I1
          IF (I.EQ.1) THEN
            L1=LPELC
          ELSE
            L1=LQ(L1)
          ENDIF
        ENDDO
C
        ID1=IQ(L1+2)
        PT1=Q(L1+7)
        IF(CUTPTE.AND.PT1.LE.PTECUT) GOTO 100
        P1=Q(L1+6)
        EP1=SQRT(abs(Q(L1+11))+abs(Q(L1+12)))*P1/PT1
        THE1=Q(L1+8)
        PHI1=Q(L1+10)
        IF (PHI1.LT.0.) PHI1=PHI1+TWOPI
        ETHE1=0.004
        EPHI1=0.004
C
        LZTRK1=LQ(L1-3)                    ! Location of the 1-st track bank
        IF (LZTRK1.LE.0) GO TO 100         ! Bank doesn't exist

C--  Find parameters of 1-st track in r-phi

          LVTX1=LQ(LZTRK1-6)          ! Ref. link to VTX track
          LCDC1=LQ(LZTRK1-7)          ! Ref. link to CDC track
          LFDC1=LQ(LZTRK1-8)          ! Ref. link to FDC track
          IF (LVTX1.GT.0) THEN  ! use the VTX component if available
            PHI1=Q(LVTX1+6)
            IF (PHI1.LT.0.) PHI1=PHI1+TWOPI
            EPHI1=Q(LVTX1+16)
          ELSE IF(LCDC1.GT.0) THEN
            PHI1=Q(LCDC1+6)
            IF (PHI1.LT.0.) PHI1=PHI1+TWOPI
            EPHI1=Q(LCDC1+16)
          ELSE IF(LFDC1.GT.0) THEN
            PHI1=Q(LFDC1+6)
            IF (PHI1.LT.0.) PHI1=PHI1+TWOPI
            EPHI1=Q(LFDC1+23)
          ELSE
            GO TO 102
          END IF

C--  Find parameters of 1-st track in theta

          DO LBIT=9,12
            ISTAT1(LBIT-8)=IBITS(IQ(LZTRK1),LBIT,1)
          ENDDO
          THETA1_VTX=LVTX1.GT.0.AND.LCDC1.EQ.0.AND.LFDC1.EQ.0
          THETA1_VTX=THETA1_VTX.OR.ISTAT1(1).EQ.1
          IF (THETA1_VTX) THEN
            THE1=Q(LVTX1+9)
            ETHE1=Q(LVTX1+18)
          ELSE
            IF(LCDC1.GT.0) THEN
              THE1=Q(LCDC1+9)
              ETHE1=Q(LCDC1+18)
            END IF
            IF(LFDC1.GT.0) THEN
              THE1=Q(LFDC1+22)
              ETHE1=Q(LFDC1+24)
            END IF
          END IF
C
C-  Find 42 standard parameters for this electron, fill QHIS
 102      CONTINUE
          CALL EL2_EL_FILL(L1,IDL1,QHIS(KE1+6),PCL1)
          CALL ENERGY_CASH(L1,EM1,EM2,EM3,EM4,FH1)
          QHIS(KE1+56)=EM1
          QHIS(KE1+57)=EM2
          QHIS(KE1+58)=EM3
          QHIS(KE1+59)=EM4
          QHIS(KE1+60)=FH1
C
C-  Fill the rest of this electron part of the NTUPLE
          LOC=KE1
          QHIS(LOC+1)=P1
          QHIS(LOC+2)=P1*SIN(THE1)
          QHIS(LOC+3)=THE1 
          IF(THE1.GT.0.) QHIS(LOC+4)=-ALOG(TAN(THE1/2.))
          QHIS(LOC+5)=PHI1 
C
C-- Start loop over second electron
C
        DO 200 I2=I1+1,NELCS
          IDL2=0
          IF(I2.GT.NPELC) THEN
            IF(CUTOEE) GOTO 200
            IDL2=1
          ENDIF
C
          J=I2-I1
          DO I=1,J
            IF (I.EQ.1) THEN
              IF(I1+1.EQ.(NPELC+1)) THEN
                L2=GZPPHO()
              ELSE
                L2=LQ(L1)
              ENDIF
            ELSE
              IF(I1+I.EQ.(NPELC+1)) THEN
                L2=GZPPHO()
              ELSE
                L2=LQ(L2)
              ENDIF
            ENDIF
          ENDDO
C
          PT2=Q(L2+7)
          IF(CUTPTE.AND.PT2.LT.PTECUT) GOTO 200
C
          ID2=IQ(L2+2)
          P2=Q(L2+6)
          EP2=SQRT(abs(Q(L2+11))+abs(Q(L2+12)))*P2/PT2
          THE2=Q(L2+8)
          ETHE2=0.004
          PHI2=Q(L2+10)
          IF (PHI2.LT.0.) PHI2=PHI2+TWOPI
          EPHI2=0.004
C
C  Find parameters of 2-nd track in r-phi
  201     IF(IDL2.GT.0) GOTO 202
          LZTRK2=LQ(L2-3)
          IF (LZTRK2.LE.0) GO TO 202
          LVTX2=LQ(LZTRK2-6)
          LCDC2=LQ(LZTRK2-7)
          LFDC2=LQ(LZTRK2-8)
          IF (LVTX2.GT.0) THEN
            PHI2=Q(LVTX2+6)
            IF (PHI2.LT.0.) PHI2=PHI2+TWOPI
            EPHI2=Q(LVTX2+16)
          ELSE IF(LCDC2.GT.0) THEN
            PHI2=Q(LCDC2+6)
            IF (PHI2.LT.0.) PHI2=PHI2+TWOPI
            EPHI2=Q(LCDC2+16)
          ELSE IF(LFDC2.GT.0) THEN
            PHI2=Q(LFDC2+6)
            IF (PHI2.LT.0.) PHI2=PHI2+TWOPI
            EPHI2=Q(LFDC2+23)
          ELSE
            GO TO 202
          END IF
C
C  Find parameters of 2-nd track in theta
C
          DO LBIT=9,12
            ISTAT2(LBIT-8)=IBITS(IQ(LZTRK2),LBIT,1)
          ENDDO
          THETA2_VTX=LVTX2.GT.0.AND.LCDC2.EQ.0.AND.LFDC2.EQ.0
          THETA2_VTX=THETA2_VTX.OR.ISTAT2(1).EQ.1
          IF (THETA2_VTX) THEN
            THE2=Q(LVTX2+9)
            ETHE2=Q(LVTX2+18)
          ELSE
            IF(LCDC2.GT.0) THEN
              THE2=Q(LCDC2+9)
              ETHE2=Q(LCDC2+18)
            END IF
            IF(LFDC2.GT.0) THEN
              THE2=Q(LFDC2+22)
              ETHE2=Q(LFDC2+24)
            END IF
          END IF
C
C-   Track parameters found
C
  202     CONTINUE
          PTMAX=AMAX1(PT1,PT2)
          PTMIN=AMIN1(PT1,PT2)
          IF(CUTPTE.AND.PTMAX.LE.PTXCUT) GOTO 200
C
C-  Variables for the mass calculation and fit
          CALL VZERO(STR,40)
          CALL VZERO(ETR,40)
          STR(1,1)=P1
          STR(1,2)=P2
          STR(4,1)=ELEC
          STR(4,2)=ELEC
          STR(4,3)=PDG_MASS
          STR(2,1)=THE1
          STR(3,1)=PHI1
          STR(2,2)=THE2
          STR(3,2)=PHI2
          ETR(1,1)=EP1 
          ETR(1,2)=EP2 
          ETR(2,1)=ETHE1
          ETR(3,1)=EPHI1
          ETR(2,2)=ETHE2
          ETR(3,2)=EPHI2
C
          NTR=3
          NR=2
          IND(1)=1
          IND(2)=1
          IND(3)=6
          CALL STEP1_MASS(MASS,OPANG)
          IF(CUTMSS.AND.MASS.GT.MSSCUT) GOTO 200 
C
C-  Fill NTUPLE part for the 2nd electron/photon
          CALL EL2_EL_FILL(L2,IDL2,QHIS(KE2+6),PCL2)
          CALL ENERGY_CASH(L2,EM1,EM2,EM3,EM4,FH1)
          QHIS(KE2+56)=EM1
          QHIS(KE2+57)=EM2
          QHIS(KE2+58)=EM3
          QHIS(KE2+59)=EM4
          QHIS(KE2+60)=FH1
C
          LOC=KE2
          QHIS(LOC+1)=P2
          QHIS(LOC+2)=P2*SIN(THE2)
          QHIS(LOC+3)=THE2 
          IF(THE2.GT.0.) QHIS(LOC+4)=-ALOG(TAN(THE2/2.))
          QHIS(LOC+5)=PHI2 
C
C-  Fill NTUPLE part for pairs
          LOC=KEE
          CALL VZERO(QHIS(KEE+1),25)
          QHIS(LOC+1)=STR(1,3) 
          QHIS(LOC+2)=STR(1,3)*SIN(STR(2,3))
          QHIS(LOC+3)=STR(2,3)
          IF(STR(2,3).GT.0) QHIS(LOC+4)=-ALOG(TAN(STR(2,3)/2.))
          QHIS(LOC+5)=STR(3,3)
C
          QHIS(LOC+6)=FLOAT(I1*10+I2) 
          IF(IDL2.NE.0) QHIS(LOC+6)=-QHIS(LOC+6) 
          QHIS(LOC+7)=MASS 
          QHIS(LOC+8)=PTMIN 
          QHIS(LOC+9)=PTMAX
          QHIS(LOC+10)=OPANG
C
C-  Correlate dielectrons with a muon and jets
          CALL EL2_DE_FILL(QHIS) 
C
C-  Calculate masses: PELC/PPHO, CLUSTER, ELFIT
          P4PSI(1)=Q(L1+3)+Q(L2+3)
          P4PSI(2)=Q(L1+4)+Q(L2+4) 
          P4PSI(3)=Q(L1+5)+Q(L2+5) 
          P4PSI(4)=Q(L1+6)+Q(L2+6) 
          QHIS(LOC+18)=SQRT(ABS(P4PSI(4)**2-P4PSI(1)**2-P4PSI(2)**2
     %                      -P4PSI(3)**2))
          P4PSI(1)=PCL1(1)+PCL2(1)
          P4PSI(2)=PCL1(2)+PCL2(2)
          P4PSI(3)=PCL1(3)+PCL2(3)
          P4PSI(4)=PCL1(4)+PCL2(4)
          QHIS(LOC+19)=SQRT(ABS(P4PSI(4)**2-P4PSI(1)**2-P4PSI(2)**2
     %                      -P4PSI(3)**2))
          P4PSI(1)=PCL1(5)+PCL2(5)
          P4PSI(2)=PCL1(6)+PCL2(6)
          P4PSI(3)=PCL1(7)+PCL2(7)
          P4PSI(4)=PCL1(8)+PCL2(8)
          QHIS(LOC+20)=SQRT(ABS(P4PSI(4)**2-P4PSI(1)**2-P4PSI(2)**2
     %                      -P4PSI(3)**2))
C
          CALL VZERO(QHIS(KE1+61),3)
          CALL VZERO(QHIS(KE2+61),3)
  207     IF (MASS.LT.MASS_LO.OR.MASS.GT.MASS_HI) GO TO 210
C
C-  Correlate ISAJET leptons and reconstructed leptons
          IF(MCDATA) CALL ISA_LL(LEPTONS,PDG_MASS,
     %    MCE1,MCTHE1,MCPHI1,MCE2,MCTHE2,MCPHI2,MCMASS,FOUND)
          IF(FOUND) THEN
            QHIS(36)=MCMASS
            R1=ABS(THE1-MCTHE1) 
            R2=ABS(THE1-MCTHE2) 
            IF(R1.LT.R2) THEN
              KF1=KE1
              KF2=KE2
            ELSE
              KF1=KE2
              KF2=KE1
            ENDIF
            QHIS(KF1+61)=MCE1
            QHIS(KF1+62)=MCTHE1
            QHIS(KF1+63)=MCPHI1
            QHIS(KF2+61)=MCE2
            QHIS(KF2+62)=MCTHE2
            QHIS(KF2+63)=MCPHI2
          ENDIF
C
C-  Call the kinematic fitting.
          IEND=0
          IF (DO_KIN_FIT.EQ..TRUE.) CALL VKIN_FIT(2,IEND,PRB,CHI)
          IF (IEND.LT.0) GO TO 200
C
  210     NPAIR=NPAIR + 1
          NACC = NACC + 1
          QHIS(3)=FLOAT(NPAIR)
          QHIS(37)=FLOAT(NTOT)   ! Total number of events processed
          QHIS(38)=FLOAT(N2EL)   ! Number of events with accepted ee pair
          QHIS(39)=FLOAT(NACC)   ! Total number of accepted pairs
C
C- Fill Ntuple 
C
          CALL DHDIR('DILBOSON_RCP','HBOOK_DIRECTORY',IER,' ')
C          CALL HCDIR('//PAWC',' ') 
          CALL HFN(IDN,QHIS)
C
  200   CONTINUE
  100 CONTINUE
      IF(NPAIR.GT.0) THEN
        DIELEC=.TRUE.
        N2EL=N2EL+1
      ENDIF
C
  999 RETURN
      END
