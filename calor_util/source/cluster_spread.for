      SUBROUTINE CLUSTER_SPREAD(EMID,ETPH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Calculates energy weighted moments of EM
C-                            cluster and related quantities.
C-
C-   Inputs  : EMID = PPHO or PELC bank ID
C-   Outputs : ETPH(27), array of values for shape variables
C-   Controls: None
C-
C-   Created  MAY-92   R. Madden
C-   Updated  15-Sep-94   R. Madden  Adapted for general package use
C-   Updated   9-Dec-94   H. Greenlee  Get rid of incorrect machine block.
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:CHMATR.INC'
      INCLUDE 'D0$INC:CISALEC.INC'
C
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      INTEGER ILYR,BITS
      INTEGER PAKADD_WORD,NLYR
      INTEGER EMID
      INTEGER NEVENT,NPPHO
      INTEGER NCELL,EM_LY_NCELL(7),C

      BYTE PAKADD_BYTE(4)
      EQUIVALENCE (PAKADD_WORD,PAKADD_BYTE)
 
      REAL IETA,IPHI,CETA,CPHI
      REAL ENE_MAX,ENE_CLUS,EM_LY(8),EM_LY_MAX(7)
      REAL ENERGY
      REAL IN(2,2),R,ALPHA,MAJ,MIN,CIRC
      REAL INL(2,2),LALPHA,LMAJ,LMIN,LCIRC
      REAL INL2(2,2),L2ALPHA,L2MAJ,L2MIN,L2CIRC
      REAL INL3(2,2),L3ALPHA,L3MAJ,L3MIN,L3CIRC
      REAL IN_ONE(2,2),ALPHA_ONE,MAJ_ONE,MIN_ONE,CIRC_ONE
      REAL INL_ONE(2,2),LALPHA_ONE,LMAJ_ONE,LMIN_ONE,LCIRC_ONE
      REAL IN_TWO(2,2),ALPHA_TWO,MAJ_TWO,MIN_TWO,CIRC_TWO
      REAL INL_TWO(2,2),LALPHA_TWO,LMAJ_TWO,LMIN_TWO,LCIRC_TWO
      REAL IN_FOUR(2,2),ALPHA_FOUR,MAJ_FOUR,MIN_FOUR,CIRC_FOUR
      REAL INL_FOUR(2,2),LALPHA_FOUR,LMAJ_FOUR,LMIN_FOUR,LCIRC_FOUR
      REAL IN_TOT(2,2),ALPHA_TOT,MAJ_TOT,MIN_TOT,CIRC_TOT
      REAL INL_TOT(2,2),LALPHA_TOT,LMAJ_TOT,LMIN_TOT,LCIRC_TOT
      REAL CETAMAX,CPHIMAX,DETA,DPHI,CETAMEAN,CPHIMEAN,E,P,ENER3
      REAL CETAMEAN1,CPHIMEAN1,CETAMEAN2,CPHIMEAN2
      REAL CETAMEAN3,CPHIMEAN3,CETAMEAN4,CPHIMEAN4
      REAL E1,E2,E3,E4,P1,P2,P3,P4,R1,R2,R3,R4,ENER1,ENER2,ENER4,ENER5
      REAL ETASPREAD,PHISPREAD,SPREAD,WEIGHT,WEISUM
      REAL WEIGHT_ONE,WEISUM_ONE
      REAL WEIGHT_TWO,WEISUM_TWO
      REAL WEIGHT_FOUR,WEISUM_FOUR
      REAL WEIGHT_TOT,WEISUM_TOT
      REAL LETASPREAD,LPHISPREAD,LSPREAD,WEIGHT1,WEISUM1
      REAL L2ETASPREAD,L2PHISPREAD,L2SPREAD,WEIGHT2,WEISUM2
      REAL L3ETASPREAD,L3PHISPREAD,L3SPREAD,WEIGHT3,WEISUM3
      REAL LSPREAD_ONE,LSPREAD_TWO,LSPREAD_FOUR,LSPREAD_TOT
      REAL ET_PHOT,ETPH(27),ETPHOT1
C
      LOGICAL EM
C
C-----------------------------------------------------------------------------
C   Get energy in each layer (for normalizing cell weights)
C
      CALL VZERO (ETPH, 27)
      ETPHOT1=0
    2 ENER1=0
      ENER2=0
      ENER3=0
      ENER4=0
      ENER5=0
      ENE_CLUS=0
      ET_PHOT=0
      LCACL = LQ(EMID-2)
      LCASH = LQ(LCACL-2)
      DO WHILE (LCASH .GT. 0)
        NCELL = IQ(LCASH+2)
        DO C=1,2*NCELL-1,2
          PAKADD_WORD = IQ(LCASH+2+C)
          ENERGY      = Q(LCASH+3+C)
          ILYR = PAKADD_BYTE(BYTE2)
          IF (ILYR.EQ.1)  ENER1=ENER1+ENERGY
          IF (ILYR.EQ.2)  ENER2=ENER2+ENERGY
          IF (ILYR.GE.3.AND.ILYR.LE.6) ENER3=ENER3+ENERGY
          IF (ILYR.EQ.7)  ENER4=ENER4+ENERGY
          IF (ILYR.EQ.11)  ENER5=ENER5+ENERGY
          ENE_CLUS=ENE_CLUS+ENERGY
        ENDDO
        LCASH = LQ(LCASH)
      ENDDO
C------------------------------------------------------------------------------
C
      NEVENT = 0
      NPPHO = 0
      NPPHO = NPPHO + 1
      ET_PHOT  = Q(EMID+7)
C
C
      LCACL = LQ(EMID-2)
      LCASH = LQ(LCACL-2)
C
      ENE_MAX= 0.0
      CETA=0.0
      CPHI=0.0
      CETAMAX= 0.0
      CPHIMAX= 0.0
      CETAMEAN= 0.0
      CPHIMEAN= 0.0
      CETAMEAN1= 0.0
      CPHIMEAN1= 0.0
      CETAMEAN2= 0.0
      CPHIMEAN2= 0.0
      CETAMEAN3= 0.0
      CPHIMEAN3= 0.0
      CETAMEAN4= 0.0
      CPHIMEAN4= 0.0
      CALL VZERO(EM_LY,7)
      CALL VZERO(EM_LY_MAX,7)
      CALL VZERO(EM_LY_NCELL,7)
      WEISUM= 0.0
      WEISUM_ONE= 0.0
      WEISUM_TWO= 0.0
      WEISUM_FOUR= 0.0
      WEISUM_TOT= 0.0
      DO WHILE (LCASH .GT. 0)
        NCELL = IQ(LCASH+2)
C
C
        DO C=1,2*NCELL-1,2
          EM = .FALSE.
          PAKADD_WORD = IQ(LCASH+2+C)
          ENERGY      = Q(LCASH+3+C)
          IETA = PAKADD_BYTE(BYTE4)
          IPHI = PAKADD_BYTE(BYTE3)
          ILYR = PAKADD_BYTE(BYTE2)
          BITS = PAKADD_BYTE(BYTE1)
          NLYR = ILYR                     !  set layers from 1-4
          IF (ABS(IETA) .GT. 0) THEN
            CETA = IETA - 0.5*IETA/ABS(IETA)
          ENDIF
          IF (ABS(IPHI) .GT. 0) THEN
            CPHI = IPHI - 0.5*IPHI/ABS(IPHI)
          ENDIF
          IF (ILYR .LE. 7) EM = .TRUE.
          IF (ILYR .EQ. 8) EM_LY(8)=EM_LY(8)+ENERGY
          IF (ILYR.GE.3.AND.ILYR.LE.6) NLYR = 3
          IF (EM) THEN
            EM_LY(NLYR) = EM_LY(NLYR) + ENERGY
            EM_LY_NCELL(NLYR)= EM_LY_NCELL(NLYR) + 1
            IF (ENERGY .GT. ENE_MAX) ENE_MAX = ENERGY
            IF (ENERGY .GT. EM_LY_MAX(NLYR)) EM_LY_MAX(NLYR)=ENERGY
C-----------------------------------------------------------------------------
C          SET THE ORIGIN FOR cluster COORDINATE SYSTEM
C-----------------------------------------------------------------------------
            DETA=0
            DPHI=0
            WEIGHT_TOT= 0.0
            IF (ENE_CLUS.LE.0) GOTO 999
            IF (ENERGY/ENE_CLUS.GT.0) THEN
              WEIGHT_TOT = 5.5 + LOG(ENERGY/ENE_CLUS)
            ENDIF
            IF (WEIGHT_TOT .LT. 0) WEIGHT_TOT=0
            WEISUM_TOT = WEISUM_TOT + WEIGHT_TOT
            DETA=0
            DPHI=0
            IF (NLYR .EQ. 3) THEN
              IF (ILYR .EQ. 3 .OR. ILYR .EQ. 4) DETA=-0.25
              IF (ILYR .EQ. 5 .OR. ILYR .EQ. 6) DETA=0.25
              IF (ILYR .EQ. 3 .OR. ILYR .EQ. 5) DPHI=-0.25
              IF (ILYR .EQ. 4 .OR. ILYR .EQ. 6) DPHI=0.25
              WEIGHT= 0.0
              IF (ENERGY/ENER3.GT.0) THEN
                WEIGHT = 5.0 + LOG(ENERGY/ENER3)
              ENDIF
              IF (WEIGHT.LT.0) WEIGHT=0
              WEISUM = WEISUM + WEIGHT
              CETAMEAN3=CETAMEAN3+(CETA+DETA)*WEIGHT
              CPHIMEAN3=CPHIMEAN3+(CPHI+DPHI)*WEIGHT
              IF (ENERGY .GE. EM_LY_MAX(3)) THEN
                CETAMAX = CETA + DETA
                CPHIMAX = CPHI + DPHI
              ENDIF
            ENDIF
            CETAMEAN=CETAMEAN+(CETA+DETA)*WEIGHT_TOT
            CPHIMEAN=CPHIMEAN+(CPHI+DPHI)*WEIGHT_TOT
            IF (ILYR.EQ.1) THEN
              WEIGHT_ONE= 0.0
              IF (ENERGY.GT.0.AND.ENER1.GT.0) THEN
                WEIGHT_ONE = 5.5 + LOG(ENERGY/ENER1)
              ENDIF
              IF (WEIGHT_ONE.LT.0) WEIGHT_ONE=0
              WEISUM_ONE = WEISUM_ONE + WEIGHT_ONE
              CETAMEAN1=CETAMEAN1+CETA*WEIGHT_ONE
              CPHIMEAN1=CPHIMEAN1+CPHI*WEIGHT_ONE
            ENDIF
            IF (ILYR.EQ.2) THEN
              WEIGHT_TWO= 0.0
              IF (ENERGY.GT.0.AND.ENER2.GT.0) THEN
                WEIGHT_TWO = 5.5 + LOG(ENERGY/ENER2)
              ENDIF
              IF (WEIGHT_TWO.LT.0) WEIGHT_TWO=0
              WEISUM_TWO = WEISUM_TWO + WEIGHT_TWO
              CETAMEAN2=CETAMEAN2+CETA*WEIGHT_TWO
              CPHIMEAN2=CPHIMEAN2+CPHI*WEIGHT_TWO
            ENDIF
            IF (ILYR.EQ.7) THEN
              WEIGHT_FOUR= 0.0
              IF (ENERGY.GT.0.AND.ENER4.GT.0) THEN
                WEIGHT_FOUR = 5.5 + LOG(ENERGY/ENER4)
              ENDIF
              IF (WEIGHT_FOUR.LT.0) WEIGHT_FOUR=0
              WEISUM_FOUR = WEISUM_FOUR + WEIGHT_FOUR
              CETAMEAN4=CETAMEAN4+CETA*WEIGHT_FOUR
              CPHIMEAN4=CPHIMEAN4+CPHI*WEIGHT_FOUR
            ENDIF
          ENDIF
        ENDDO
        LCASH = LQ(LCASH)
      ENDDO
C-----------------------------------------------------------------------------
C          FILL MOMENT OF ENERGY-INERTIA TENSOR
C-----------------------------------------------------------------------------
      IN(1,1)=0.
      IN(1,2)=0.
      IN(2,1)=0.
      IN(2,2)=0.
      INL(1,1)=0.
      INL(1,2)=0.
      INL(2,1)=0.
      INL(2,2)=0.
      IN_ONE(1,1)=0.
      IN_ONE(1,2)=0.
      IN_ONE(2,1)=0.
      IN_ONE(2,2)=0.
      INL_FOUR(2,1)=0.
      INL_FOUR(1,2)=0.
      IN_TOT(1,1)=0.
      INL_ONE(2,2)=0.
      IN_TWO(1,1)=0.
      IN_TWO(1,2)=0.
      IN_TWO(2,1)=0.
      IN_TWO(2,2)=0.
      INL_TWO(1,1)=0.
      INL_TWO(1,2)=0.
      INL_TWO(2,1)=0.
      INL_TWO(2,2)=0.
      IN_FOUR(1,1)=0.
      IN_FOUR(1,2)=0.
      IN_FOUR(2,1)=0.
      IN_FOUR(2,2)=0.
      INL_FOUR(1,1)=0.
      INL_FOUR(1,2)=0.
      INL_FOUR(2,1)=0.
      INL_FOUR(2,2)=0.
      IN_TOT(1,1)=0.
      IN_TOT(1,2)=0.
      IN_TOT(2,1)=0.
      IN_TOT(2,2)=0.
      INL_TOT(1,1)=0.
      INL_TOT(1,2)=0.
      INL_TOT(2,1)=0.
      INL_TOT(2,2)=0.
      ETASPREAD=0.
      PHISPREAD=0.
      SPREAD=0.
      LETASPREAD=0.
      LPHISPREAD=0.
      LSPREAD=0.
      L2SPREAD=0
      L3SPREAD=0
      LSPREAD_ONE=0.
      LSPREAD_TWO=0.
      LSPREAD_FOUR=0.
      LSPREAD_TOT=0.
      LCIRC=0
      L2CIRC=0
      L3CIRC=0
      LCIRC_ONE=0
      LCIRC_TWO=0
      LCIRC_FOUR=0
      LCIRC_TOT=0
      IF (WEISUM .GT. 0) THEN
        CETAMEAN3=CETAMEAN3/WEISUM
        CPHIMEAN3=CPHIMEAN3/WEISUM
      ENDIF
      IF (WEISUM_ONE .GT. 0) THEN
        CETAMEAN1=CETAMEAN1/WEISUM_ONE
        CPHIMEAN1=CPHIMEAN1/WEISUM_ONE
      ENDIF
      IF (WEISUM_TWO .GT. 0) THEN
        CETAMEAN2=CETAMEAN2/WEISUM_TWO
        CPHIMEAN2=CPHIMEAN2/WEISUM_TWO
      ENDIF
      IF (WEISUM_FOUR .GT. 0) THEN
        CETAMEAN4=CETAMEAN4/WEISUM_FOUR
        CPHIMEAN4=CPHIMEAN4/WEISUM_FOUR
      ENDIF
      IF (WEISUM_TOT .GT. 0) THEN
        CETAMEAN=CETAMEAN/WEISUM_TOT
        CPHIMEAN=CPHIMEAN/WEISUM_TOT
      ENDIF
C
      LCACL = LQ(EMID-2)
      LCASH = LQ(LCACL-2)
C
      WEISUM1=0.
      WEISUM2=0.
      WEISUM3=0.
      WEISUM_ONE= 0.0
      WEISUM_TWO= 0.0
      WEISUM_FOUR= 0.0
      WEISUM_TOT= 0.0
      DO WHILE (LCASH .GT. 0)
        DO C=1,2*NCELL-1,2
          PAKADD_WORD = IQ(LCASH+2+C)
          ENERGY      = Q(LCASH+3+C)
          IETA = PAKADD_BYTE(BYTE4)
          IPHI = PAKADD_BYTE(BYTE3)
          ILYR = PAKADD_BYTE(BYTE2)
          BITS = PAKADD_BYTE(BYTE1)
          IF (ABS(IETA) .GT. 0) THEN
            CETA = IETA - 0.5*IETA/ABS(IETA)
          ENDIF
          IF (ABS(IPHI) .GT. 0) THEN
            CPHI = IPHI - 0.5*IPHI/ABS(IPHI)
          ENDIF
          WEIGHT_TOT = 0.0
          E=CETA-CETAMEAN+DETA
          P=CPHI-CPHIMEAN+DPHI
          R=(E**2+P**2)**0.5
          IF (ENERGY.GT.0) THEN
            IN_TOT(1,1)= IN_TOT(1,1)+ENERGY/ENE_CLUS*(R**2-E**2)
            IN_TOT(2,2)= IN_TOT(1,1)+ENERGY/ENE_CLUS*(R**2-P**2)
            IN_TOT(1,2)= IN_TOT(1,1)+ENERGY/ENE_CLUS*(-E*P)
            IN_TOT(2,1)=IN_TOT(1,2)
          ENDIF
          IF (ENERGY.GT.0.AND.ENE_CLUS.GT.0) THEN
            WEIGHT_TOT = 5.5 + LOG(ENERGY/ENE_CLUS)
          ENDIF
          IF (WEIGHT_TOT .LT. 0) WEIGHT_TOT=0
          WEISUM_TOT = WEISUM_TOT + WEIGHT_TOT
          INL_TOT(1,1)=INL_TOT(1,1)+WEIGHT_TOT*(R**2-E**2)
          INL_TOT(2,2)=INL_TOT(2,2)+WEIGHT_TOT*(R**2-P**2)
          INL_TOT(1,2)=INL_TOT(1,2)+WEIGHT_TOT*(-E*P)
          INL_TOT(2,1)=INL_TOT(1,2)
          LSPREAD_TOT=LSPREAD_TOT+ABS(R2*WEIGHT_TOT)
          IF (ILYR.GE.3.AND.ILYR.LE.6) THEN
            NLYR = 3
            IF (EM_LY(3) .GT. 0) THEN
              DETA=0
              DPHI=0
              WEIGHT1=0
              WEIGHT2=0
              WEIGHT3=0
              IF (ILYR .EQ. 3 .OR. ILYR .EQ. 4) DETA=-0.25
              IF (ILYR .EQ. 5 .OR. ILYR .EQ. 6) DETA=0.25
              IF (ILYR .EQ. 3 .OR. ILYR .EQ. 5) DPHI=-0.25
              IF (ILYR .EQ. 4 .OR. ILYR .EQ. 6) DPHI=0.25
              E3=CETA-CETAMEAN3+DETA
              P3=CPHI-CPHIMEAN3+DPHI
              R3=(E3**2+P3**2)**0.5
              IF (ENERGY.GT.0.AND.ENER3 .GT. 0) THEN
                IN(1,1)=IN(1,1)+ENERGY/ENER3*(R3**2-E3**2)
                IN(2,2)=IN(2,2)+ENERGY/ENER3*(R3**2-P3**2)
                IN(1,2)=IN(1,2)+ENERGY/ENER3*(-E3*P3)
                IF (ENERGY.GT.0.AND.ENER3.GT.0) THEN
                  WEIGHT1 = 4.5 + LOG(ENERGY/ENER3)
                ENDIF
                IF (WEIGHT1 .LT. 0) WEIGHT1 = 0
                WEISUM1 = WEISUM1 + WEIGHT1
                INL(1,1)=INL(1,1)+WEIGHT1*(R3**2-E3**2)
                INL(2,2)=INL(2,2)+WEIGHT1*(R3**2-P3**2)
                INL(1,2)=INL(1,2)+WEIGHT1*(-E3*P3)
                IF (ENERGY.GT.0.AND.ENER3.GT.0) THEN
                  WEIGHT2 = 5.0 + LOG(ENERGY/ENER3)
                ENDIF
                IF (WEIGHT2 .LT. 0) WEIGHT2 = 0
                WEISUM2 = WEISUM2 + WEIGHT2
                INL2(1,1)=INL2(1,1)+WEIGHT2*(R3**2-E3**2)
                INL2(2,2)=INL2(2,2)+WEIGHT2*(R3**2-P3**2)
                INL2(1,2)=INL2(1,2)+WEIGHT2*(-E3*P3)
                IF (ENERGY.GT.0.AND.ENER3.GT.0) THEN
                  WEIGHT3 = 5.5 + LOG(ENERGY/ENER3)
                ENDIF
                IF (WEIGHT3 .LT. 0) WEIGHT3 = 0
                WEISUM3 = WEISUM3 + WEIGHT3
                INL3(1,1)=INL3(1,1)+WEIGHT3*(R3**2-E3**2)
                INL3(2,2)=INL3(2,2)+WEIGHT3*(R3**2-P3**2)
                INL3(1,2)=INL3(1,2)+WEIGHT3*(-E3*P3)
                IN(2,1)=IN(1,2)
                INL(2,1)=INL(1,2)
                INL2(2,1)=INL2(1,2)
                INL3(2,1)=INL3(1,2)
                IF (ENERGY.GT.0.AND.ENER3.GT.0) THEN
                  ETASPREAD=ETASPREAD+ABS(E3*ENERGY/ENER3)
                  PHISPREAD=PHISPREAD+ABS(P3*ENERGY/ENER3)
                  SPREAD=SPREAD+ABS(R3*ENERGY/ENER3)
                ENDIF
                LETASPREAD=LETASPREAD+ABS(E3*WEIGHT1)
                LPHISPREAD=LPHISPREAD+ABS(P3*WEIGHT1)
                LSPREAD=LSPREAD+ABS(R3*WEIGHT1)
                L2ETASPREAD=L2ETASPREAD+ABS(E3*WEIGHT2)
                L2PHISPREAD=L2PHISPREAD+ABS(P3*WEIGHT2)
                L2SPREAD=L2SPREAD+ABS(R3*WEIGHT2)
                L3ETASPREAD=L3ETASPREAD+ABS(E3*WEIGHT3)
                L3PHISPREAD=L3PHISPREAD+ABS(P3*WEIGHT3)
                L3SPREAD=L3SPREAD+ABS(R3*WEIGHT3)
              ENDIF
            ENDIF
          ENDIF
          IF (ILYR .EQ. 1) THEN
            WEIGHT_ONE=0
            E1=IETA-CETAMEAN1
            P1=IPHI-CPHIMEAN1
            R1=(E1**2+P1**2)**0.5
            IF (ENERGY.GT.0.AND.ENER1.GT.0) THEN
              IN_ONE(1,1)=IN_ONE(1,1)+ENERGY/ENER1*(R1**2-E1**2)
              IN_ONE(2,2)=IN_ONE(2,2)+ENERGY/ENER1*(R1**2-P1**2)
              IN_ONE(1,2)=IN_ONE(1,2)+ENERGY/ENER1*(-E1*P1)
              IN_ONE(2,1)=IN_ONE(1,2)
              WEIGHT_ONE = 5.5 + LOG(ENERGY/ENER1)
              IF (WEIGHT_ONE .LT. 0) WEIGHT_ONE=0
              WEISUM_ONE = WEISUM_ONE + WEIGHT_ONE
              INL_ONE(1,1)=INL_ONE(1,1)+WEIGHT_ONE*(R1**2-E1**2)
              INL_ONE(2,2)=INL_ONE(2,2)+WEIGHT_ONE*(R1**2-P1**2)
              INL_ONE(1,2)=INL_ONE(1,2)+WEIGHT_ONE*(-E1*P1)
              INL_ONE(2,1)=INL_ONE(1,2)
              LSPREAD_ONE=LSPREAD_ONE+ABS(R2*WEIGHT_ONE)
            ENDIF
          ENDIF
          IF (ILYR .EQ. 2) THEN
            WEIGHT_TWO=0
            E2=IETA-CETAMEAN2
            P2=IPHI-CPHIMEAN2
            R2=(E2**2+P2**2)**0.5
            IF (ENERGY.GT.0.AND.ENER2.GT.0) THEN
              IN_TWO(1,1)=IN_TWO(1,1)+ENERGY/ENER2*(R2**2-E2**2)
              IN_TWO(2,2)=IN_TWO(2,2)+ENERGY/ENER2*(R2**2-P2**2)
              IN_TWO(1,2)=IN_TWO(1,2)+ENERGY/ENER2*(-E2*P2)
              IN_TWO(2,1)=IN_TWO(1,2)
              WEIGHT_TWO = 5.5 + LOG(ENERGY/ENER2)
              IF (WEIGHT_TWO .LT. 0) WEIGHT_TWO=0
              WEISUM_TWO = WEISUM_TWO + WEIGHT_TWO
              INL_TWO(1,1)=INL_TWO(1,1)+WEIGHT_TWO*(R2**2-E2**2)
              INL_TWO(2,2)=INL_TWO(2,2)+WEIGHT_TWO*(R2**2-P2**2)
              INL_TWO(1,2)=INL_TWO(1,2)+WEIGHT_TWO*(-E2*P2)
              INL_TWO(2,1)=INL_TWO(1,2)
              LSPREAD_TWO=LSPREAD_TWO+ABS(R2*WEIGHT_TWO)
            ENDIF
          ENDIF
          IF (ILYR .EQ. 7) THEN
            WEIGHT_FOUR=0
            E4=IETA-CETAMEAN4
            P4=IPHI-CPHIMEAN4
            R4=(E4**2+P4**2)**0.5
            IF (ENERGY.GT.0.AND.ENER4.GT.0) THEN
              IN_FOUR(1,1)=IN_FOUR(1,1)+ENERGY/ENER4*(R4**2-E4**2)
              IN_FOUR(2,2)=IN_FOUR(2,2)+ENERGY/ENER4*(R4**2-P4**2)
              IN_FOUR(1,2)=IN_FOUR(1,2)+ENERGY/ENER4*(-E4*P4)
              IN_FOUR(2,1)=IN_FOUR(1,2)
              WEIGHT_FOUR = 5.5 + LOG(ENERGY/ENER4)
              IF (WEIGHT_FOUR .LT. 0) WEIGHT_FOUR=0
              WEISUM_FOUR = WEISUM_FOUR + WEIGHT_FOUR
              INL_FOUR(1,1)=INL_FOUR(1,1)+WEIGHT_FOUR*(R4**2-E4**2)
              INL_FOUR(2,2)=INL_FOUR(2,2)+WEIGHT_FOUR*(R4**2-P4**2)
              INL_FOUR(1,2)=INL_FOUR(1,2)+WEIGHT_FOUR*(-E4*P4)
              INL_FOUR(2,1)=INL_FOUR(1,2)
              LSPREAD_FOUR=LSPREAD_FOUR+ABS(R2*WEIGHT_FOUR)
            ENDIF
          ENDIF
        ENDDO
        LCASH = LQ(LCASH)
      ENDDO
      IF (WEISUM1.GT.0)THEN
        INL(1,1)=INL(1,1)/WEISUM1
        INL(2,1)=INL(2,1)/WEISUM1
        INL(1,2)=INL(1,2)/WEISUM1
        INL(2,2)=INL(2,2)/WEISUM1
        LETASPREAD=LETASPREAD/WEISUM1
        LPHISPREAD=LPHISPREAD/WEISUM1
        LSPREAD=LSPREAD/WEISUM1
      ENDIF
      IF (WEISUM2.GT.0)THEN
        INL2(1,1)=INL2(1,1)/WEISUM2
        INL2(2,1)=INL2(2,1)/WEISUM2
        INL2(1,2)=INL2(1,2)/WEISUM2
        INL2(2,2)=INL2(2,2)/WEISUM2
        L2ETASPREAD=L2ETASPREAD/WEISUM2
        L2PHISPREAD=L2PHISPREAD/WEISUM2
        L2SPREAD=L2SPREAD/WEISUM2
      ENDIF
      IF (WEISUM3.GT.0)THEN
        INL3(1,1)=INL3(1,1)/WEISUM3
        INL3(2,1)=INL3(2,1)/WEISUM3
        INL3(1,2)=INL3(1,2)/WEISUM3
        INL3(2,2)=INL3(2,2)/WEISUM3
        L3ETASPREAD=L3ETASPREAD/WEISUM3
        L3PHISPREAD=L3PHISPREAD/WEISUM3
        L3SPREAD=L3SPREAD/WEISUM3
      ENDIF
      IF (WEISUM_ONE .GT. 0) THEN
        INL_ONE(1,1)=INL_ONE(1,1)/WEISUM_ONE
        INL_ONE(2,2)=INL_ONE(2,2)/WEISUM_ONE
        INL_ONE(2,1)=INL_ONE(2,1)/WEISUM_ONE
        INL_ONE(1,2)=INL_ONE(1,2)/WEISUM_ONE
        LSPREAD_ONE=LSPREAD_ONE/WEISUM_ONE
      ELSE
        LSPREAD_ONE=-99
      ENDIF
      IF (WEISUM_TWO .GT. 0) THEN
        INL_TWO(1,1)=INL_TWO(1,1)/WEISUM_TWO
        INL_TWO(2,2)=INL_TWO(2,2)/WEISUM_TWO
        INL_TWO(2,1)=INL_TWO(2,1)/WEISUM_TWO
        INL_TWO(1,2)=INL_TWO(1,2)/WEISUM_TWO
        LSPREAD_TWO=LSPREAD_TWO/WEISUM_TWO
      ELSE
        LSPREAD_TWO=-99
      ENDIF
      IF (WEISUM_FOUR .GT. 0) THEN
        INL_FOUR(1,1)=INL_FOUR(1,1)/WEISUM_FOUR
        INL_FOUR(2,2)=INL_FOUR(2,2)/WEISUM_FOUR
        INL_FOUR(2,1)=INL_FOUR(2,1)/WEISUM_FOUR
        INL_FOUR(1,2)=INL_FOUR(1,2)/WEISUM_FOUR
        LSPREAD_FOUR=LSPREAD_FOUR/WEISUM_FOUR
      ELSE
        LSPREAD_FOUR=-99
      ENDIF
      IF (WEISUM_TOT .GT. 0) THEN
        INL_TOT(1,1)=INL_TOT(1,1)/WEISUM_TOT
        INL_TOT(2,2)=INL_TOT(2,2)/WEISUM_TOT
        INL_TOT(2,1)=INL_TOT(2,1)/WEISUM_TOT
        INL_TOT(1,2)=INL_TOT(1,2)/WEISUM_TOT
        LSPREAD_TOT=LSPREAD_TOT/WEISUM_TOT
      ELSE
        LSPREAD_TOT=-99
      ENDIF
C-----------------------------------------------------------------------------
C               DIAGONALIZE ENERGY-INERTIA TENSOR
C-----------------------------------------------------------------------------
      CIRC=0
      ALPHA=0
      MAJ=0
      MIN=0
      IF (ABS(IN(1,1)**2-IN(2,2)**2) .GT. 0) THEN
        IF (ABS(2*IN(1,2)/(IN(1,1)**2-IN(2,2)**2)) .GT. 0)
     &              THEN
          ALPHA=0.5*ATAN(2*IN(1,2)/(IN(1,1)**2-IN(2,2)**2))
        ENDIF
      ENDIF
      MAJ=COS(ALPHA)*IN(1,1)-SIN(ALPHA)*IN(1,2)
      MIN=-SIN(ALPHA)*IN(1,2)-COS(ALPHA)*IN(2,2)
      IF (MAJ.LT.1.0E-5) MAJ=0
      IF (MIN.LT.1.0E-5) MIN=0
      IF (MIN .GT. 0) THEN
        CIRC = ABS(MAJ/MIN)
      ENDIF
      IF (MIN .EQ. 0 .OR. MAJ .EQ. 0) CIRC = 0
      IF (MIN .EQ. 0 .AND. MAJ .EQ. 0) CIRC = 1
      IF (CIRC .GT. 1) CIRC=1/CIRC
C------------------------------
C  cutoff constant = 4.5
C------------------------------
      LALPHA=0
      LMAJ=0
      LMIN=0
      IF (ABS(INL(1,1)**2-INL(2,2)**2) .GT. 0) THEN
        IF (ABS(2*INL(1,2)/(INL(1,1)**2-INL(2,2)**2)) .GT. 0)
     &              THEN
          LALPHA=0.5*ATAN(2*INL(1,2)/(INL(1,1)**2-INL(2,2)**2))
        ENDIF
      ENDIF
      LMAJ=COS(LALPHA)*INL(1,1)-SIN(LALPHA)*INL(1,2)
      LMIN=-SIN(LALPHA)*INL(1,2)-COS(LALPHA)*INL(2,2)
      IF (LMAJ.LT.1.0E-5) LMAJ=0
      IF (LMIN.LT.1.0E-5) LMIN=0
      IF (LMIN .GT. 0) THEN
        LCIRC = ABS(LMAJ/LMIN)
      ENDIF
      IF (LMIN .EQ. 0 .OR. LMAJ .EQ. 0) LCIRC = 0
      IF (LMIN .EQ. 0 .AND. LMAJ .EQ. 0) LCIRC = 1
      IF (LCIRC .GT. 1) LCIRC=1/LCIRC
C------------------------------
C  cutoff constant = 5.0
C------------------------------
      L2CIRC=0
      L2ALPHA=0
      L2MAJ=0
      L2MIN=0
      IF (ABS(INL2(1,1)**2-INL2(2,2)**2) .GT. 0) THEN
        IF (ABS(2*INL2(1,2)/(INL2(1,1)**2-INL2(2,2)**2)) .GT. 0)
     &              THEN
          L2ALPHA=0.5*ATAN(2*INL2(1,2)/(INL2(1,1)**2-INL2(2,2)**2))
        ENDIF
      ENDIF
      L2MAJ=COS(L2ALPHA)*INL2(1,1)-SIN(L2ALPHA)*INL2(1,2)
      L2MIN=-SIN(L2ALPHA)*INL2(1,2)-COS(L2ALPHA)*INL2(2,2)
      IF (L2MAJ.LT.1.0E-5) L2MAJ=0
      IF (L2MIN.LT.1.0E-5) L2MIN=0
      IF (L2MIN .GT. 0) THEN
        L2CIRC = ABS(L2MAJ/L2MIN)
      ENDIF
      IF (L2MIN .EQ. 0 .OR. L2MAJ .EQ. 0) L2CIRC = 0
      IF (L2MIN .EQ. 0 .AND. L2MAJ .EQ. 0) L2CIRC = 1
      IF (L2CIRC .GT. 1) L2CIRC=1/L2CIRC
C------------------------------
C  cutoff constant = 5.5
C------------------------------
      L3CIRC=0
      L3ALPHA=0
      L3MAJ=0
      L3MIN=0
      IF (ABS(INL3(1,1)**2-INL3(2,2)**2) .GT. 0) THEN
        IF (ABS(2*INL3(1,2)/(INL3(1,1)**2-INL3(2,2)**2)) .GT. 0)
     &              THEN
          L3ALPHA=0.5*ATAN(2*INL3(1,2)/(INL3(1,1)**2-INL3(2,2)**2))
        ENDIF
      ENDIF
      L3MAJ=COS(L3ALPHA)*INL3(1,1)-SIN(L3ALPHA)*INL3(1,2)
      L3MIN=-SIN(L3ALPHA)*INL3(1,2)-COS(L3ALPHA)*INL3(2,2)
      IF (L3MAJ.LT.1.0E-5) L3MAJ=0
      IF (L3MIN.LT.1.0E-5) L3MIN=0
      IF (L3MIN .GT. 0) THEN
        L3CIRC = ABS(L3MAJ/L3MIN)
      ENDIF
      IF (L3MIN .EQ. 0 .OR. L3MAJ .EQ. 0) L3CIRC = 0
      IF (L3MIN .EQ. 0 .AND. L3MAJ .EQ. 0) L3CIRC = 1
      IF (L3CIRC .GT. 1) L3CIRC=1/L3CIRC
C-----------------------------------------------------------------------------
C               DIAGONALIZE ENERGY-INERTIA TENSOR layer 1
C-----------------------------------------------------------------------------
      CIRC_ONE=0
      ALPHA_ONE=0
      MAJ_ONE=0
      MIN_ONE=0
      IF (ABS(IN_ONE(1,1)**2-IN_ONE(2,2)**2) .GT. 0) THEN
        IF (ABS(2*IN_ONE(1,2)/(IN_ONE(1,1)**2-IN_ONE(2,2)**2)) .GT.
     &        0) THEN
          ALPHA_ONE=0.5*ATAN(2*IN_ONE(1,2)/(IN_ONE(1,1)**2-IN_ONE(2,
     &          2)**2))
        ENDIF
      ENDIF
      MAJ_ONE=COS(ALPHA_ONE)*IN_ONE(1,1)-SIN(ALPHA_ONE)*IN_ONE(1,2)
      MIN_ONE=-SIN(ALPHA_ONE)*IN_ONE(1,2)-COS(ALPHA_ONE)*IN_ONE(2,2)
      IF (MAJ_ONE.LT.1.0E-5) MAJ_ONE=0
      IF (MIN_ONE.LT.1.0E-5) MIN_ONE=0
      IF (MIN_ONE .GT. 0) THEN
        CIRC_ONE = ABS(MAJ_ONE/MIN_ONE)
      ENDIF
      IF (MIN_ONE .EQ. 0 .OR. MAJ_ONE .EQ. 0) CIRC_ONE = 0
      IF (MIN_ONE .EQ. 0 .AND. MAJ_ONE .EQ. 0) CIRC_ONE = 1
      IF (CIRC_ONE .GT. 1) CIRC_ONE=1/CIRC_ONE
C----------------------------------------------------------------------------
      LCIRC_ONE=0
      LALPHA_ONE=0
      LMAJ_ONE=0
      LMIN_ONE=0
      IF (ABS(INL_ONE(1,1)**2-INL_ONE(2,2)**2) .GT. 0) THEN
        IF (ABS(2*INL_ONE(1,2)/(INL_ONE(1,1)**2-INL_ONE(2,2)**2)) .
     &        GT. 0) THEN
          LALPHA_ONE=0.5*ATAN(2*INL_ONE(1,2)/(INL_ONE(1,1)
     &          **2-INL_ONE(2,2)**2))
        ENDIF
      ENDIF
      LMAJ_ONE=COS(LALPHA_ONE)*INL_ONE(1,1)-SIN(LALPHA_ONE)
     &      *INL_ONE(1,2)
      LMIN_ONE=-SIN(LALPHA_ONE)*INL_ONE(1,2)-COS(LALPHA_ONE)
     &      *INL_ONE(2,2)
      IF (LMAJ_ONE.LT.1.0E-5) LMAJ_ONE=0
      IF (LMIN_ONE.LT.1.0E-5) LMIN_ONE=0
      IF (LMIN_ONE .GT. 0) THEN
        LCIRC_ONE = ABS(LMAJ_ONE/LMIN_ONE)
      ENDIF
      IF (LMIN_ONE .EQ. 0 .OR. LMAJ_ONE .EQ. 0) LCIRC_ONE = 0
      IF (LMIN_ONE .EQ. 0 .AND. LMAJ_ONE .EQ. 0) LCIRC_ONE = 1
      IF (LCIRC_ONE .GT. 1) LCIRC_ONE=1/LCIRC_ONE
C----------------------------------------------------------------------------
C               DIAGONALIZE ENERGY-INERTIA TENSOR layer 2
C-----------------------------------------------------------------------------
      CIRC_TWO=0
      ALPHA_TWO=0
      MAJ_TWO=0
      MIN_TWO=0
      IF (ABS(IN_TWO(1,1)**2-IN_TWO(2,2)**2) .GT. 0) THEN
        IF (ABS(2*IN_TWO(1,2)/(IN_TWO(1,1)**2-IN_TWO(2,2)**2)) .GT.
     &        0) THEN
          ALPHA_TWO=0.5*ATAN(2*IN_TWO(1,2)/(IN_TWO(1,1)**2-IN_TWO(2,
     &          2)**2))
        ENDIF
      ENDIF
      MAJ_TWO=COS(ALPHA_TWO)*IN_TWO(1,1)-SIN(ALPHA_TWO)*IN_TWO(1,2)
      MIN_TWO=-SIN(ALPHA_TWO)*IN_TWO(1,2)-COS(ALPHA_TWO)*IN_TWO(2,2)
      IF (MAJ_TWO.LT.1.0E-5) MAJ_TWO=0
      IF (MIN_TWO.LT.1.0E-5) MIN_TWO=0
      IF (MIN_TWO .GT. 0) THEN
        CIRC_TWO = ABS(MAJ_TWO/MIN_TWO)
      ENDIF
      IF (MIN_TWO .EQ. 0 .OR. MAJ_TWO .EQ. 0) CIRC_TWO = 0
      IF (MIN_TWO .EQ. 0 .AND. MAJ_TWO .EQ. 0) CIRC_TWO = 1
      IF (CIRC_TWO .GT. 1) CIRC_TWO=1/CIRC_TWO
C----------------------------------------------------------------------------
      LCIRC_TWO=0
      LALPHA_TWO=0
      LMAJ_TWO=0
      LMIN_TWO=0
      IF (ABS(INL_TWO(1,1)**2-INL_TWO(2,2)**2) .GT. 0) THEN
        IF (ABS(2*INL_TWO(1,2)/(INL_TWO(1,1)**2-INL_TWO(2,2)**2)) .
     &        GT. 0) THEN
          LALPHA_TWO=0.5*ATAN(2*INL_TWO(1,2)/(INL_TWO(1,1)
     &          **2-INL_TWO(2,2)**2))
        ENDIF
      ENDIF
      LMAJ_TWO=COS(LALPHA_TWO)*INL_TWO(1,1)-SIN(LALPHA_TWO)
     &      *INL_TWO(1,2)
      LMIN_TWO=-SIN(LALPHA_TWO)*INL_TWO(1,2)-COS(LALPHA_TWO)
     &      *INL_TWO(2,2)
      IF (LMAJ_TWO.LT.1.0E-5) LMAJ_TWO=0
      IF (LMIN_TWO.LT.1.0E-5) LMIN_TWO=0
      IF (LMIN_TWO .GT. 0) THEN
        LCIRC_TWO = ABS(LMAJ_TWO/LMIN_TWO)
      ENDIF
      IF (LMIN_TWO .EQ. 0 .OR. LMAJ_TWO .EQ. 0) LCIRC_TWO = 0
      IF (LMIN_TWO .EQ. 0 .AND. LMAJ_TWO .EQ. 0) LCIRC_TWO = 1
      IF (LCIRC_TWO .GT. 1) LCIRC_TWO=1/LCIRC_TWO
C-----------------------------------------------------------------------------
C               DIAGONALIZE ENERGY-INERTIA TENSOR layer 4
C-----------------------------------------------------------------------------
      CIRC_FOUR=0
      ALPHA_FOUR=0
      MAJ_FOUR=0
      MIN_FOUR=0
      IF (ABS(IN_FOUR(1,1)**2-IN_FOUR(2,2)**2) .GT. 0) THEN
        IF (ABS(2*IN_FOUR(1,2)/(IN_FOUR(1,1)**2-IN_FOUR(2,2)**2)) .
     &        GT. 0) THEN
          ALPHA_FOUR=0.5*ATAN(2*IN_FOUR(1,2)/(IN_FOUR(1,1)
     &          **2-IN_FOUR(2,2)**2))
        ENDIF
      ENDIF
      MAJ_FOUR=COS(ALPHA_FOUR)*IN_FOUR(1,1)-SIN(ALPHA_FOUR)
     &      *IN_FOUR(1,2)
      MIN_FOUR=-SIN(ALPHA_FOUR)*IN_FOUR(1,2)-COS(ALPHA_FOUR)
     &      *IN_FOUR(2,2)
      IF (MAJ_FOUR.LT.1.0E-5) MAJ_FOUR=0
      IF (MIN_FOUR.LT.1.0E-5) MIN_FOUR=0
      IF (MIN_FOUR .GT. 0) THEN
        CIRC_FOUR = ABS(MAJ_FOUR/MIN_FOUR)
      ENDIF
      IF (MIN_FOUR .EQ. 0 .OR. MAJ_FOUR .EQ. 0) CIRC_FOUR = 0
      IF (MIN_FOUR .EQ. 0 .AND. MAJ_FOUR .EQ. 0) CIRC_FOUR = 1
      IF (CIRC_FOUR .GT. 1) CIRC_FOUR=1/CIRC_FOUR
C----------------------------------------------------------------------------
      LCIRC_FOUR=0
      LALPHA_FOUR=0
      LMAJ_FOUR=0
      LMIN_FOUR=0
      IF (ABS(INL_FOUR(1,1)**2-INL_FOUR(2,2)**2) .GT. 0) THEN
        IF (ABS(2*INL_FOUR(1,2)/(INL_FOUR(1,1)**2-INL_FOUR(2,2)**2))
     &        .GT. 0) THEN
          LALPHA_FOUR=0.5*ATAN(2*INL_FOUR(1,2)/(INL_FOUR(1,1)
     &          **2-INL_FOUR(2,2)**2))
        ENDIF
      ENDIF
      LMAJ_FOUR=COS(LALPHA_FOUR)*INL_FOUR(1,1)-SIN(LALPHA_FOUR)
     &      *INL_FOUR(1,2)
      LMIN_FOUR=-SIN(LALPHA_FOUR)*INL_FOUR(1,2)-COS(LALPHA_FOUR)
     &      *INL_FOUR(2,2)
      IF (LMAJ_FOUR.LT.1.0E-5) LMAJ_FOUR=0
      IF (LMIN_FOUR.LT.1.0E-5) LMIN_FOUR=0
      IF (LMIN_FOUR .GT. 0) THEN
        LCIRC_FOUR = ABS(LMAJ_FOUR/LMIN_FOUR)
      ENDIF
      IF (LMIN_FOUR .EQ. 0 .OR. LMAJ_FOUR .EQ. 0) LCIRC_FOUR = 0
      IF (LMIN_FOUR .EQ. 0 .AND. LMAJ_FOUR .EQ. 0) LCIRC_FOUR = 1
      IF (LCIRC_FOUR .GT. 1) LCIRC_FOUR=1/LCIRC_FOUR
C-----------------------------------------------------------------------------
C               DIAGONALIZE ENERGY-INERTIA TENSOR layers 1-4 combined
C-----------------------------------------------------------------------------
      CIRC_TOT=0
      ALPHA_TOT=0
      MAJ_TOT=0
      MIN_TOT=0
      IF (ABS(IN_TOT(1,1)**2-IN_TOT(2,2)**2) .GT. 0) THEN
        IF (ABS(2*IN_TOT(1,2)/(IN_TOT(1,1)**2-IN_TOT(2,2)**2)) .
     &        GT. 0) THEN
          ALPHA_TOT=0.5*ATAN(2*IN_TOT(1,2)/(IN_TOT(1,1)
     &          **2-IN_TOT(2,2)**2))
        ENDIF
      ENDIF
      MAJ_TOT=COS(ALPHA_TOT)*IN_TOT(1,1)-SIN(ALPHA_TOT)
     &      *IN_TOT(1,2)
      MIN_TOT=-SIN(ALPHA_TOT)*IN_TOT(1,2)-COS(ALPHA_TOT)
     &      *IN_TOT(2,2)
      IF (MAJ_TOT.LT.1.0E-5) MAJ_TOT=0
      IF (MIN_TOT.LT.1.0E-5) MIN_TOT=0
      IF (MIN_TOT .GT. 0) THEN
        CIRC_TOT = ABS(MAJ_TOT/MIN_TOT)
      ENDIF
      IF (MIN_TOT .EQ. 0 .OR. MAJ_TOT .EQ. 0) CIRC_TOT = 0
      IF (MIN_TOT .EQ. 0 .AND. MAJ_TOT .EQ. 0) CIRC_TOT = 1
      IF (CIRC_TOT .GT. 1) CIRC_TOT=1/CIRC_TOT
C----------------------------------------------------------------------------
      LCIRC_TOT=0
      LALPHA_TOT=0
      LMAJ_TOT=0
      LMIN_TOT=0
      IF (ABS(INL_TOT(1,1)**2-INL_TOT(2,2)**2) .GT. 0) THEN
        IF (ABS(2*INL_TOT(1,2)/(INL_TOT(1,1)**2-INL_TOT(2,2)**2))
     &        .GT. 0) THEN
          LALPHA_TOT=0.5*ATAN(2*INL_TOT(1,2)/(INL_TOT(1,1)
     &          **2-INL_TOT(2,2)**2))
        ENDIF
      ENDIF
      LMAJ_TOT=COS(LALPHA_TOT)*INL_TOT(1,1)-SIN(LALPHA_TOT)
     &      *INL_TOT(1,2)
      LMIN_TOT=-SIN(LALPHA_TOT)*INL_TOT(1,2)-COS(LALPHA_TOT)
     &      *INL_TOT(2,2)
      IF (LMAJ_TOT.LT.1.0E-5) LMAJ_TOT=0
      IF (LMIN_TOT.LT.1.0E-5) LMIN_TOT=0
      IF (LMIN_TOT .GT. 0) THEN
        LCIRC_TOT = ABS(LMAJ_TOT/LMIN_TOT)
      ENDIF
      IF (LMIN_TOT .EQ. 0 .OR. LMAJ_TOT .EQ. 0) LCIRC_TOT = 0
      IF (LMIN_TOT .EQ. 0 .AND. LMAJ_TOT .EQ. 0) LCIRC_TOT = 1
      IF (LCIRC_TOT .GT. 1) LCIRC_TOT=1/LCIRC_TOT
C----------------------------------------------------------------------------
C  Fill array ETPH with energy-moment related quantities.
C----------------------------------------------------------------------------
      IF (ET_PHOT.GT.ETPHOT1) THEN
        ETPHOT1=ET_PHOT
        ETPH(1)=CIRC      ! circularity weighted by simple energy fraction
        ETPH(2)=LCIRC     ! log weighted circularity, cutoff constant = 4.5
        ETPH(3)=L2CIRC    ! log circ, constant = 5.0
        ETPH(4)=L3CIRC    ! log circ, constant = 5.5
        ETPH(5)=LCIRC_ONE   ! EM layer 1 log circ, constant = 5.5
        ETPH(6)=LCIRC_TWO   ! layer 2
        ETPH(7)=LCIRC_FOUR  ! layer 4
        ETPH(8)=LCIRC_TOT   ! combined layers, 1-4
        ETPH(9)=SPREAD      ! spread weighted by simple energy fraction
        ETPH(10)=LSPREAD    ! log weighted spread, cutoff constant = 4.5
        ETPH(11)=L2SPREAD   ! log spread, constant = 5.0
        ETPH(12)=L2ETASPREAD  ! log spread in pseudorapidity only, c = 5.0
        ETPH(13)=L2PHISPREAD  ! log spread in phi only, c = 5.0
        ETPH(14)=L3SPREAD     ! log spread, constant = 5.5
        ETPH(15)=LSPREAD_ONE  ! EM layer 1 log spread, constant = 5.5
        ETPH(16)=LSPREAD_TWO  ! layer 2
        ETPH(17)=LSPREAD_FOUR ! layer 4
        ETPH(18)=LSPREAD_TOT  ! combined layers, 1-4
        ETPH(19)=FLOAT(EM_LY_NCELL(1)) ! number of cells, layer 1
        ETPH(20)=FLOAT(EM_LY_NCELL(2)) ! layer 2
        ETPH(21)=FLOAT(EM_LY_NCELL(3)) ! layer 3
        ETPH(22)=FLOAT(EM_LY_NCELL(7)) ! layer 4
        ETPH(23)=EM_LY_MAX(1) ! energy of maximum energy cell in layer 1
        ETPH(24)=EM_LY_MAX(2) ! layer 2
        ETPH(25)=EM_LY_MAX(3) ! layer 3
        ETPH(26)=EM_LY_MAX(7) ! layer 4
        ETPH(27)=ENE_CLUS     ! cluster energy
      ENDIF
C-------------------------------------------
      EM_LY(1)=0
      EM_LY(2)=0
      EM_LY(7)=0
      EM_LY(3)=0
  999 RETURN
      END
