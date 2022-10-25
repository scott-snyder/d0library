      LOGICAL FUNCTION QCD_UPK_ELC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access information on electrons for
C-                         diffractive W studies.
C-                         Fills variables in include file so that
C-                         the columnwised Ntuple entries can be filled
C-                         in QCD_NTUP_FILL.
C-
C-   Returned value  : .TRUE.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: CLEANEM.RCP, CORRECTEM.RCP
C-
C-   Created  11-SEP-1995   Jaehoon Yu
C-   Updated  12-DEC-1995   Andrew Brandt  remove JET_ELC_MATCH.INC
C-   Updated  08-JAN-1996   Jaehoon Yu : Added 5, 4, and 3 variable electron
C-                                       likelihood values.
C-   Updated  02-MAR-1996   Andrew Brandt  added NCLOUD,EMVTX call and vars
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHMTE.LINK'
      INCLUDE 'D0$INC:QCD_ELC.INC/LIST'
C
      INTEGER LPELC,GZPELC,LZTRK,I,GOLDELEC, PELC_VER
      REAL    ET,COR_FACTOR, ELIKE
      LOGICAL TRD_INFO(20),GOLDEN(20),STRICT_ELEC,OK,GOLD
      INTEGER TRK,STATUS,STAT_LO,STAT_HI, IER
C
      INTEGER NCVAR,NTVAR, MAXLOG, PELC_ADD, LVCOR
      REAL    CQUAN(50),TQUAN(50), DELTAE, OLDE
      REAL    DUMMY,ELIKE_SET_MASK_CC,ELIKE_SET_MASK_EC
      INTEGER MASK_ALL,MASK_TRDOFF,MASK_TRDOFF_DEDXOFF,
     &  MASK_TRD_DEDX_ONLY
      CHARACTER*4 NAME
      INTEGER NVBST
      Real XBAR(4,3),ZV,RV,PrbRZ,PrbXY,Prb
      INTEGER NPVAR,NCLOUD
      REAL QUANS(15)

      data MASK_ALL / z'1F' /
      data MASK_TRDOFF / z'0F' /
      data MASK_TRDOFF_DEDXOFF / z'0E' /
      data MASK_TRD_DEDX_ONLY / z'11' /
C----------------------------------------------------------------------
      QCD_UPK_ELC = .TRUE.
C
C       Initialization
C
      NELEC = 0
      MAXLOG = 0
      NELC_MAX = 0
      DO I=1,3
        ELC_ET(I)  =-999.
        ELC_E(I)   =-999.
        ELC_EZ(I)  =-999.
        ELC_EX(I)  =-999.
        ELC_EY(I)  =-999.
        ELC_ETA(I) =-999.
        ELC_PHI(I) =-999.
        ELC_EM_FRAC(I)=-999.
        ELC_HMATRIX_CHISQ(I)=-999.
        ELC_DEDX(I) = -999.
        ELC_VTX_MIP(I) = -999.
        ELC_CAL_ETA(I) = -999
        ELC_DISPERSION(I) = -999.
        ELC_E_FISO1(I) = -999.
        ELC_E_FISO2(I) = -999.
        ELC_ET_FISO1(I) = -999.
        ELC_ET_FISO2(I) = -999.
        ELC_NCELLS(I) = -999
        ELC_NCELLS_THRESHOLD(I) = -999
        ELC_E_ISO1(I) = -999.
        ELC_E_ISO2(I) = -999.
        ELC_ET_ISO1(I) = -999.
        ELC_ET_ISO2(I) = -999.
        ELC_TRK_SIG(I) = -999.
        ELC_ETOT_LIKE(I) = -999.
        ELC_TOT_LEFF(I) = -999.
        ELC_IMPACT_XY(I) = -999.
        ELC_IMPACT_Z(I)  = -999.
        ELC_COR_FACTOR(I) = -999.
        ELC_ELIKE_5V(I) = -999.
        ELC_ELIKE_4V(I) = -999.
        ELC_ELIKE_2V(I) = -999.
        ELC_EMV_PRBXY(I) = -999.
        ELC_EMV_PRBRZ(I) = -999.
        ELC_EMV_PRB(I) = -999.
        ELC_NCLOUD(I) = -999
      ENDDO
C
      LPELC=GZPELC()
      IF(LPELC.NE.0) THEN
        NELEC=0
        GOLDELEC = 0
        PELC_VER = 0
C
C         loop through electron banks and pick up to 3rd e
C
        DO WHILE (LPELC.GT.0)
          PELC_VER = IQ(LPELC+1)
          NAME = '    '
          PELC_ADD = -999.
          COR_FACTOR = 0.
          DELTAE = 0.
          OLDE = 0.
          OK=.FALSE.
          TRK=1
          CALL CLEANEM(LPELC,TRK,OK,STATUS)
          CALL CLEANEM_CQUANS(NCVAR,CQUAN)
          CALL CLEANEM_TQUANS(NTVAR,TQUAN)
C
C Get Cloud info
C
          CALL CLEAN_PHOTON_VAR(NPVAR,QUANS)
          IF(NPVAR.GE.15) THEN
            NCLOUD=QUANS(15)
          ELSE
            NCLOUD=-1.
          END IF
C
          NELEC=NELEC+1
C
C Get probabilities
C
          CALL EMVTX(LPELC,XBAR,ZV,RV,PrbRZ,PrbXY,Prb,NVBST)
C
          ET=Q(LPELC+7)
C
C
C ****  Obtain the correction factor for the electron.
C
          CALL QCD_CORRECTEM('e', LPELC, COR_FACTOR)
          IF(COR_FACTOR.LE.0.) THEN
            LVCOR = LQ(LPELC - 4)
            PELC_ADD = LQ( LVCOR - 2 )
            CALL UHTOC(IQ(LVCOR+2),4,NAME,4)
            IF((NAME.EQ.'pelc'.OR.NAME.EQ.'PELC') .AND.
     &          (PELC_ADD .EQ. LPELC) ) THEN
              DELTAE = Q( LVCOR + 6 )
              OLDE = Q(LPELC + 6) - DELTAE
              COR_FACTOR = Q(LPELC + 6)/OLDE
            ELSEIF(PELC_ADD .EQ. LPELC ) THEN
              DELTAE = Q( LVCOR + 6 )
              OLDE = Q(LPELC + 6) - DELTAE
              COR_FACTOR = Q(LPELC + 6)/OLDE
            ELSE
              COR_FACTOR = -999.
            ENDIF
          ENDIF
C
C ****  N. Graf's Golden electron selection
C
          LZTRK = LQ(LPELC-3)             ! Link to associated ZTRAK bank
          TRD_INFO(NELEC) = .TRUE.
          GOLDEN(NELEC) = .FALSE.
          GOLD = .FALSE.
c
          STRICT_ELEC=.TRUE.
          TRK=1
          IF(STRICT_ELEC) THEN
            IF(OK) THEN
              GOLDELEC = GOLDELEC + 1
              GOLDEN(NELEC) = .TRUE.
              GOLD = .TRUE.
            ENDIF
          ELSE
            GOLDELEC = GOLDELEC + 1
            GOLDEN(NELEC) = .TRUE.
          ENDIF
C
          MAXLOG = NELEC
          IF(NELEC.GE.3) MAXLOG = 3
          DO I=1,MAXLOG
            IF(ET.GT.ELC_ET(I)) THEN
              IF(I.LT.3) THEN
                ELC_ET(I+1)  = ELC_ET(I)
                ELC_E(I+1)   = ELC_E(I)
                ELC_EZ(I+1)  = ELC_EZ(I)
                ELC_EX(I+1)  = ELC_EX(I)
                ELC_EY(I+1)  = ELC_EY(I)
                ELC_ETA(I+1) = ELC_ETA(I)
                ELC_PHI(I+1) = ELC_PHI(I)
                ELC_EM_FRAC(I+1) = ELC_EM_FRAC(I)
                ELC_E_FISO1(I+1)   = ELC_E_FISO1(I)
                ELC_E_FISO2(I+1)   = ELC_E_FISO2(I)
                ELC_ET_FISO1(I+1)  = ELC_ET_FISO1(I)
                ELC_ET_FISO2(I+1)  = ELC_ET_FISO2(I)
                ELC_E_ISO1(I+1)    = ELC_E_ISO1(I)
                ELC_E_ISO2(I+1)    = ELC_E_ISO2(I)
                ELC_ET_ISO1(I+1)   = ELC_ET_ISO1(I)
                ELC_ET_ISO2(I+1)   = ELC_ET_ISO2(I)
                ELC_CAL_ETA(I+1)   = ELC_CAL_ETA(I)
                ELC_HMATRIX_CHISQ(I+1)= ELC_HMATRIX_CHISQ(I)
C                ELC_DET_CRACK(I+1)= ELC_DET_CRACK(I)
                ELC_NCELLS(I+1)   = ELC_NCELLS(I)
                ELC_NCELLS_THRESHOLD(I+1) = ELC_NCELLS_THRESHOLD(I)
                ELC_DISPERSION(I+1) = ELC_DISPERSION(I)
                ELC_DEDX(I+1)  = ELC_DEDX(I)
                ELC_VTX_MIP(I+1) = ELC_VTX_MIP(I)
                ELC_TRK_SIG(I+1) = ELC_TRK_SIG(I)
                ELC_ETOT_LIKE(I+1) = ELC_ETOT_LIKE(I)
                ELC_TOT_LEFF(I+1)  = ELC_TOT_LEFF(I)
                ELC_IMPACT_XY(I+1) = ELC_IMPACT_XY(I)
                ELC_IMPACT_Z(I+1)  = ELC_IMPACT_Z(I)
                ELC_COR_FACTOR(I+1) = ELC_COR_FACTOR(I)
                ELC_ELIKE_5V(I+1) = ELC_ELIKE_5V(I)
                ELC_ELIKE_4V(I+1) = ELC_ELIKE_4V(I)
                ELC_ELIKE_2V(I+1) = ELC_ELIKE_2V(I)
                ELC_EMV_PRBXY(I+1) =  ELC_EMV_PRBXY(I)
                ELC_EMV_PRBRZ(I+1) =  ELC_EMV_PRBRZ(I)
                ELC_EMV_PRB(I+1) =  ELC_EMV_PRB(I)
                ELC_NCLOUD(I+1) =  ELC_NCLOUD(I)
              ENDIF
              CALL MVBITS(INT(CQUAN(1)),16,16,STAT_HI,0)
              CALL MVBITS(INT(CQUAN(1)),0,16,STAT_LO,0)
              ELC_ET(I)  =ET
              ELC_E(I)   = Q(LPELC+6)
              ELC_EZ(I)  = Q(LPELC+5)
              ELC_EX(I)  = Q(LPELC+3)
              ELC_EY(I)  = Q(LPELC+4)
              ELC_ETA(I) = Q(LPELC+9)
              ELC_PHI(I) = Q(LPELC+10)
              ELC_EM_FRAC(I) = CQUAN(9)
              ELC_E_FISO1(I)   = CQUAN(13)
              ELC_E_FISO2(I)   = CQUAN(14)
              ELC_ET_FISO1(I)  = CQUAN(15)
              ELC_ET_FISO2(I)  = CQUAN(16)
              ELC_E_ISO1(I)    = CQUAN(23)
              ELC_E_ISO2(I)    = CQUAN(24)
              ELC_ET_ISO1(I)   = CQUAN(25)
              ELC_ET_ISO2(I)   = CQUAN(26)
              ELC_CAL_ETA(I)   = CQUAN(5)
              ELC_HMATRIX_CHISQ(I)=CQUAN(4)
C              ELC_DET_CRACK(I)=CQUAN(20)
              ELC_NCELLS(I)   =CQUAN(21)
              ELC_NCELLS_THRESHOLD(I) = CQUAN(22)
              ELC_DISPERSION(I) = CQUAN(11)
              IF (ABS(ELC_CAL_ETA(I)).LT.13) ELC_DEDX(I)  = TQUAN(13)
              IF (ABS(ELC_CAL_ETA(I)).GT.13) ELC_DEDX(I)  = TQUAN(14)
              ELC_VTX_MIP(I) = TQUAN(15)
              ELC_TRK_SIG(I) =TQUAN(12)
              ELC_ETOT_LIKE(I) = TQUAN(16)
              ELC_TOT_LEFF(I)  = TQUAN(17)
              ELC_IMPACT_XY(I) = TQUAN(20)
              ELC_IMPACT_Z(I)  = TQUAN(21)
              ELC_COR_FACTOR(I) = COR_FACTOR
              ELC_EMV_PRBXY(I) = PRBXY
              ELC_EMV_PRBRZ(I)  = PRBRZ
              ELC_EMV_PRB(I)   = PRB
              ELC_NCLOUD(I)   = NCLOUD

C
C ****  Newly added by Jae Yu on Jan. 4, 1996
C
              IF(PELC_VER.GE.7) THEN
                ELC_ELIKE_5V(I) = Q(LPELC+41)
                ELC_ELIKE_4V(I) = Q(LPELC+42)
                ELC_ELIKE_2V(I) = Q(LPELC+43)
              ELSE
                IF(ABS(ELC_CAL_ETA(I)).LE.12) THEN
                  DUMMY = ELIKE_SET_MASK_CC(MASK_ALL)
                  ELC_ELIKE_5V(I) = ELIKE(LPELC, 0.52, IER)
                  DUMMY = ELIKE_SET_MASK_CC(MASK_TRDOFF)
                  ELC_ELIKE_4V(I) = ELIKE(LPELC, 0.52, IER)
                  DUMMY = ELIKE_SET_MASK_CC(MASK_TRD_DEDX_ONLY)
                  ELC_ELIKE_2V(I) = ELIKE(LPELC, 0.52, IER)
                ELSEIF(ABS(ELC_CAL_ETA(I)).GT.12) THEN
                  DUMMY = ELIKE_SET_MASK_EC(MASK_ALL)
                  ELC_ELIKE_5V(I) = ELIKE(LPELC, 0.62, IER)
                  DUMMY = ELIKE_SET_MASK_EC(MASK_TRDOFF)
                  ELC_ELIKE_4V(I) = ELIKE(LPELC, 0.62, IER)
                  DUMMY = ELIKE_SET_MASK_EC(MASK_TRD_DEDX_ONLY)
                  ELC_ELIKE_2V(I) = ELIKE(LPELC, 0.62, IER)
                ENDIF
              ENDIF
C
              GOTO 100
            ENDIF
          ENDDO
  100     LPELC=LQ(LPELC)          ! pointer to next electron
        ENDDO
C
      ENDIF
      NELC_MAX = NELEC
      IF(NELEC.GE.3) NELEC = 3
C
  999 RETURN
      END
