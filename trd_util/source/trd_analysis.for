      SUBROUTINE TRD_ANALYSIS(LDUMM,ACCEPTANCE,EFFICIENCY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRD analysis routine
C-                         Warning : the output of this routine is very
C-                         sensitive to the corrections, calibrations and
C-                         algorithms selected in TRD_ANALYSIS.RCP !
C-   Inputs  :
C-
C-      LDUMM        integer   link to TRDT,PELC,PPHO or PMUO bank
C-
C-   Outputs :
C-
C-     ACCEPTANCE    logical   overall accptance (geometry, bad runs, ...)
C-
C-     EFFICIENCY    real      electron efficiency
C-                             pion     : efficiency close to 1.
C-                             electron : efficiency in [0.,1.]
C-
C-   Controls: TRD_ANALYSIS.RCP,TRD.RCP
C-
C-   Created   8-APR-1993   Alain PLUQUET
C-   Updated   8-JUN-1994   A. Zylberstejn :use common blocks to transmit
C-                           real_word and integer_word to external world
C-   Updated  29-JUL-1994   A. Zylberstejn  call to routines filling banks TDST
C-                                          and TDANA
C-   Updated  26-OCT-1994  Jean-Francois LEBRAT: modify call to
C-        trd_electron_pion. Call ENERGY_MAX_CELLS TRD_TO_CDC_DEDX
C-        Fill banks TDST & TANA with likelihood variables
C-   Updated  14-DEC-1994   A. Zylberstejn :call TRD_CHECK_INTEGRITY
C-   Updated  16-DEC-1994   Alain PLUQUET  Returns 999 for MC data
C-   Updated  28-DEC-1994   A. Zylberstejn  Check for TRD bad runs
C-   Updated  22-FEB-1995   A. ZYLBERSTEJN  :read Micro-dsts', allow MC data to
C-   be put in banks TANA and TDST
C-   Updated  29-MAR-1995   A. Zylberstejn  Introduce protection against
C-   clobbered zebra structure
C-   Updated   5-MAY-1995   A. ZYLBERSTEJN : put likelihood in bank trdt
C-   Updated  19-JUN-1995   A. Zylberstejn  Check switch in trd.rcp if we want
C-   to rewrite TRDT and TPRL banks
C-   Updated  29-AUG-1995   A. ZYLBERSTEJN  Suppress call to trd_like
C-   Updated  30-AUG-1995   Lewis Taylor Goss  added EPSL_2 courtesy of JFL
C-   Updated   5-OCT-1995   A. Zylberstejn : change place of TDSTFL
C-   Updated   4-APR-1996   LT Goss modify to pass LDUMM
C-   Updated  20-MAR-2004   sss - compile with g77
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INCLUDE 'D0$INC:TRDLIK.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:THIT_INFO.INC'
      INCLUDE 'D0$INC:trd_phi_z.INC'
      INCLUDE 'D0$INC:TRD_DST_ENERGIES.INC'
      INCLUDE 'D0$INC:WORD_IN_TPRL.INC'
C      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INTEGER LENGWS
      PARAMETER (LENGWS=5000)
C      COMMON/WORKSP/WS(LENGWS)
      REAL WS(LENGWS)
      INTEGER IWS(LENGWS)
      EQUIVALENCE(WS,IWS,W(1001))
      INTEGER RECOVERSION,PASS
C
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL     EFFICIENCY,EPICOR(3),EFFICIENCY_LIK,EFFICIENCY_LIK_2
      REAL EPST_COR,EPSL_COR,EPSLP_COR,LIK1_COR,LIK2_COR
      REAL     RW(3,NWORD),VERSION,ENERGY_MAX_CELLS(5),LIK1,LIK2,ECDC
      INTEGER  I,IWD(3,NWORD),DIAGNOSTIC_COR,DIAGNOSTIC_ENV,IER,NA,K
      INTEGER  LTRDT,MULTIPLICITY(3),HITS(3),CD_ACTIVITY,FIRED_CELLS(3)
      INTEGER  GZUDST,LOUT,TRUNIT,LCLUS,LUDST,JBYT,WIRE,SECTOR,IETA
      INTEGER  LCACL_LOCAL,LCACL_TEMP,LPPHO_TEMP,GZPPHO,LDUMM
      INTEGER  LZTRK_LOCAL,RUN_NUM,RUNNO,RUNNI,EVONUM,EVT,EVTI
      REAL CSEC,CELE,ZCATH,EFFI,RECO_VRS
      EQUIVALENCE (IWD,I_IN_TPRL),(RW,R_IN_TPRL)
      LOGICAL  ACCEPTANCE,CORRECTION(10),GEOMETRY(3),DOPRINT,CC,ACCI
      LOGICAL TRD_DO_PRINT,REWRITE,RWB,MC_DATA,TRDINI
      LOGICAL FIRST,BAD_SECTOR,BADTRACK(10),TRD_ANALYSIS_INI
      LOGICAL TRD_CHECK_INTEGRITY,MONTE_CARLO_DATA,TRD_BADRUN
      INTEGER  LTDST,GZTDST,LAYER,LTANA,LTPRL,IFOIS,LDUMMI,TGEO
      INTEGER LTTRH,GZTTRH
      CHARACTER*4 BANK
      REAL TRD_ANAL_CATH
      DATA IFOIS/0/
      DATA FIRST/.TRUE./
      IF(FIRST)THEN ! initialization
        TRDINI =  TRD_ANALYSIS_INI()
        MC_DATA =  IQ(LHEAD+1) .GT. 1000
        CALL EZPICK('TRD_RCP')
        REWRITE=.FALSE.
        CALL EZGET('REWRITE_BANKS',RWB,IER)
        IF(IER.EQ.0)REWRITE=RWB
        CALL EZRSET
        LDUMMI=0
        FIRST=.FALSE.
        LOUT = TRUNIT()
        ACCI=.FALSE.
        RUNNI = 0
        EVTI = 0
        EFFI=999.
      END IF   !end of  initialization
C      DOPRINT=TRD_DO_PRINT()
      DOPRINT=.FALSE.
      IFOIS=IFOIS+1
C      DOPRINT=IFOIS.LE.50
      IF(DOPRINT)WRITE(LOUT,*)' enter trd_analysis, lbank,lbanki',
     &  LDUMM,LDUMMI
      RUN_NUM = RUNNO()
      EVT = EVONUM()
      IF((RUN_NUM.EQ.RUNNI).AND.(EVT.EQ.EVTI).AND.(LDUMM.NE.0).AND.
     &  (LDUMM.EQ.LDUMMI).AND..NOT.MONTE_CARLO_DATA()) THEN ! do not enter
                                           ! twice for the  same TRD "track
        ACCEPTANCE=ACCI
        EFFICIENCY=EFFI
C        PRINT*,' same track in trd_analysis'
        GO TO 999
      END IF
      LDUMMI=LDUMM
      ACCEPTANCE=.FALSE.
      EFFICIENCY=999.
      EFFICIENCY_LIK=999.
      EFFICIENCY_LIK_2=999.
      LIK1=-1000.
      LIK2=-1000.
      IF(LDUMM.LE.0)THEN
C        PRINT*,' in TRD_ANALYSIS, ldumm',ldumm
        CALL ERRMSG('No bank ','trd_analysis',' ', 'W')
        GO TO 999
      END IF
      IF (.NOT.MONTE_CARLO_DATA() .AND. TRD_BADRUN())GO TO 999! discard badruns
      LTDST=GZTDST()
      IF(LTDST.NE.0) CALL MZDROP(IXMAIN,LTDST,' ')
      LUDST=GZUDST()
      IF(DOPRINT)WRITE(LOUT,*)' in TRD_ANALYSIS,lztrk',LZTRK,' ludst',
     &  LUDST
C handle multiple banks
      CALL RECO_VERSION(RECOVERSION,PASS)
      RECO_VRS = FLOAT(RECOVERSION) + 0.01*FLOAT(PASS)
      CALL UHTOC(IQ(LDUMM-4),4,BANK,4)
      IF (BANK.EQ.'PELC') THEN
        LTRDT = 0
        LZTRK_LOCAL = LQ(LDUMM - 3)
        IF (LZTRK_LOCAL.GT.0) LTRDT = LQ(LZTRK_LOCAL - 9)
      ELSEIF (BANK.EQ.'PPHO') THEN
        LTRDT = 0
        LCACL_LOCAL = LQ(LDUMM - 2)
        IF (LCACL_LOCAL.GT.0) LTRDT = LQ(LCACL_LOCAL - 5)
        IF (RECO_VRS.LT.12.2) GOTO 999
      ELSEIF (BANK.EQ.'PMUO') THEN
        LTRDT = 0
        LZTRK_LOCAL = LQ(LDUMM - 5)
        IF (LZTRK_LOCAL.GT.0) LTRDT = LQ(LZTRK_LOCAL - 9)
        IF ((RECO_VRS.LT.12.2).OR.(GZUDST().LT.0)) GOTO 999
      ELSEIF (BANK.EQ.'TRDT') THEN
        LTRDT = LDUMM
      ENDIF
C
      IF (LTRDT.EQ.0.AND.BANK.NE.'TRDT') THEN
        IF ((BANK.EQ.'PELC').OR.(RECO_VRS.GE.12.2.AND.((BANK.EQ.'PPHO')
     &    .OR.(GZUDST().EQ.0.AND.BANK.EQ.'PMUO')))) THEN
          CALL TRD_NUM_LAYERS(LDUMM,GEOMETRY,BADTRACK,TGEO)
          ACCEPTANCE = TGEO.GT.0
          IF (ACCEPTANCE) EFFICIENCY = 1.
          LTTRH = GZTTRH()
          IF (LTTRH.EQ.0) CALL BKTTRH(LTTRH)
          CALL TDSTFL(ACCEPTANCE,EFFICIENCY,EFFICIENCY_LIK,
     &      EFFICIENCY_LIK_2,LIK1,LIK2)
          LTDST = GZTDST()
          Q(LTDST+24) = TGEO
        ENDIF
        GOTO 999
      ENDIF
C
C  +------------+
C  | micro dst's|
C  +------------+
      IF(LUDST.NE.0)THEN
        IF(DOPRINT) WRITE(LOUT,*)' Call get_trd_on_mdst'
        CALL GET_TRD_ON_MDST(LDUMM,IER)
        LTDST = GZTDST()
        ACCEPTANCE =Q(LTDST+7).GT.0.5
        EFFICIENCY =Q(LTDST+8)
        IF(IER.NE.0)THEN
          CALL ERRMSG('error decoding mdst ','trd_analysis',' ', 'W')
          GO TO 999
        END IF
        LTDST=GZTDST()
        GO TO 930
      END IF ! end of test on mdst
C +----------+
C | DST/sta  |
C +----------+
C          IF(DOPRINT)WRITE(LOUT,*)' in TRD_ANALYSIS,lclus ppho',LCLUS
C      IF(DOPRINT)WRITE(LOUT,*)' calor eta',Q(LCLUS+19)
      IETA=-100
      LZTRK_LOCAL = LQ(LTRDT - 4)
      IF(LZTRK_LOCAL.NE.0)THEN ! Check if ZTRK
        IF (BANK.EQ.'TRDT') THEN
          LCLUS = LQ(LZTRK_LOCAL - 4)
        ELSE
          LCLUS = LDUMM
        ENDIF
        IF(DOPRINT)WRITE(LOUT,*)' in TRD_ANALYSIS,lpelc',LPELC
        IF (LCLUS.NE.0) THEN
          IF (IQ(LCLUS-4).EQ.4HPELC)THEN
            IF( .NOT.TRD_CHECK_INTEGRITY(LZTRK_LOCAL)) GOTO 999
            IETA=Q(LCLUS+19)
C            write(lout,*)' lztrk,LQ(LTRDT-4)',lztrk,LQ(LTRDT-4)
            IF(LZTRK.NE.LZTRK_LOCAL)THEN
              CALL ERRMSG(' TRDT bank structure clobbered',
     &          'TRD_ANALYSIS',' ','W')
              GO TO 999
            ENDIF
          ENDIF
        END IF
      ELSE ! No ZTRK check if cacl bank associated to TRD
        IF(LQ(LTRDT-5).LE.0)THEN ! Check if TRD associated to CACL
          CALL ERRMSG('no ZTRK, no CACL ','trd_analysis',' ', 'W')
          GO TO 999
        ELSE
          IF (BANK.EQ.'TRDT') THEN
            LCACL_LOCAL = LQ(LTRDT-5)
            LCACL_TEMP = 0
            LPPHO_TEMP = GZPPHO()
            DO WHILE(LPPHO_TEMP.NE.0.AND.LCACL_LOCAL.NE.LCACL_TEMP)
              LCACL_TEMP = LQ(LPPHO_TEMP - 2)
              IF (LCACL_TEMP.EQ.LCACL) LCLUS = LPPHO_TEMP
              LPPHO_TEMP = LQ(LPPHO_TEMP)
            ENDDO
          ELSE
            LCLUS = LDUMM
          ENDIF
          IF (LCLUS.GT.0) IETA=Q(LCLUS+19)
        END IF ! end of test on CACL
      END IF  ! End of check on ZTRK
      CC=IABS(IETA).LE.12
      IF(DOPRINT)
     &  WRITE(LOUT,*)' in trd_analysis cc',CC,' lztrk',LZTRK,
     &  ' lclus',LCLUS
      CALL VZERO(IWD,NWORD*3)
      CALL VFILL(RW,NWORD*3,0.)
      DO LAYER=1,3
        CALL VZERO(INTEGER_WORD,NWORD)
        CALL VFILL(REAL_WORD,NWORD,0.)
        LTPRL=LQ(LTRDT-LAYER)
        IF (LTPRL.GT.0)
     +      CALL UNPACK_TPRL(LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
        DO I=1,NWORD
          IWD(LAYER,I)=INTEGER_WORD(I)
          RW(LAYER,I)=REAL_WORD(I)
        ENDDO
        IF(DOPRINT)
     +      WRITE(LOUT,*)' in TRD_ANALYSIS,MONTE_CARLO_DATA ',
     &      MC_DATA,' version',VERSION,' nb of anodes',IWD(LAYER,4)
        NA=INTEGER_WORD(4)
        IF(DOPRINT .AND.NA.GE.1)WRITE(LOUT,*)' wire,energy out',
     +       ( IWD(LAYER,50+WIRE),RW(LAYER,50+WIRE),WIRE=1,NA)
C  perform zero substraction for mc_data
        IF(NA.GE.2 .AND.MC_DATA)THEN
          K=0
          DO WIRE=1,NA
            IF(RW(LAYER,50+WIRE).GT. 0.2)THEN
              K=K+1
              RW(LAYER,50+K)=RW(LAYER,50+WIRE)
              REAL_WORD(50+K)=RW(LAYER,50+K)
              INTEGER_WORD(50+K)=IWD(LAYER,50+K)
              IWD(LAYER,50+K)=IWD(LAYER,50+K)
            END IF
          END DO
          NA=K
          INTEGER_WORD(4)=NA
          IWD(LAYER,4)=NA
        END IF
      END DO  ! end of loop on layers
C
      IF(VERSION.LT.4 .OR.MC_DATA)THEN
        IF(DOPRINT)
     +      WRITE(LOUT,*)' Call TRD_DST_COR with na',IWD(1,4),IWD(2,4),
     &      IWD(3,4)
        CALL TRD_DST_COR (LTRDT,VERSION,CORRECTION,ENERGY,
     &      DIAGNOSTIC_COR,RW,IWD,EPICOR)
      ENDIF
      CALL TRD_ENERGY_FIRED_CELLS
     &    (LTRDT,RW,IWD,FIRED_CELLS,ENERGY_FIRED_CELLS)
      CALL TRD_ENVIRONMENT
     &    (LTRDT,VERSION,RW,IWD,
     &    GEOMETRY,MULTIPLICITY,HITS,CD_ACTIVITY,DIAGNOSTIC_ENV)
C      ACCEPTANCE=GEOMETRY(3)
C      IF(DOPRINT)WRITE(LOUT,*)
C     +    ' in trd_analysis,call tdstfl with acceptance ',ACCEPTANCE
      IF(.NOT.MONTE_CARLO_DATA())THEN
C        write(lout,*)' DIAGNOSTIC_COR,DIAGNOSTIC_ENV',DIAGNOSTIC_COR,
C     &    DIAGNOSTIC_ENV
        IF(DIAGNOSTIC_COR.NE.0 .OR. DIAGNOSTIC_ENV.NE.0)GO TO 999
      END IF

      ACCEPTANCE = GEOMETRY(1)
      CALL TDSTFL(ACCEPTANCE,EFFICIENCY,EFFICIENCY_LIK,EFFICIENCY_LIK_2,
     &    LIK1,LIK2)
C     -----------------------------
      IF(.NOT.GEOMETRY(1))GO TO 999
C     -----------------------------
      ACCEPTANCE = GEOMETRY(1).AND.GEOMETRY(3)
      CALL TRD_ENERGY_MAX_CELLS (RW,IWD,ENERGY_MAX_CELLS)
C
      IF(DOPRINT)WRITE(LOUT,*)' in trd_analysis,from trdt',
     &    'electron effic,total energy',Q(LTRDT+14),' epst',Q(LTRDT+15),
     &    ' epsl',Q(LTRDT+16),' etot',Q(LTRDT+4),Q(LTRDT+5),
     &    ' likelihood Etot',Q(LTRDT+6)
      ECDC=0.
      IF (.NOT.MONTE_CARLO_DATA())THEN
        IF(LZTRK.NE.0)CALL TRD_TO_CDC_DEDX (LTRDT,ECDC)
      ELSE      ! Monte-Carlo
        LIK1=-999.
        IF(DOPRINT)
     +      WRITE(LOUT,*)' appel a get_trd_cor_epi i 2nd loop ,ltdst',
     &      LTDST
        IF(LTDST.NE.0)THEN
          IF(DOPRINT)THEN
            DO I=1,3
C            CALL GET_TRD_COR_EPI(I,CORRECTION,IER)
              LTPRL=LQ(LTRDT-I)
              LTANA=LQ(LTDST-I)
              IF(LTANA.GT.0)
     +            WRITE(LOUT,*)' layer',I,' epicor',Q(LTANA+1),
     +            ' ped correction',Q(LTANA+15),' sector',Q(LTANA+21),
     +            ' wire',Q(LTANA+26),' electronic gain',Q(LTANA+29),
     +            'gas correction',Q(LTANA+43),' HV correction',
     +            Q(LTANA+44)
            END DO
          END IF
        END IF
      END IF
      IF(DOPRINT)
     +  WRITE(LOUT,*) ' in TRD_ANALYSIS,acceptance ',ACCEPTANCE
      LTDST=GZTDST()
      IF(LTDST.NE.0)Q(LTDST+24)=0
      IWS(1)=0
      IF(CC)IWS(1)=1
      CALL TRD_ELECTRON_PION
     &      (ENERGY_FIRED_CELLS,ECDC,GEOMETRY,HITS,ACCEPTANCE,
     &      EFFICIENCY,EFFICIENCY_LIK,EFFICIENCY_LIK_2,LIK1,LIK2)
      IF(.NOT.CC)THEN
        IF(LTDST.NE.0) ACCEPTANCE=Q(LTDST+24).NE.0
      END IF
      IF(DOPRINT)
     +     WRITE(LOUT,*)' DST call TRD_ELECTRON_PION efired',
     &     ENERGY_FIRED_CELLS,' epst,epsl,epslp',
     &     EFFICIENCY,EFFICIENCY_LIK,EFFICIENCY_LIK_2
C      CALL TRD_EPS_COR
C     &      (ENERGY_FIRED_CELLS,ECDC,GEOMETRY,HITS,ACCEPTANCE,
C     &      EFFICIENCY,EFFICIENCY_LIK,EFFICIENCY_LIK_2,LIK1,LIK2)
C      IF(DOPRINT)
C     +     WRITE(LOUT,*)' DST call TRD_EPS_COR, efired',
C     &     ENERGY_FIRED_CELLS,' epst,epsl,epslp',
C     &     EFFICIENCY,EFFICIENCY_LIK,EFFICIENCY_LIK_2
C      write(lout,*)' in trd_electron_pion,acceptance',acceptance,' cc',cc
      IF(ACCEPTANCE .AND. .NOT.DOPRINT)GO TO 920
      IF(LTDST.NE.0)THEN
        WRITE(LOUT,*)' exit trd_analysis,acceptance',
     &      ACCEPTANCE,' epst',EFFICIENCY,' tgeo',Q(LTDST+24),
     &      'cc',CC, ' geometry',GEOMETRY,'DIAGNOSTIC_COR',
     &      DIAGNOSTIC_COR,' DIAGNOSTIC_ENV',DIAGNOSTIC_ENV
      ELSE
        WRITE(LOUT,*)' exit trd_analysis,acceptance',
     &      ACCEPTANCE,' epst',EFFICIENCY,' tgeo =0',' cc',CC,
     &      ' geometry',GEOMETRY,'DIAGNOSTIC_COR',
     &      DIAGNOSTIC_COR,' DIAGNOSTIC_ENV',DIAGNOSTIC_ENV
      END IF
C      END IF
  920 CONTINUE
C put likelihood and efficiencies in banks  (4-may-1995),
      LTDST=GZTDST()
      IF(LTDST.LE.0)THEN
        CALL ERRMSG('no TDST bank created ','trd_analysis',
     &      'TRDT bank exits ', 'W')
        GO TO 999
      END IF
      Q(LTDST+7)=0.
      IF (ACCEPTANCE) Q(LTDST+7)=1.
C      IF (LUDST.EQ.0)THEN
      IF(DOPRINT)
     +    WRITE(LOUT,*)' exit trd_analysis avec efire',Q(LTDST+12),
     +    Q(LTDST+13), Q(LTDST+14),' eps',EFFICIENCY,EFFICIENCY_LIK,
     +    EFFICIENCY_LIK_2
      IF(DOPRINT)WRITE(LOUT,*)'eps cor',Q(LTDST+25),Q(LTDST+26),EPSLP
      Q(LTRDT+8)=Q(LTDST+11)
      Q(LTRDT+9)=Q(LTDST+10)
      Q(LTRDT+17)=Q(LTDST+20)
      Q(LTRDT+19)=Q(LTDST+9)
  930 CONTINUE
      LTDST=GZTDST()
      IF(LTDST.LE.0)THEN
        CALL ERRMSG('no TDST bank created ','trd_analysis',
     &      'TRDT bank exits ', 'W')
        GO TO 999
      END IF
      IF(DOPRINT)
     +      WRITE(LOUT,*)' epst apres trd_analysis',Q(LTDST+8),
     &      'energy Fired cell',Q(LTDST+12),Q(LTDST+13),
     &      Q(LTDST+14),' epsl',Q(LTDST+9)
      IF(REWRITE .AND.VERSION.LT.4.) CALL TRD_REWB(LTRDT)
      DO LAYER=1,3 ! compute Z from cathodes and phi track
        LTPRL=LQ(LTRDT-LAYER)
        IF(LTPRL.NE.0)THEN
          IF(IQ(LTPRL+24).EQ.0)THEN
            ZCATH=TRD_ANAL_CATH(LTRDT,LAYER)
C            WRITE(LOUT,*)' TRD_ANAL_CATH(LTRDT,LAYER)',ZCATH
            IQ(LTPRL+24)=2000
            IF(ABS(ZCATH).LT.100.) IQ(LTPRL+24)=ZCATH*100.
C              write(lout,*)' in trd_analysis,z cath',q(ltana+300)
          END IF
          LTANA=LQ(LTDST-LAYER)
          Q(LTANA+300)=ZCATH
          IF(Q(LTANA+298).LE.0.)THEN
            Q(LTANA+297)=PHI_TRD(LAYER)
            Q(LTANA+298)=R_TRD(LAYER)
            Q(LTANA+299)=Z_TRD(LAYER)
          END IF ! end of test on TDST
        END IF ! end of test on TPRL
      END DO
  999 CONTINUE
      ACCI=ACCEPTANCE
      EFFI=EFFICIENCY
      RUNNI=RUN_NUM
      EVTI = EVT
      IF(DOPRINT)
     +    WRITE(LOUT,*)
     +    ' exit trd_analysis, LTRDT,ACCEPTANCE,EFFICIENCY',
     &    LTRDT,ACCEPTANCE,EFFICIENCY
      RETURN
      END
