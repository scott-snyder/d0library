      SUBROUTINE TRD_ELECTRON_PION(ENERGY_FIRED_CELLS,ECDC,GEOMETRY,
     &  HITS,ACCEPTANCE,EPST,EPSL,EPSL_2,LIK1,LIK2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-
C- ENERGY_FIRED_CELLS  energies in MIP
C-                     ENERGY_FIRED_CELLS(1) fired cell energy lay 1
C-                     ENERGY_FIRED_CELLS(2) fired cell energy lay 2
C-                     ENERGY_FIRED_CELLS(3) fired cell energy lay 3
C-                     ENERGY_FIRED_CELLS(4) total fired cell energy
C-                     ENERGY_FIRED_CELLS(5) trunc fired cell energy
C-                   etrunc = sum lowest 2 layer energies (3 lyr track)
C-                          = (3/4)*(sum both layer energies) (2 lyr track)
C-                          = (3/2)*(layer 1 energy) (1 lyr track)
C-
C-
C-     ECDC            dE/dx in CDC in MIP
C-
C-     HITS            number of hits in layer 1,2,3
C-
C-     GEOMETRY        .TRUE. if layer 1,2,3 crossed by the track
C-
C-     ACCEPTANCE      .TRUE. if GEOMETRY(i) .TRUE. for i=1,2,3
C-                          overwritten to be true for 1 and 2 layer tracks
C-
C-  Outputs:
C-
C-     EPST            efficiency that excludes the candidate
C-                     theoritically close to 1. for pions
C-                                   in [0.,1.]  for electrons
C-                     - Computed via truncated energy -
C-
C-     EPSL            efficiency that excludes the candidate
C-                     theoritically close to 1. for pions
C-                                   in [0.,1.]  for electrons
C-                     - Computed via TRD + CDC Likelihood -
C-
C-     EPSL_2          efficiency that excludes the candidate
C-                     theoritically close to 1. for pions
C-                                   in [0.,1.]  for electrons
C-                     - Computed via TRD only Likelihood -
C-
C-     LIK1            likelihood computed with TRD & CDC
C-
C-     LIK2            likelihood computed with TRD only
C-
C-   Controls:
C-
C-   Created  19-APR-1993   Alain PLUQUET
C-   Modified 26-OCT-1994   Jean-Francois LEBRAT:
C- algorithm to compute the efficiency out of a TRD+CDC
C- likelihood method. Efficiency close to 1 for pions and in [0.,1.]
C- for electrons
C-   Updated  23-NOV-1994   A. ZYLBERSTEJN   cleaning
C-   Updated   3-MAY-1995   A. ZYLBERSTEJN : protect against de/dx=0.
C-   Updated  11-MAY-1995   A. Zylberstejn : protect against lik1 too small
C-                                           or too big
C-
C-   Updated  16-MAY-1995   JF Lebrat        protects against probabilities
C-                                           equal to zero unprotect against
C-                                           de/dx=0.
C-   Updated  29-AUG-1995   A. Zylberstejn   : include calculation of
C-                                             likelihood per layer
C-   Updated  29-AUG-1995   Lewis Taylor Goss  add EPSL_2 (courtesy of
C-                                             Jean-Francois Lebrat)
C-   Updated  15-SEP-1995   A. ZYLBERSTEJN   :suppress test on acceptance to
C-   compute likelihood per layer; suppress test on ZTRK to compute epsl'
C-   Updated  Dec-10-1995   Bob Kehoe   -- add forward tracks via call to
C-                                          epsilon_t_end
C-   Updated   9-JAN-1996   A. ZYLBERSTEJN : Include MC calculations
C-   Updated  Jan-26-1996   Bob Kehoe - implement entry-point num_layers. apply
C-                                      theta cut to acceptance applied by reco
C-                                      to hits.
C-   Updated  21-MAR-1996   A. ZYLBERSTEJN: compute eps's without Temp.
C-                                          correction
C-   Updated   6-SEP-1996   A. Zylberstejn  : introduce YD calculations
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRDLIK.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C      COMMON/LIK_PI_EL/ LIK1PI ,LIK1EL,LIK2PI ,LIK2EL,LIK_ION_PI,
C     &  LIK_ION_EL
C      REAL LIK1PI ,LIK1EL,LIK2PI ,LIK2EL
C      REAL LIK_ION_PI,LIK_ION_EL
      INCLUDE 'D0$INC:trd_phi_z.inc'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INTEGER LENGWS
      PARAMETER (LENGWS=5000)
C      COMMON/WORKSP/WS(LENGWS)
      REAL WS(LENGWS)
      INTEGER IWS(LENGWS)
      EQUIVALENCE(WS,IWS,W(1001))
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZTPHY.LINK'
      INCLUDE 'D0$LINKS:IZT1A0.LINK'
      INCLUDE 'D0$LINKS:IZT1B0.LINK'
      INCLUDE 'D0$LINKS:IZTLIK.LINK'
      INCLUDE 'D0$LINKS:IZTPIO.LINK'
      INCLUDE 'D0$LINKS:IZTELE.LINK'
      INCLUDE 'D0$LINKS:IZTPI1.LINK'
      INCLUDE 'D0$LINKS:IZTPI2.LINK'
      INCLUDE 'D0$LINKS:IZTPDE.LINK'
      INCLUDE 'D0$LINKS:IZTEL1.LINK'
      INCLUDE 'D0$LINKS:IZTEL2.LINK'
      INCLUDE 'D0$LINKS:IZTEDE.LINK'
      INCLUDE 'D0$LINKS:IZTLTA.LINK'
      INTEGER VERSION, PASS,LTDST,GZTDST,GZTCOR,LTCOR
      REAL ENERGY_FIRED_CELLS(5),EPST,THRESHOLD,EPSL,EPSL_2
      REAL ECDC,A,B,ETRUNC_REF_MC,LIKE_REF_MC
      REAL EPSTR(2),EPSLR(2),EPSLR_2(2),LIK1R(2),LIK2R(2),LL(3,2)
      REAL ENERGY(3), THETA_PRIME, RECO_VRS,CORT,TRD_COR_TEMP
      REAL THETA_PRIME2
      REAL ECDC_DENS
      LOGICAL ACCEPTANCE,GEOMETRY(3),DOPRINT,MC_DATA,TRD_DO_PRINT
      LOGICAL ACCEPTANCE_3 ! normal three-layer acceptance
      LOGICAL DO_TEMP_CORRECTION,RUN1C,PERFORM_T_COR
      LOGICAL ERROR_LIKE,ACCI,NOCORT
C----------------------------------------------------------------
C truncated energy fired cells  0=underflows/150 bins/151=overflows
C----------------------------------------------------------------
      REAL TEFC_H111(0:151),TEFC_H112(0:151)
      REAL TEFC_H122(0:151),TEFC_H222(0:151)
      REAL TEFC(0:151,4)
      EQUIVALENCE (TEFC(0,1),TEFC_H111(0)),(TEFC(0,2),TEFC_H112(0)),
     &  (TEFC(0,3),TEFC_H122(0)),(TEFC(0,4),TEFC_H222(0))
      REAL ONE(0:151),XMAX,FINAL_ENERGY
      INTEGER I,IER,J,K,HITS(3),ICAS,RUNNO,RUNI,NAT,LVMAX,LOUT,TRUNIT
      INTEGER LTP,LTE,LTPHY,POSITION,LT,NATRUN,NERR1,NERR2,IFOIS
      INTEGER LZTRKI,LPELCI,HIT(3),TGEO,LYR_ACC,LOC,CASE,CASEM
C----------------------------------------------------------------
C TRD + CDC combined likelihood
C----------------------------------------------------------------
      INTEGER LTPIO,LTELE,LTPI1,LTPI2,LTPDE,LTEL1,
     &        LTEL2,LTEDE,LTLTA
      REAL    PRO_EL,PRO_EL2,PRO_PI,PRO_PI2,PRO_PICDC,PRO_ELCDC,
     &        PRO_PIL1,PRO_PIL2,PRO_PIL3,PRO_ELL1,PRO_ELL2,PRO_ELL3,
     &        LIK1,LIK2,E1,E2,E3
      LOGICAL FIRST,RUN1A,GOOD_ENERGIES,ALL_LAYERS,DOCCVTXONLY
C----------------------------------------------------------------
C EPSL_DENS TRD+CDC (MINI-BIAS,ELECTRON, CONVERSION)
C----------------------------------------------------------------
      INTEGER NUVW,NU,NV,NW
      REAL XXMIN,XXMAX,YYMIN,YYMAX,ZZMIN,ZZMAX
      REAL XU(41),YUV(41,41),ZUVW(41,41,21)
      INTEGER INDEX(32000)
      INTEGER NPUVW,NPU,NPV,NPW,ILAY
      REAL XXPMIN,XXPMAX,YYPMIN,YYPMAX,ZZPMIN,ZZPMAX
      REAL XPU(41),YPUV(41,41),ZPUVW(41,41,21)
      INTEGER INDEXP(32000)
      REAL VRAIS_PI,VRAIS_EL,VRAIS_FA,VRAIS_CO
      REAL VRAIS_PIP,VRAIS_ELP,VRAIS_FAP,VRAIS_COP
      REAL PAS(4),EN(4)
      REAL XXX,RNDM
      LOGICAL DO_DENS_ANALYSIS
      INTEGER NA(3),MUVW,LUVW_DENS
      DATA FIRST/.TRUE./
      DATA NATRUN/0/,RUNI/0/,NERR1/0/,NERR2/0/,THRESHOLD/0.2/
      DATA IFOIS/0/
      IF (FIRST) THEN
        FIRST = .FALSE.
        LOUT = TRUNIT()
        CALL EZLOC ('TRD_ANALYSIS_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_ANALYSIS_RCP',IER)
        CALL EZPICK ('TRD_ANALYSIS_RCP')
        CALL EZGET('DOCCVTXONLY',DOCCVTXONLY,IER)
        CALL EZGET('DO_TEMP_CORRECTION',DO_TEMP_CORRECTION,IER)
        DO_DENS_ANALYSIS=.FALSE.
        CALL EZLOC ('TRD_ANALYSIS_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_ANALYSIS_RCP',IER)
        CALL EZPICK ('TRD_ANALYSIS_RCP')
        CALL EZGET('DO_DENS_ANALYSIS',DO_DENS_ANALYSIS,IER)
        CALL EZRSET
        MC_DATA = IQ(LHEAD+1) .GT. 1000
        LTCOR=GZTCOR()
        NOCORT=GZTCOR().EQ.0 .OR. MC_DATA
        IF(DO_DENS_ANALYSIS)
     #    CALL NEURO_DENS(INDEX,XXMIN,XXMAX,YYMIN,YYMAX,ZZMIN,ZZMAX,
     #    NUVW,NU,NV,NW,XU,YUV,ZUVW,
     #    INDEXP,XXPMIN,XXPMAX,YYPMIN,YYPMAX,ZZPMIN,ZZPMAX,
     #    NPUVW,NPU,NPV,NPW,XPU,YPUV,ZPUVW)
C
      ENDIF      !end of initialization

C      IF(DOPRINT .OR. LZTRK.EQ.LZTRKI)
C     +   WRITE(LOUT,*)' enter trd_electron_pion geometry',GEOMETRY,
C     +   'lztrk,lztrki',LZTRK,LZTRKI,' lpelc,lpelci',LPELC,LPELCI,
C     +   ' ifois',IFOIS
      LZTRKI=LZTRK
      LPELCI=LPELC
      ERROR_LIKE=.FALSE.
      IF (LTGEN.LE.0) THEN
        CALL ERRMSG(' TRD_ELECTRON_PION','TRD_ELECTRON_PION',
     &        'Bank TGEN not found','W')
        ERROR_LIKE=.TRUE.
      ENDIF
C
C----------------------------------------------------------------
C
      IF (RUNNO().NE.RUNI) THEN
        RUNI = RUNNO()
        RUN1C=RUNI.GE.94478
        PERFORM_T_COR=DO_TEMP_CORRECTION.OR.RUN1C
        NAT  = 1
        IF(.NOT.RUN1A()) NAT = 2
        LTPHY = LC(LTGEN-IZTPHY)
        IF (LTPHY.LE.0) THEN
          CALL ERRMSG(' TRD_ELECTRON_PION','TRD_ELECTRON_PION',
     &          'Bank TPHY not found','W')
          GO TO 999
        ENDIF
        IF (IC(LTPHY-2).LE.3) NAT = 1
        IF (NAT.NE.NATRUN) THEN
          NATRUN = NAT
          IF (NAT.EQ.1) THEN
            LT = LC(LTPHY-IZT1A0)
            CALL INTMSG(' USE RUN1A TABLES FOR EPST')
          ELSE
            LT = LC(LTPHY-IZT1B0)
            CALL INTMSG(' USE RUN1B TABLES FOR EPST')
          END IF
          IF (LT.LE.0) THEN
            CALL ERRMSG(' TRD_ELECTRON_PION','TRD_ELECTRON_PION',
     &          'Bank T1A0 not found','W')
            GO TO 999
          ENDIF
          IF(.NOT.MC_DATA)THEN
            XMAX = C(LT+7)/150.
            DO J = 0,151
              TEFC_H111(J) = FLOAT(IC(LT+J+12))
              TEFC_H112(J) = FLOAT(IC(LT+J+165))
              TEFC_H122(J) = FLOAT(IC(LT+J+318))
              TEFC_H222(J) = FLOAT(IC(LT+J+471))
              ONE(J)       = 1.
            ENDDO
            DO J = 1,151
              TEFC_H111(J) = TEFC_H111(J-1)+TEFC_H111(J)
              TEFC_H112(J) = TEFC_H112(J-1)+TEFC_H112(J)
              TEFC_H122(J) = TEFC_H122(J-1)+TEFC_H122(J)
              TEFC_H222(J) = TEFC_H222(J-1)+TEFC_H222(J)
            ENDDO
            CALL VSCALE (TEFC_H111,1./TEFC_H111(151),TEFC_H111,152)
            CALL VSCALE (TEFC_H112,1./TEFC_H112(151),TEFC_H112,152)
            CALL VSCALE (TEFC_H122,1./TEFC_H122(151),TEFC_H122,152)
            CALL VSCALE (TEFC_H222,1./TEFC_H222(151),TEFC_H222,152)
            CALL VSUB (ONE,TEFC_H111,TEFC_H111,152)
            CALL VSUB (ONE,TEFC_H112,TEFC_H112,152)
            CALL VSUB (ONE,TEFC_H122,TEFC_H122,152)
            CALL VSUB (ONE,TEFC_H222,TEFC_H222,152)
          END IF ! end of test on MC
        END IF
      END IF
C
C      if(ifois.le.10)
C      write(lout,*)' in trd_electron_pion acceptance',
C     &  acceptance
C set values of parameters when track does not cross layer1
      TGEO         = 0
      LIK1         =-1000.
      LIK2         =-1000.
      LIK2         =-1000.

      LIKE_LAYER(1)=-1000.
      LIKE_LAYER(2)=-1000.
      LIKE_LAYER(3)=-1000.
      EPST         =  999.
      EPSL         =  999.
      EPSL_2       =  999.
C
      LTLIK = LC(LTGEN-IZTLIK)
C
      IF (LTLIK.LE.0) THEN
        CALL ERRMSG(' STP bank error ','TRD_ELECTRON_PION',
     &          'Bank TLIK not found','w')
      ELSE ! test on links hanging from TLIK
        LTPIO = LC(LTLIK-IZTPIO)
        LTELE = LC(LTLIK-IZTELE)
        LTLTA = LC(LTLIK-IZTLTA)
        IF (LTPIO.LE.0 .OR. LTELE.LE.0 .OR. LTLTA.LE.0) THEN
          CALL ERRMSG(' STP bank error ','TRD_ELECTRON_PION',
     &          'Bank TELE or TPIO or TLTA not found','w')
          ERROR_LIKE=.TRUE.
        ELSE ! test on links hanging from tpio
          LTPI1 = LC(LTPIO-IZTPI1)
          LTPI2 = LC(LTPIO-IZTPI2)
          LTPDE = LC(LTPIO-IZTPDE)
          IF (LTPI1.LE.0.OR.LTPI2.LE.0.OR.LTPDE.LE.0.) THEN
            CALL ERRMSG(' STP bank error','TRD_ELECTRON_PION',
     &          'Bank TPI1 or TPI2 or TPDE not found','w')
            ERROR_LIKE=.TRUE.
          ELSE ! test on links hanging from tele
            LTEL1 = LC(LTELE-IZTEL1)
            LTEL2 = LC(LTELE-IZTEL2)
            LTEDE = LC(LTELE-IZTEDE)
            IF (LTEL1.LE.0.OR.LTEL2.LE.0..OR.LTEDE.LE.0.) THEN
              CALL ERRMSG(' STP bank error ','TRD_ELECTRON_PION',
     &          'Bank TEL1 or TEL2 or TEDE not found','w')
              ERROR_LIKE=.TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
      DO CASE =1,2
        EPSLR(CASE)         =  999.
        EPSLR_2(CASE)       =  999.
        EPSTR(CASE)         =  999.
        LIK1R(CASE)         =-1000.
        LIK2R(CASE)         =-1000.
        LL(1,CASE)=-100.
        LL(2,CASE)=-200.
        LL(3,CASE)=-400.
      END DO
      LIK2PI = -1000.
      LIK2EL = -1000.
      LIK1PI = -1000.
      LIK1EL = -1000.
      DOPRINT=TRD_DO_PRINT()
C      DOPRINT=IFOIS.LE.5
C     &             .or. HITs(1).LE.0 .OR.HITS(2).LE.0 .OR.HITS(3).LE.0
C      IF(DOPRINT)
C      WRITE(LOUT,3454)HITS, GEOMETRY,ACCEPTANCE
 3454 FORMAT(' in trd_electron_pion hits',3I2,' geometry',
     +    3L3,' acceptance',L3)
C From now on, assume GEOMETRY(1)=true
C     -----------------------------
      IF(.NOT.GEOMETRY(1))GO TO 999
C     -----------------------------
      LIKE_LAYER(1)=-100.
      HIT(1)=MIN0(HITS(1),2)
      HIT(2)=MIN0(HITS(2),2)
      HIT(3)=MIN0(HITS(3),2)
C----------------------------------------------------------------
C   truncated energy fired cells method (Alain Pluquet)
C----------------------------------------------------------------
      IFOIS=IFOIS+1
C      IF(DOPRINT)
C     +    WRITE(LOUT,*)' in trd_electron_pion hits',HITS,
C     &    ' acceptance',ACCEPTANCE,' e fired',ENERGY_FIRED_CELLS
      ALL_LAYERS=HIT(1).GT.0 .AND. HIT(2).GT.0 .AND. HIT(3).GT.0
      CORT=TRD_COR_TEMP()
      CASEM=2
      IF(ABS(CORT-1.).LT.0.01 .OR. NOCORT .OR. RUN1A())CASEM=1
      ACCEPTANCE_3 = ACCEPTANCE
      ACCI=ACCEPTANCE
C      if(doprint)write(lout,*)
C     +' before 500, cort',CORT,' casem',CASEM
      DO 500 CASE=1,CASEM !case=1: old energies, =2: decorrection for temp.
        CALL VSCALE(ENERGY_FIRED_CELLS,FLOAT(CASE-1)*CORT+FLOAT(2-CASE),
     &                                                         ENERGY,3)
        GOOD_ENERGIES= ENERGY(1).GE.THRESHOLD .AND.
     &                 ENERGY(2).GE.THRESHOLD .AND.
     &                 ENERGY(3).GE.THRESHOLD
C        if(doprint)
C     +   write(lout,*)' case',CASE,' energies',ENERGY,' good_energies',
C     &    GOOD_ENERGIES,' acceptance',ACCI
        IF (ACCI) THEN
          TGEO = 3
          EPSTR(CASE)   =   1. ! 1 OR MORE LAYERS ARE MISSING ==> PI
          IF (GOOD_ENERGIES.AND.ALL_LAYERS) THEN
            IF(CASE.EQ.1)THEN
              I=2
              J=3
              K=LVMAX(ENERGY,3)
              IF(K.EQ.2)THEN
                I=1
                J=3
              ELSE IF(K.EQ.3)THEN
                I=1
                J=2
              END IF
            END IF
            FINAL_ENERGY= ENERGY(I)+ ENERGY(J)
C            PRINT*,' case',CASE,
C     &            ' final_energy',FINAL_ENERGY,' i,j',I,J
            IF(MC_DATA)THEN
              EPSTR(CASE)=ETRUNC_REF_MC(FINAL_ENERGY,HIT)
C              IF(DOPRINT)
C     +          WRITE(LOUT,*)' e_fired',ENERGY, 'i,j min',
C     +          I,J,'   epst',EPSTR(CASE),' nanod',HIT,' lztrk,lpelc',
C     +          LZTRK, LPELC
            ELSE
              POSITION=IFIX(FINAL_ENERGY/XMAX)
              IF (POSITION.LT.1)   POSITION=0
              IF (POSITION.GT.150) POSITION=151
              ICAS=1
C            IF (HIT(I).EQ.1.AND.HIT(J).EQ.1)ICAS=1
C            IF(HIT(I).GE.2 .OR. HIT(J).GE.2)ICAS=3! case 1-2
C            IF(HIT(I).GE.2 .and. HIT(J).GE.2)ICAS=4! case 2-2
              IF ((HIT(1).GE.2.AND.HIT(2).EQ.1.AND.HIT(3).EQ.1).OR.
     &          (HIT(1).EQ.1.AND.HIT(2).GE.2.AND.HIT(3).EQ.1).OR.
     &          (HIT(1).EQ.1.AND.HIT(2).EQ.1.AND.HIT(3).GE.2))ICAS=2!1-1-2
              IF ((HIT(1).EQ.1.AND.HIT(2).GE.2.AND.HIT(3).GE.2).OR.
     &          (HIT(1).GE.2.AND.HIT(2).EQ.1.AND.HIT(3).GE.2).OR.
     &          (HIT(1).GE.2.AND.HIT(2).GE.2.AND.HIT(3).EQ.1))ICAS=3!1-2-2
              IF (HIT(1).GE.2.AND.HIT(2).GE.2.AND.HIT(3).GE.2)ICAS=4!2-2-2
C              PRINT*,' case',CASE,' position',POSITION,' icas',ICAS,
C     &            ' final_energy',FINAL_ENERGY,' i,j',I,J
              IF (POSITION.GE.2.AND.POSITION.LE.150) THEN
                EPSTR(CASE)= TEFC(POSITION,ICAS)+
     &            (TEFC(POSITION,ICAS)-TEFC(POSITION-1,ICAS))*
     &            (FINAL_ENERGY /XMAX - FLOAT(POSITION))
              ELSE
                EPSTR(CASE) = TEFC(POSITION,ICAS)
              ENDIF ! end of test on position
C              IF(DOPRINT)
C     +          WRITE(LOUT,*)' in trd_electron_pion case',CASE,
C     +          ' epst',EPSTR(CASE)
            END IF ! end of test on MC
          ENDIF    ! end of test on good energies
        ELSE  ! acceptance false
C          if(doprint)
C     +     WRITE(LOUT,*)'acceptance false, geometry(3)',GEOMETRY(3),
C     &      ' iws(1)',IWS(1),' DOCCVTXONLY ',DOCCVTXONLY
          IF(.NOT.DOCCVTXONLY.AND.IWS(1).NE.0) THEN
            EPSTR(CASE) = 999.
            GO TO 400 ! exit for CC tracks
          ENDIF
C-        *** bk, for < 3lyr calc. trunc.-sum with available layers and
C-        *** determine epsilon_t for track.  set tgeo for num_layers.
C          WRITE(LOUT,*)' case',CASE,' epst before EPSILON_T_END',
C     &      EPSTR(CASE)
          CALL EPSILON_T_END(ENERGY,HITS,GEOMETRY,EPSTR(CASE),
     &      FINAL_ENERGY, TGEO,IER)
C          WRITE(LOUT,*)' espst after EPSILON_T_END',EPSTR(CASE),' tgeo',
C     &      TGEO
C          IF(CASE.EQ.1)THEN
          IF(IER.LT.0)TGEO=0
          IF(TGEO.NE.0)THEN
            IF(CASE.EQ.2 .AND. .NOT.PERFORM_T_COR)GO TO 3204
            ENERGY_FIRED_CELLS(4) = FINAL_ENERGY*(2.0/3.0)*FLOAT(TGEO)
            ENERGY_FIRED_CELLS(5) = FINAL_ENERGY
            ACCEPTANCE = .TRUE.
 3204       CONTINUE
          END IF
        END IF ! end of test of acceptance
C-        *** bk, for reco v12.20 and earlier, reject tracks with angle within
C-        *** 15-degrees of beampipe. for reco >= 12.21, cut is 8-degrees.
        CALL RECO_VERSION(VERSION,PASS)
        RECO_VRS = FLOAT(VERSION) + 0.01*FLOAT(PASS)
        THETA_PRIME2 = ABS((PI/2.0)-THETA_TRD(1))
        IF ((RECO_VRS.LE.12.2.AND.THETA_PRIME2.GE.1.31).OR.
     &    (RECO_VRS.GT.12.2.AND.THETA_PRIME2.GE.1.43)) THEN
          ACCEPTANCE = .FALSE.
          ACCEPTANCE_3 = .FALSE.
          TGEO = 0
          EPSTR(CASE) = 999.
        ENDIF
C
C------------------------------------
C   likelihood (Jean-Francois LEBRAT)
C------------------------------------
        IF(ERROR_LIKE)GO TO 400
        E1 = AMIN1(ENERGY(1),14.9)
        E2 = AMIN1(ENERGY(2),14.9)
        E3 = AMIN1(ENERGY(3),14.9)
C  layer 1
        IF(HIT(1).NE.0 .AND.E1.GE.THRESHOLD )THEN
          LTP=LTPI1*(2-HIT(1))+LTPI2*(HIT(1)-1)
          LTE=LTEL1*(2-HIT(1))+LTEL2*(HIT(1)-1)
          PRO_PIL1 = C(LTP+INT(E1*6.6667+11))/C(LTP+8)
          PRO_ELL1 = C(LTE+INT(E1*6.6667+11))/C(LTE+8)
          IF (PRO_PIL1.EQ.0.) PRO_PIL1 = 1. / C(LTP+8)
          IF (PRO_ELL1.EQ.0.) PRO_ELL1 = 1. / C(LTE+8)
C          LIKE_LAYER(1) = LOG(PRO_ELL1/PRO_PIL1)
          LL(1,CASE) = LOG(PRO_ELL1/PRO_PIL1)
C          PRINT*,' case',CASE,' like_layer1',LL(1,CASE)
        END IF
C  layer 2
        IF(GEOMETRY(2))THEN
          LL(2,CASE)=-200.
          IF(HIT(2).NE.0 .AND. E2.GE.THRESHOLD)THEN
            LTP=LTPI1*(2-HIT(2))+LTPI2*(HIT(2)-1)
            LTE=LTEL1*(2-HIT(2))+LTEL2*(HIT(2)-1)
            PRO_PIL2 = C(LTP+INT(E2*6.6667+111)) / C(LTP+9)
            PRO_ELL2 = C(LTE+INT(E2 *6.6667+111)) / C(LTE+9)
            IF (PRO_PIL2.EQ.0.) PRO_PIL2 = 1. / C(LTP+9)
            IF (PRO_ELL2.EQ.0) PRO_ELL2 = 1. / C(LTE+9)
C            LIKE_LAYER(2) = LOG(PRO_ELL2/PRO_PIL2)
            LL(2,CASE) = LOG(PRO_ELL2/PRO_PIL2)
          END IF
C  layer 3
          IF(GEOMETRY(3))THEN
            LL(3,CASE)=-400.
            IF(HIT(3).NE.0 .AND. E3.GE.THRESHOLD)THEN
              LTP=LTPI1*(2-HIT(3))+LTPI2*(HIT(3)-1)
              LTE=LTEL1*(2-HIT(3))+LTEL2*(HIT(3)-1)
              PRO_PIL3 = C(LTP+INT(E3*6.6667+211))/C(LTP+10)
              PRO_ELL3 = C(LTE+INT(E3*6.6667+211))/C(LTE+10)
              IF (PRO_PIL3.EQ.0.) PRO_PIL3 = 1. /C(LTP+10)
              IF (PRO_ELL3.EQ.0.) PRO_ELL3 = 1. /C(LTE+10)
C              LIKE_LAYER(3) = LOG(PRO_ELL3/PRO_PIL3)
              LL(3,CASE) = LOG(PRO_ELL3/PRO_PIL3)
            END IF
            LIK2R(CASE)=LL(1,CASE)+LL(2,CASE)+LL(3,CASE)
          ELSE! geometry(3) false, geometry(2) true
            LIK2R(CASE)=LL(1,CASE)+LL(2,CASE)
            IF(LL(1,CASE).GT.-50. AND. LL(2,CASE).GT.-50.)
     +          LIK2R(CASE)=1.5*(LL(1,CASE)+LL(2,CASE))
          END IF ! end of test of geometry 3
        ELSE !geometry(2) false geometry(1) true
          LIK2R(CASE)=LL(1,CASE)
          IF(LL(1,CASE).GT.-50.)LIK2R(CASE)=3.*LIK2R(CASE)
        END IF! end of test on geometry 2
C
C    compute epsl,epsl'
C
C
C        IF(DOPRINT)
C     +      WRITE(LOUT,*)' in trD_ELECTRON_PION, like_layer',LIKE_LAYER
C        IF(.NOT.ACCEPTANCE_3)GO TO 400 ! *** bk, need acceptance_3 for CDC
C ONE TRD LAYER MISSING -> PION
        EPSLR(CASE)   =   1.
        EPSLR_2(CASE) =   1.
        LIK1R(CASE)   = -99.
C          IF(doprint)
C     +      WRITE(LOUT,*)' e1,e2,e3',E1,E2,E3,
C     +      ' threshold',THRESHOLD
        IF (.NOT. GOOD_ENERGIES .OR. .NOT.ALL_LAYERS)GO TO 400
        IF (.NOT.MC_DATA .AND. LQ(LZTRK-7).NE.0) THEN ! request CDC track
C                                            TRD+CDC likelihood and epsl
          ECDC_DENS=ECDC
          IF (ECDC.GE.4.) ECDC = 3.99
          PRO_PICDC = C(LTPDE+INT(ECDC*25.+11)) / C(LTPDE+8)
          IF (PRO_PICDC.EQ.0) PRO_PICDC = 1. / C(LTPDE+8)
          PRO_ELCDC = C(LTEDE+INT(ECDC*25.+11)) / C(LTEDE+8)
          IF ( PRO_ELCDC.EQ.0) PRO_ELCDC = 1. / C(LTEDE+8)
          PRO_PI  = PRO_PIL1 * PRO_PIL2 * PRO_PIL3 * PRO_PICDC
          PRO_EL  = PRO_ELL1 * PRO_ELL2 * PRO_ELL3 * PRO_ELCDC
          LIK2PI  = LOG(PRO_PIL1 * PRO_PIL2 * PRO_PIL3 )
          LIK2EL= LOG(PRO_ELL1 * PRO_ELL2 * PRO_ELL3 )
          LIK1PI=LOG(PRO_PI)
          LIK1EL=LOG(PRO_EL)
          LIK_ION_PI=LOG(PRO_PICDC)
          LIK_ION_EL=LOG(PRO_ELCDC )
          LIK1R(CASE) = LOG(PRO_EL/PRO_PI)
          IF (LIK1R(CASE).LE.-10.) THEN
            IF (NERR1.LE.10) CALL ERRMSG
     &          (' likelihood <-10 ','TRD_ELECTRON_PION','set to -10',
     &          'W')
            LIK1R(CASE)  = -10.
            NERR1 = NERR1 + 1
          END IF
          IF (LIK1R(CASE).GT.10.) THEN
            IF (NERR2.LE.10)
     &            CALL ERRMSG(' likelihood >10 ','TRD_ELECTRON_PION',
     &            'set to 10','W')
            LIK1R(CASE)  = 10.
            NERR2 = NERR2 + 1
          END IF
          EPSLR(CASE) = 1 - C(LTLTA+10+(HIT(1)+HIT(2)+HIT(3)-3)
     &          * 400 + INT((LIK1R(CASE)+10.)*20.)+1)
C          IF(CASEM.EQ.2)PRINT*,' case',CASE,' lik1r',LIK1R(CASE),
C     &      'epslr',EPSLR(CASE)
        END IF
C
C TRD only LIKELIHOOD and epsl'
C
        B=LIK2
        A=ABS(LIK2)
        IF (A.GT.10.) THEN
          IF (NERR1.LE.10)
     &          CALL ERRMSG(' abs(likelihood) >10 ',
     &          'TRD_ELECTRON_PION', 'set to 10','W')
          NERR1 = NERR1 + 1
          A=10.
          LIK2=SIGN(A,LIK2)
        END IF
C-------------------------- EPSL_2 (without dE/dx) -----------------------
C
        IF(MC_DATA)THEN
          EPSLR_2(CASE)=LIKE_REF_MC(LIK2,HIT) !
        ELSE
          EPSLR_2(CASE) = 1 - C(LTLTA+10+(HIT(1)+HIT(2)+HIT(3)-3)
     &        * 400 + INT((LIK2R(CASE)+10.)*20.)+1+1610)
        END IF
C        IF(DOPRINT)
C     +    WRITE(LOUT,*)' lik2',LIK2,' like_layer',LIKE_LAYER,' epsl_2',
C     +    epsl_2
C
  400   CONTINUE
        IF(CASEM.EQ.1)THEN
          EPSTR(2)= EPSTR(1)
          EPSLR(2)=EPSLR(1)
          EPSLR_2(2)=EPSLR_2(1)
          LIK1R(2)=LIK1R(1)
          LIK2R(2)=LIK2R(1)
        END IF
  500 CONTINUE ! En of loop on case for likelihood
C
C ********* LIKELIHOOD With CDC(DE/DX) AND 3 TRD LAYERS, for Minbias,Electron,
C           and Conversions)  ( Y.DUCROS June 28th 1996)
C
      DENS_3CH=1000.
      DENS_ION_3CH=1000.
C      INDEX_DENS=0
      DENS_2CH=1000.
      DENS_ION_2CH=1000.
      IF(.NOT.MC_DATA.AND.DO_DENS_ANALYSIS. AND.
     &  HITS(1).GT.0 .AND. HITS(2).GT.0 .AND. HITS(3).GT.0)THEN
        EN(1)=ENERGY_FIRED_CELLS(1)
        EN(2)=ENERGY_FIRED_CELLS(2)
        EN(3)=ENERGY_FIRED_CELLS(3)
        EN(4)=ECDC_DENS
        NA(1)=HITS(1)
        NA(2)=HIT(2)
        NA(3)=HITS(3)
C        DO ILAY=1,3
C          IF(EN(ILAY).GT.0.) EN(ILAY)=EN(ILAY)+0.1*RNDM(XXX)
C        END DO
        CALL HIS_DENS(EN,NA,PAS,VRAIS_PI,VRAIS_FA,VRAIS_EL,VRAIS_CO,
     #                      VRAIS_PIP,VRAIS_FAP,VRAIS_ELP,VRAIS_COP)
C
C *********   3 TRD Layers and  CDC Without DE/DX cut **************
C
        MUVW=LUVW_DENS(VRAIS_PI,VRAIS_EL,VRAIS_CO,
     #    XXMIN,XXMAX,YYMIN,YYMAX,ZZMIN,ZZMAX,
     #    NU,NV,NW, XU,YUV,ZUVW)
        IF(NUVW.GT.0.AND.MUVW.GT.0.AND.MUVW.LE.NUVW) THEN
          DENS_ION_3CH=FLOAT(INDEX(MUVW)-1)/FLOAT(NUVW)
        END IF
C
C ********    3 TRD Layers and  CDC With DE/DX cut**************
C
        MUVW=LUVW_DENS(VRAIS_PIP,VRAIS_ELP,VRAIS_COP,
     #    XXPMIN,XXPMAX,YYPMIN,YYPMAX,ZZPMIN,ZZPMAX,
     #    NPU,NPV,NPW, XPU,YPUV,ZPUVW)
        IF(NPUVW.GT.0.AND.MUVW.GT.0.AND.MUVW.LE.NPUVW)
     +     DENS_3CH=FLOAT(INDEXP(MUVW)-1)/FLOAT(NPUVW)
      END IF

C    --------------------------------------------------------------
  999 CONTINUE
      LTDST=GZTDST()
      CASE=1
      IF(CASEM.EQ.2 .AND. PERFORM_T_COR)CASE=2
      EPST=EPSTR(CASE)
      EPSL=EPSLR(CASE)
      EPSL_2=EPSLR_2(CASE)
      LIK1=LIK1R(CASE)
      LIK2=LIK2R(CASE)
      LIKE_LAYER(1)=LL(1,CASE)
      LIKE_LAYER(2)=LL(2,CASE)
      LIKE_LAYER(3)=LL(3,CASE)
      IF(LTDST.NE.0)THEN
        Q(LTDST+24)=TGEO
        Q(LTDST+8)=EPST
        Q(LTDST+9)=EPSL
        Q(LTDST+10)=LIK1
        Q(LTDST+11)=LIK2
        Q(LTDST+17)=LIKE_LAYER(1)
        Q(LTDST+18)=LIKE_LAYER(2)
        Q(LTDST+19)=LIKE_LAYER(3)
        Q(LTDST+20)=EPSL_2
        Q(LTDST+25)=EPSTR(1)
        Q(LTDST+26)=EPSLR(1)
        Q(LTDST+27)=EPSLR_2(1)
        Q(LTDST+28)=LIK1R(1)
        Q(LTDST+29)=LIK2R(1)
        IF(CASEM.EQ.2)THEN
          Q(LTDST+25)=EPSTR(2)
          Q(LTDST+26)=EPSLR(2)
          Q(LTDST+27)=EPSLR_2(2)
          Q(LTDST+28)=LIK1R(2)
          Q(LTDST+29)=LIK2R(2)
        END IF ! end of test on case
        Q(LTDST+30)=DENS_ION_3CH
        Q(LTDST+31)=DENS_3CH
C        IF(EPSTR(1).NE.EPSTR(2))THEN
C          WRITE(LOUT,'(a20,3g10.4))')'eps non corriges',
C     &      EPSTR(1),EPSLR(1),EPSLR_2(1)
C          WRITE(LOUT,'(a20,3g10.4))')'eps     corriges',
C     &      EPSTR(2),EPSLR(2),EPSLR_2(2)
C        END IF

      END IF! end of test on LTDST
C      IF(DOPRINT) WRITE(LOUT,*)' exit trd_electron_pion, acceptance',
C     &    ACCEPTANCE,' tgeo',TGEO,' epst',EPST,' geometry',GEOMETRY,
C     &    ' hits',HITS
      RETURN

      ENTRY NUM_LAYERS(LYR_ACC,THETA_PRIME)
C------------------------------------------------------------------------
C-    Purpose:  output number good layers crossed and angle of track
C-
C-    outputs:
C-          lyr_acc          I  = 1, 2, or 3 good layers in acceptance
C-          theta_prime      R  = angle of track with respect to vertical
C-
C-   Created  Jan-15-1996   Bob Kehoe
C------------------------------------------------------------------------
      THETA_PRIME = ABS((PI/2.0)-THETA_TRD(1))
      LYR_ACC = TGEO
      RETURN
      END
