      SUBROUTINE TRD_EPS_COR
     &  (ENERGY_FIRED_CELLS,ECDC,GEOMETRY,HITS,ACCEPTANCE,EPST,EPSL,
     &  EPSL_2,LIK1,LIK2)
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
      INTEGER VERSION, PASS,LAYER,LTDST,GZTDST
      REAL EPSTI
      REAL ENERGY_FIRED_CELLS(5),EPST,THRESHOLD,EPSL,EPSL_2
      REAL EFIRED(5)
      REAL ECDC,A,B,ETRUNC_REF_MC,LIKE_REF_MC
      REAL ENERGY(3), THETA_PRIME, RECO_VRS,CORT,TRD_COR_TEMP
      LOGICAL ACCEPTANCE,GEOMETRY(3),DOPRINT,MC_DATA,TRD_DO_PRINT
      LOGICAL DO_TEMP_CORRECTION
      LOGICAL ACCEPTANCE_3 ! normal three-layer acceptance
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
      INTEGER LZTRKI,LPELCI,HIT(3),TGEO,LYR_ACC,POS1,POS2,LOC
C----------------------------------------------------------------
C TRD + CDC combined likelihood
C----------------------------------------------------------------
      INTEGER LTPIO,LTELE,LTPI1,LTPI2,LTPDE,LTEL1,
     &        LTEL2,LTEDE,LTLTA
      REAL    PRO_EL,PRO_EL2,PRO_PI,PRO_PI2,PRO_PICDC,PRO_ELCDC,
     &        PRO_PIL1,PRO_PIL2,PRO_PIL3,PRO_ELL1,PRO_ELL2,PRO_ELL3,
     &        LIK1,LIK2,E1,E2,E3
C----------------------------------------------------------------
      LOGICAL FIRST,RUN1A,GOOD_ENERGIES,ALL_LAYERS
      DATA FIRST/.TRUE./
      DATA NATRUN/0/,RUNI/0/,NERR1/0/,NERR2/0/,THRESHOLD/0.2/
      DATA IFOIS/0/
      IF (FIRST) THEN
        FIRST = .FALSE.
        LOUT = TRUNIT()
        MC_DATA = IQ(LHEAD+1) .GT. 1000
        CALL EZLOC ('TRD_ANALYSIS_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_ANALYSIS_RCP',IER)
        CALL EZPICK ('TRD_ANALYSIS_RCP')
        CALL EZGET('DO_TEMP_CORRECTION',DO_TEMP_CORRECTION,IER)
        CALL EZRSET
      ENDIF      !end of initialization
      IF (.NOT.DO_TEMP_CORRECTION) GOTO 999
      LZTRKI=LZTRK
      LPELCI=LPELC
      IF (LTGEN.LE.0) THEN
        CALL ERRMSG(' TRD_EPS_COR','TRD_EPS_COR',
     &        'Bank TGEN not found','W')
        GO TO 999
      ENDIF
C
C----------------------------------------------------------------
C
      IF(run1a() .or. MC_DATA)GO TO 999
      IF (RUNNO().NE.RUNI) THEN
        RUNI = RUNNO()
        NAT  = 1
        IF(.NOT.RUN1A()) NAT = 2
        LTPHY = LC(LTGEN-IZTPHY)
        IF (LTPHY.LE.0) THEN
          CALL ERRMSG(' TRD_EPS_COR','TRD_EPS_COR',
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
            CALL ERRMSG(' TRD_EPS_COR','TRD_EPS_COR',
     &          'Bank T1A0 not found','W')
            GO TO 999
          ENDIF
          IF(.NOT.MC_DATA)THEN
            XMAX = C(LT+7)
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
          END IF
        END IF
      END IF ! end of test on MC
C
C      if(ifois.le.10)write(lout,*)' in TRD_EPS_CORacceptance',
C     &  acceptance
C set values of parameters when track does not cross layer1
      CORT=TRD_COR_TEMP()
      IF(ABS(CORT-1.).LE.0.01)GO TO 999
      EPSTI=EPST
      TGEO         = 0
      LIK1         =-1000.
      LIK2         =-1000.
      LIKE_LAYER(1)=-1000.
      LIKE_LAYER(2)=-1000.
      LIKE_LAYER(3)=-1000.
      EPST         =  999.
      EPSL         =  999.
      EPSL_2       =  999.
      LIK2PI = -1000.
      LIK2EL = -1000.
      LIK1PI = -1000.
      LIK1EL = -1000.
C      IF(DOPRINT)
C     +    WRITE(LOUT,*)' in TRD_EPS_COR hits',HITS,' geometry(1)',
C     +    GEOMETRY(1)
      IF(.NOT.GEOMETRY(1))GO TO 999
C From now on, assume GEOMETRY(1)=true
      LIKE_LAYER(1)=-100.
      HIT(1)=MIN0(HITS(1),2)
      HIT(2)=MIN0(HITS(2),2)
      HIT(3)=MIN0(HITS(3),2)
C----------------------------------------------------------------
C   truncated energy fired cells method (Alain Pluquet)
C----------------------------------------------------------------
      IFOIS=IFOIS+1
      DOPRINT=TRD_DO_PRINT()
c      DOPRINT=IFOIS.LE.40
      DO LAYER=1,3
        EFIRED(LAYER)=ENERGY_FIRED_CELLS(LAYER)*CORT
      END DO
C      write(lout,*)' in TRD_EPS_COR,doprint ',doprint,' ifois',ifois
C        if(doprint)write(lout,*)'MC_DATA',mc_data,'  IQ(LHEAD+1)',
C     &    IQ(LHEAD+1)
      EPST         =   1. ! 1 OR MORE LAYERS ARE MISSING ==> PION
c      IF(DOPRINT)
c     +    WRITE(LOUT,*)' in TRD_EPS_COR hits',HITS,
c     &    ' acceptance',ACCEPTANCE,' e fired',EFIRED
      GOOD_ENERGIES= EFIRED(1).GE.THRESHOLD .AND.
     &               EFIRED(2).GE.THRESHOLD .AND.
     &               EFIRED(3).GE.THRESHOLD
      ALL_LAYERS=HIT(1).GT.0 .AND. HIT(2).GT.0 .AND. HIT(3).GT.0
      ACCEPTANCE_3 = ACCEPTANCE
      IF (ACCEPTANCE) THEN
        TGEO = 3
        EPST         =   1. ! 1 OR MORE LAYERS ARE MISSING ==> PI
        IF (GOOD_ENERGIES.AND.ALL_LAYERS) THEN
          I=2
          J=3
          K=LVMAX(EFIRED,3)
          IF(K.EQ.2)THEN
            I=1
            J=3
          ELSE IF(K.EQ.3)THEN
            I=1
            J=2
          END IF
          FINAL_ENERGY= EFIRED(I)+ EFIRED(J)
          IF(DOPRINT)
     &      WRITE(LOUT,*)' final_energy',FINAL_ENERGY,' mc_data',MC_DATA
C          IF(MC_DATA)THEN
C            EPST=ETRUNC_REF_MC(FINAL_ENERGY,HIT)
C          ELSE
          POSITION=IFIX(FINAL_ENERGY/XMAX*150.)
          POS2=POSITION
          POS1=IFIX(ENERGY_FIRED_CELLS(I)+ENERGY_FIRED_CELLS(J))
     +        *150./XMAX
          IF (POSITION.LT.1)   POSITION=0
          IF (POSITION.GT.150) POSITION=151
          IF(DOPRINT)
     &         WRITE(LOUT,*)' xmax,FINAL_ENERGY',XMAX,FINAL_ENERGY,
     &         ' position',POSITION,' pos1,pos2',POS1,POS2
          ICAS=1
C            IF (HIT(I).EQ.1.AND.HIT(J).EQ.1)ICAS=1
C            IF(HIT(I).GE.2 .OR. HIT(J).GE.2)ICAS=3! case 1-2
C            IF(HIT(I).GE.2 .and. HIT(J).GE.2)ICAS=4! case 2-2
          IF ((HIT(1).GE.2.AND.HIT(2).EQ.1.AND.HIT(3).EQ.1).OR.
     &          (HIT(1).EQ.1.AND.HIT(2).GE.2.AND.HIT(3).EQ.1).OR.
     &          (HIT(1).EQ.1.AND.HIT(2).EQ.1.AND.HIT(3).GE.2)) ICAS = 2
     &          !1-1-2
          IF ((HIT(1).EQ.1.AND.HIT(2).GE.2.AND.HIT(3).GE.2).OR.
     &          (HIT(1).GE.2.AND.HIT(2).EQ.1.AND.HIT(3).GE.2).OR.
     &          (HIT(1).GE.2.AND.HIT(2).GE.2.AND.HIT(3).EQ.1))
     &          ICAS = 3 !1-2-2
          IF (HIT(1).GE.2.AND.HIT(2).GE.2.AND.HIT(3).GE.2) ICAS = 4 !2-2-2
          IF(DOPRINT)
     +          WRITE(LOUT,*)' In TRD_EPS_COR e_fired',
     +          EFIRED, 'i,j min',
     +          I,J,'   epst',EPST,' nanod',HIT,' lztrk,lpelc',LZTRK,
     +          LPELC,
     +          'icas',ICAS,
     &          ' etrunc',FINAL_ENERGY,' e(j)',FLOAT(POSITION)
     &          *XMAX/150.,
     &          ' position',POSITION
          IF (POSITION.GE.2.AND.POSITION.LE.150) THEN
            EPST= (TEFC(POSITION,ICAS) - TEFC(POSITION-1,ICAS))
     &            * (FINAL_ENERGY / XMAX * 150. - FLOAT(POSITION))
     &            + TEFC(POSITION,ICAS)
c            IF(DOPRINT)
c     &            WRITE(LOUT,*)
c     &            'TEFC(POSITION,ICAS),TEFC(POSITION-1,ICAS)',
c     &            TEFC(POSITION,ICAS),TEFC(POSITION-1,ICAS),
c     &            ' epst',EPST,
c     &            ' coeff',FINAL_ENERGY / XMAX * 150. - FLOAT(POSITION)
          ELSE
            EPST = TEFC(POSITION,ICAS)
          ENDIF ! end of test on position
C          END IF ! end of test on MC
        ENDIF
C-        *** bk, for < 3lyr calc. trunc.-sum with available layers and
C-        *** determine epsilon_t for track.  set tgeo for num_layers entry pt.
        CALL UCOPY(ENERGY_FIRED_CELLS,ENERGY,3)
        CALL EPSILON_T_END(ENERGY,HITS,GEOMETRY,EPST,FINAL_ENERGY,
     &        TGEO,IER)
        EFIRED(4) = FINAL_ENERGY*(2.0/3.0)*FLOAT(TGEO)
        EFIRED(5) = FINAL_ENERGY
C        if (ier.eq.0.and.tgeo.gt.0) acceptance = .true.
c        IF(DOPRINT)WRITE(LOUT,*)' after epsilon_t_end,mc_data ',MC_DATA
      END IF ! end of test of acceptance
C-        *** bk, for reco v12.20 and earlier, reject tracks with angle within
C-        *** 15-degrees of beampipe. for reco >= 12.21, cut is 8-degrees.
C      call reco_version(version,pass)
C      reco_vrs = float(version) + 0.01*float(pass)
C      if ((reco_Vrs.le.12.2.and.abs((pi/2.0)-theta_trd(1)).ge.1.31).or.
C     &   (reco_Vrs.gt.12.2.and.abs((pi/2.0)-theta_trd(1)).ge.1.43)) then
C        acceptance = .false.
C        acceptance_3 = .false.
C        tgeo = 0
C      endif
C      IF(LTDST.NE.0)Q(LTDST+24)=TGEO
C
C------------------------------------
C   likelihood (Jean-Francois LEBRAT)
C------------------------------------
C
      LTLIK = LC(LTGEN-IZTLIK)
C
      IF (LTLIK.LE.0) THEN
        CALL ERRMSG(' STP bank error ','TRD_EPS_COR',
     &          'Bank TLIK not found','w')
        GO TO 999
      ENDIF
C test on links hanging from TLIK
      LTPIO = LC(LTLIK-IZTPIO)
      LTELE = LC(LTLIK-IZTELE)
      LTLTA = LC(LTLIK-IZTLTA)
      IF (LTPIO.LE.0 .OR. LTELE.LE.0 .OR. LTLTA.LE.0) THEN
        CALL ERRMSG(' STP bank error ','TRD_EPS_COR',
     &          'Bank TELE or TPIO or TLTA not found','w')
        GO TO 999
      ENDIF
C test on links hanging from tpio
      LTPI1 = LC(LTPIO-IZTPI1)
      LTPI2 = LC(LTPIO-IZTPI2)
      LTPDE = LC(LTPIO-IZTPDE)
      IF (LTPI1.LE.0.OR.LTPI2.LE.0.OR.LTPDE.LE.0.) THEN
        CALL ERRMSG(' STP bank error','TRD_EPS_COR',
     &          'Bank TPI1 or TPI2 or TPDE not found','w')
        GO TO 999
      ENDIF
C test on links hanging from tele
      LTEL1 = LC(LTELE-IZTEL1)
      LTEL2 = LC(LTELE-IZTEL2)
      LTEDE = LC(LTELE-IZTEDE)
C
      IF (LTEL1.LE.0.OR.LTEL2.LE.0..OR.LTEDE.LE.0.) THEN
        CALL ERRMSG(' STP bank error ','TRD_EPS_COR',
     &          'Bank TEL1 or TEL2 or TEDE not found','w')
        GO TO 999
      ENDIF
C
      E1 = AMIN1(EFIRED(1),14.9)
      E2 = AMIN1(EFIRED(2),14.9)
      E3 = AMIN1(EFIRED(3),14.9)
      IF(HIT(1).NE.0 .AND.E1.GE.THRESHOLD )THEN
        LTP=LTPI1
        LTE=LTEL1
        IF(HIT(1).GE.2)THEN
          LTP=LTPI2
          LTE=LTEL2
        END IF
        PRO_PIL1 = C(LTP+INT(E1*6.6667+11))/C(LTP+8)
        PRO_ELL1 = C(LTE+INT(E1*6.6667+11))/C(LTE+8)
        IF (PRO_PIL1.EQ.0.) PRO_PIL1 = 1. / C(LTP+8)
        IF (PRO_ELL1.EQ.0.) PRO_ELL1 = 1. / C(LTE+8)
        LIKE_LAYER(1) = LOG(PRO_ELL1/PRO_PIL1)
      END IF
      IF(GEOMETRY(2))THEN
        LIKE_LAYER(2)=-200.
        IF(HIT(2).NE.0 .AND. E2.GE.THRESHOLD)THEN
          LTP=LTPI1
          LTE=LTEL1
          IF(HIT(2).GE.2)THEN
            LTP=LTPI2
            LTE=LTEL2
          END IF
          PRO_PIL2 = C(LTP+INT(E2*6.6667+111)) / C(LTP+9)
          PRO_ELL2 = C(LTE+INT(E2 *6.6667+111)) / C(LTE+9)
          IF (PRO_PIL2.EQ.0.) PRO_PIL2 = 1. / C(LTP+9)
          IF (PRO_ELL2.EQ.0) PRO_ELL2 = 1. / C(LTE+9)
          LIKE_LAYER(2) = LOG(PRO_ELL2/PRO_PIL2)
        END IF
        IF(GEOMETRY(3))THEN
          LIKE_LAYER(3)=-400.
          IF(HIT(3).NE.0 .AND. E3.GE.THRESHOLD)THEN
            LTP=LTPI1
            LTE=LTEL1
            IF(HIT(3).GE.2)THEN
              LTP=LTPI2
              LTE=LTEL2
            END IF
C            IF(doprint)
C     +        WRITE(LOUT,*)' hit(3)',HIT(3),' ltp,lte',LTP,LTE,
C     &        ' e3',E3
            PRO_PIL3 = C(LTP+INT(E3*6.6667+211))/C(LTP+10)
            PRO_ELL3 = C(LTE+INT(E3*6.6667+211))/C(LTE+10)
            IF (PRO_PIL3.EQ.0.) PRO_PIL3 = 1. /C(LTP+10)
            IF (PRO_ELL3.EQ.0.) PRO_ELL3 = 1. /C(LTE+10)
            LIKE_LAYER(3) = LOG(PRO_ELL3/PRO_PIL3)
          END IF
          LIK2=LIKE_LAYER(1)+LIKE_LAYER(2)+LIKE_LAYER(3)
        ELSE! geometry(3) false, geometry(2) true
          LIK2=LIKE_LAYER(1)+LIKE_LAYER(2)
          IF(LIKE_LAYER(1).GT.-50. AND. LIKE_LAYER(2).GT.-50.)
     +        LIK2=1.5*(LIKE_LAYER(1)+LIKE_LAYER(2))
        END IF ! end of test of geometry 3
      ELSE !geometry(2) false geometry(1) true
        LIK2=LIKE_LAYER(1)
        IF(LIKE_LAYER(1).GT.-50.)LIK2=3.*LIK2
      END IF! end of test on geometry 2
C
C    compute epsl,epsl'
C
C
      IF(.NOT.ACCEPTANCE_3)GO TO 999 ! *** bk, need acceptance_3 for CDC
C ONE TRD LAYER MISSING -> PION
      EPSL   =   1.
      EPSL_2 =   1.
      LIK1   = -99.
C          IF(doprint)
C     +      WRITE(LOUT,*)' e1,e2,e3',E1,E2,E3,' threshold',THRESHOLD
      IF (.NOT. GOOD_ENERGIES .OR. .NOT.ALL_LAYERS)GO TO 999
      IF (.NOT.MC_DATA .AND. LQ(LZTRK-7).NE.0) THEN ! request CDC track
C                                            TRD+CDC likelihood and epsl
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
        LIK1 = LOG(PRO_EL/PRO_PI)
        IF (LIK1.LE.-10.) THEN
          IF (NERR1.LE.10) CALL ERRMSG
     &        (' likelihood <-10 ','TRD_EPS_COR','set to -10','W')
          LIK1  = -10.
          NERR1 = NERR1 + 1
        END IF
        IF (LIK1.GT.10.) THEN
          IF (NERR2.LE.10)
     &            CALL ERRMSG(' likelihood >10 ','TRD_EPS_COR',
     &            'set to 10','W')
          LIK1  = 10.
          NERR2 = NERR2 + 1
        END IF
        EPSL = 1 - C(LTLTA+10+(HIT(1)+HIT(2)+HIT(3)-3)
     &          * 400 + INT((LIK1+10.)*20.)+1)
      END IF
C
C TRD only LIKELIHOOD and epsl'
C
      B=LIK2
      A=ABS(LIK2)
      IF (A.GT.10.) THEN
        IF (NERR1.LE.10)
     &          CALL ERRMSG(' abs(likelihood) >10 ',
     &          'TRD_EPS_COR', 'set to 10','W')
        NERR1 = NERR1 + 1
        A=10.
        LIK2=SIGN(A,LIK2)
      END IF
C-------------------------- EPSL_2 (without dE/dx) -----------------------
C
C      IF(MC_DATA)THEN
C        EPSL_2=LIKE_REF_MC(LIK2,HIT) ! this call temporarily commented out
C      ELSE
      EPSL_2 = 1 - C(LTLTA+10+(HIT(1)+HIT(2)+HIT(3)-3)
     &        * 400 + INT((LIK2+10.)*20.)+1+1610)
C      END IF
C        IF(DOPRINT)
C     +    WRITE(LOUT,*)' lik2',LIK2,' like_layer',LIKE_LAYER,' epsl_2',
C     +    epsl_2
C
      LTDST=GZTDST()
      IF(LTDST.NE.0)THEN
        IF(DOPRINT .OR. ( EPST.NE.0..AND.ABS(EPSTI/EPST-1.).GT.0.1))THEN
          WRITE(LOUT,*)' epst_cor,EFFICIENCY',EPST,Q(LTDST+8),' cort',
     &      CORT,
     &      'pos1,pos2',POS1,POS2
        END IF
      END IF
  999 CONTINUE
      LTDST=GZTDST()
      IF(ltdst.NE.0)THEN
        Q(LTDST+25)=EPST
        Q(LTDST+26)=EPSL
        Q(LTDST+27)=EPSL_2
        Q(LTDST+28)=LIK1
        Q(LTDST+29)=LIK2
      END IF
      IF(DOPRINT) WRITE(LOUT,*)' exit TRD_EPS_COR'
      RETURN
      END
