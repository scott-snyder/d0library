      SUBROUTINE EPSILON_T_END(LAYER_ENERGIES,LAYER_NHITS,GEOMETRY,
     &  EPSILON_T,TRUNC_SUM,TGEO,IER)

C----------------------------------------------------------------------
C-   Purpose and Methods : to extend reach of TRD by including those tracks
C-      which cross 1 or 2 useful layers.  a variable, called t_end, is
C-      calculated and normalized to mimic the truncated sum in the central
C-      region, accounting for differences due to the track environment and
C-      differences with run number (1a, 1b, 1c).  this is then transformed
C-      into an epsilon_t-like quantity to consider differences in 1 and 2
C-      layer tracks.
C-
C-   Inputs  :
C-      layer_energies(3) R   : fired cell energy in layers 1, 2, and 3
C-      layer_nhits(3)    I   : number anode wires hit in layer 1, 2, and 3
C-      geometry(3)       L   : .true. if layer crossed by track
C-
C-   Outputs :
C-      epsilon_t         R   : integral of truncated sum distribution for
C-                              1 and 2 layer tracks
C-      trunc_sum         R   : sum lowest two layers for 3 lyr tracks
C-                              (3/2)* ave both layers for 2 lyr tracks
C-                              (3/2)* layer 1 for single lyr tracks
C-                              note: = zero if any good layer has no energy.
C-                                      also, scale factors have been applied to
C-                                      remove dependence or trunc. sum 1a/1b
C-                                      variations, and on cell environment.
C-      tgeo              I   : = number of good layers crossed by track taking
C-                              into account bad runs, bad sectors, and bad udst
C-                              or reco versions.
C-      ier               I   : = 0 if ok, = -1 if problem
C-
C-   Controls:  none
C-
C-   Created  10-DEC-1995   Bob Kehoe
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TRD_PHI_Z.INC'
      INTEGER NBINS,NBINS_ON_STP
      REAL HMIN,HMAX
      PARAMETER (NBINS = 76)
C      PARAMETER (hmin = 0.01)
C      PARAMETER (hmax = 30.0)
      INTEGER I,IER,K,UDST_VRS,LTANA,LTDST,GZTDST,LUDST,GZUDST
      INTEGER TGEO,TRD_ZERO,LAYER,LAYER_NHITS(3),RUNNO,EVONUM
      INTEGER VERSION,PASS
      REAL TRUNC_SUM,EPSILON_T,T_END,LAYER_ENERGIES(3),LYR_NRG
      REAL TSUM_TO_EPS_1LYR(NBINS),TSUM_TO_EPS_2LYR(NBINS)
      REAL NORM_RUNS_1LYR(3),NORM_RUNS_2LYR(3),RECO_VRS
      REAL NORM_HITS_1LYR,NORM_HITS_2LYR,BIN,INTERPOLATION
      LOGICAL TRD_BADRUN,GEOMETRY(3),BAD_SECTOR,BDSCTR
      LOGICAL FIRST
      INCLUDE 'D0$INC:zebstp.INC'
      INTEGER GZTND1,GZTND2,LTND1,LTND2
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C-          INITIALIZATION
      IF(FIRST)THEN
        FIRST=.FALSE.
        LTND1=GZTND1()
        LTND2=GZTND2()
        IF(LTND1.LE.0 .OR. LTND2.LE.0)THEN
          NBINS_ON_STP=0
          CALL ERRMSG('TND1 or TND2 bank missing',
     &              'epsilon_t_end',' ','W')
          IER = -1
          TGEO = 0
          GO TO 999
        END IF
        NBINS_ON_STP =  IC(LTND1+1)  ! nbins
        IF(NBINS_ON_STP.NE.NBINS)THEN
          CALL ERRMSG('Error in STP bank TND1 ',
     &              'epsilon_t_end',' ','W')
          IER = -1
          TGEO = 0
          GO TO 999
        END IF

        HMIN =    C(LTND1+2) !HMIN
        HMAX  =   C (LTND1+3) !Hmax
        NORM_RUNS_1LYR(1)= C  (LTND1+4) !1a
        NORM_RUNS_1LYR(2)  =  C  (LTND1+5)!1b
        NORM_RUNS_1LYR(3)  =  C  (LTND1+6)!1c
        NORM_HITS_1LYR =  C  (LTND1+7)!trunc. sum scale for '11' tracks
        DO I=1,NBINS
          TSUM_TO_EPS_1LYR(I) =    C(LTND1+7+I)
        END DO
        NBINS_ON_STP =  IC(LTND2+1)  ! nbins
        HMIN =    C(LTND2+2) !HMIN
        HMAX  =   C (LTND2+3) !Hmax
        NORM_RUNS_2LYR(1)= C  (LTND2+4) !1a
        NORM_RUNS_2LYR(2)= C  (LTND2+5)!1b
        NORM_RUNS_2LYR(3)= C  (LTND2+6)!1c
        NORM_HITS_2LYR   = C  (LTND2+7)!trunc. sum scale for '21' tracks
        DO I=1,NBINS
          TSUM_TO_EPS_2LYR(I) =    C(LTND2+7+I)
        END DO
      END IF ! End of initialization
      IF(NBINS_ON_STP.LE.0)THEN
        CALL ERRMSG('TND1 or TND2 bank missing',
     &              'epsilon_t_end',' ','W')
        IER = -1
        TGEO = 0
        GO TO 999
      END IF
      IER = 0
      TGEO = 0
      T_END = 0.0
      TRUNC_SUM = 0.
      EPSILON_T = 0.
      BAD_SECTOR = .FALSE.
      DO K = 1,3
        IF (GEOMETRY(K)) TGEO = TGEO + 1
        CALL TRD_BADSECTOR(PHI_TRD(K),BDSCTR)
        IF (BDSCTR) BAD_SECTOR = .TRUE.
      ENDDO
      LUDST = GZUDST()
      IF (LUDST.GT.0) UDST_VRS = IQ(LUDST+1)
      CALL RECO_VERSION(VERSION,PASS)
      RECO_VRS = FLOAT(VERSION) + 0.01*FLOAT(PASS)

      IF (UDST_VRS.EQ.1.OR.UDST_VRS.EQ.2) THEN
        CALL ERRMSG('BAD TRD INFO',
     &              'epsilon_t_end','udst version < 3','W')
        IER = -1
        TGEO = 0
      ELSEIF (RECO_VRS.EQ.12.2.AND.TGEO.EQ.1) THEN
        CALL ERRMSG('BAD TRD INFO',
     &              'epsilon_t_end','1-layer track in v12.20','W')
        IER = -1
        TGEO = 0
      ELSEIF (TRD_BADRUN().OR.BAD_SECTOR) THEN
        CALL ERRMSG('BAD TRD INFO',
     &              'epsilon_t_end','bad run/sector','W')
        IER = -1
        TGEO = 0
      ELSEIF (TGEO.EQ.1.OR.TGEO.EQ.2) THEN
        TRD_ZERO = .FALSE.
        DO LAYER = 1,3
          IF (GEOMETRY(LAYER)) THEN
            LYR_NRG = LAYER_ENERGIES(LAYER)
            IF (LAYER_NHITS(LAYER).EQ.0) THEN
              TRD_ZERO = .TRUE.             ! bad electron if any layer zero
            ELSEIF (LYR_NRG.LT.HMIN) THEN
              LYR_NRG = HMIN                ! zero E was due to udst rounding
            ENDIF
            T_END = T_END + LYR_NRG
          ENDIF
        ENDDO
        IF (TRD_ZERO.ne.0) T_END = 0.0
        TRUNC_SUM = 1.5*T_END/FLOAT(TGEO) ! normalize t_end to 3-lyr trsum
C-          *** remove most of dependence on run and environment with scale
C-          *** factors applied to trunc_sum.
        IF (TGEO.EQ.1) THEN
          IF (RUNNO().LT.70000) THEN
            TRUNC_SUM = NORM_RUNS_1LYR(1)*TRUNC_SUM
          ELSEIF (RUNNO().LT.94000) THEN
            TRUNC_SUM = NORM_RUNS_1LYR(2)*TRUNC_SUM
          ELSE
            TRUNC_SUM = NORM_RUNS_1LYR(3)*TRUNC_SUM
          ENDIF
          IF (LAYER_NHITS(1).EQ.1) TRUNC_SUM = NORM_HITS_1LYR*TRUNC_SUM
        ELSEIF (TGEO.EQ.2) THEN
          IF (RUNNO().LT.70000) THEN
            TRUNC_SUM = NORM_RUNS_2LYR(1)*TRUNC_SUM
          ELSEIF (RUNNO().LT.94000) THEN
            TRUNC_SUM = NORM_RUNS_2LYR(2)*TRUNC_SUM
          ELSE
            TRUNC_SUM = NORM_RUNS_2LYR(3)*TRUNC_SUM
          ENDIF
          IF (LAYER_NHITS(1).EQ.1.AND.LAYER_NHITS(2).EQ.1) THEN
            TRUNC_SUM = NORM_HITS_2LYR*TRUNC_SUM
          ENDIF
        ENDIF
C-          *** convert final value of trunc_sum to epsilon_t.  values outside
C-          *** of bounds of original histograms set to 0.0 and 1.0 for epsilon.
C-          *** within each bin, epsilon is interpolated linearly.
        IF (TRUNC_SUM.LT.HMIN) THEN
          EPSILON_T = 1.0
        ELSEIF (TRUNC_SUM.GE.HMAX) THEN
          EPSILON_T = 0.0
        ELSE
          BIN = 1.0 + FLOAT(NBINS-1)*(TRUNC_SUM - HMIN)/(HMAX - HMIN)
          DO K = 2,NBINS
            IF (TGEO.EQ.1) THEN
              IF (BIN.GE.FLOAT(K-1).AND.BIN.LT.FLOAT(K)) THEN
                INTERPOLATION = (BIN-FLOAT(K-1))*(TSUM_TO_EPS_1LYR(K)
     &                -TSUM_TO_EPS_1LYR(K-1))
                EPSILON_T = TSUM_TO_EPS_1LYR(K-1) + INTERPOLATION
              ENDIF
            ELSEIF (TGEO.EQ.2) THEN
              IF (BIN.GE.FLOAT(K-1).AND.BIN.LT.FLOAT(K)) THEN
                INTERPOLATION = (BIN-FLOAT(K-1))*(TSUM_TO_EPS_2LYR(K)
     &                -TSUM_TO_EPS_2LYR(K-1))
                EPSILON_T = TSUM_TO_EPS_2LYR(K-1) + INTERPOLATION
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ELSE
        IER = -1      ! no layers or three layer tracks
      ENDIF

  999 RETURN
      END
