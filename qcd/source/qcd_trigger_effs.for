      SUBROUTINE QCD_TRIGGER_EFFS
     &   ( NJ, ZV, FILT_NAME, J_ET, J_ETA, L1_EFF, L2_EFF, EFF, ERROR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calc event effs based on single jets effs
C-
C-   Inputs  : Number of jets, Vertex, trigger, J_Et(uncorr), J_eta
C-               I   NJ (Number of jets in event used in calc, max:25)
C-               R   VERTEX
C-               C   FILT_NAME indicating which bit fired
C-                    JET_MIN
C-                    JET_LOW
C-                    JET_MEDIUM
C-                    JET_HIGH
C-                    JET_MAX
C-               R   J_ET(1:NJ)  Array of uncorr jet E_T (R=0.7 jets)
C-               R   J_ETA(1:NJ) Array of jet eta
C-
C-   Outputs: L1 and L2 event effs
C-               R   L1_EFF  L1 event eff
C-               R   L2_EFF  L2 event eff
C-               R   EFF    Event eff
C-   Controls: none
C-
C-   Created  20-JUN-1994   Terece L. Geld
C-   Modified 30-AUG-1994   Terece L. Geld
C-                             New function, more stats for higher triggers
C-                             fitting function:  tanh(x-C)*[1-B*exp(-x/A)]
C-   Modified 11-OCT-1994   Terece L. Geld
C-                             JET_MEDIUM, JET_HIGH, JET_MAX L1 will be using
C-                             event effs due to low stats
C-   Modified DEC-1994      Terece L. Geld
C-                             Bug fix => more stats, new fits
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NJ, NJMAX, NETAB, NPFIT, NFILT
      PARAMETER (NJMAX = 25)
      PARAMETER (NFILT = 5)
      PARAMETER (NETAB = 3) ! number of eta bins
      PARAMETER (NPFIT = 3) ! number of params in fit

      REAL ZV
      REAL J_ET(NJMAX), J_ETA(NJMAX), DET_ETA
      REAL L1_EFF, L2_EFF, J_EFF, EFF, ERROR
      CHARACTER*64 FILT_NAME

      REAL P13, P17, P27, P37, P45
      REAL PL1_13_00(NPFIT,NETAB), PL1_17_13(NPFIT,NETAB)
      REAL PL1_27_17(NPFIT,NETAB), PL1_37_27(NPFIT,NETAB)
      REAL PL1_45_13(NPFIT,NETAB)
      REAL PL2_120(NPFIT,NETAB), PL2_130(NPFIT,NETAB)
      REAL PL2_150(NPFIT,NETAB), PL2_185(NPFIT,NETAB)
      REAL PL2_1115(NPFIT,NETAB)
      REAL L1CUT(NFILT), L2CUT(NFILT)
      REAL EFF_MIN, MBCUT(NETAB)

      INTEGER II, JJ, KK, IBIT, LLEN
      REAL X

      DATA PL1_13_00 / 3.603133, 7.983919, 5.978452,
     &                 3.139287, 13.62344, 21.33081,
     &                 1.842332, 12.92865, 10.37007 /

      DATA PL1_17_13 / 10.20578, 9.434969, 7.633977,
     &                 6.704972, 11.17841, 19.06482,
     &                 8.972793, 9.617303, 23.83226 /

      DATA PL1_27_17 / 47.20769, 9.276699, 12.41450,
     &                 7.741745, 15.26328, 1.263011,
     &                 80.67658, 8.234698, 12.88524 /

      DATA PL1_37_27 / 7.203684, 11.34587, 31.47449,
     &                 6.466046, 12.87431, 26.26917,
     &                 17.81517, 9.092811, 19.70514 /

      DATA PL1_45_13 / 10.55109, 9.116369, 25.88335,
     &                 88.28468, 6.421604, 29.58022,
     &                 7.201659, 11.33196, 24.47904/
!
      DATA PL2_120 /  117.2280, 3.942139, 18.08523,
     &                67.10700, 4.232271, 17.15688,
     &                33.14692, 3.978174, 17.81749 /

      DATA PL2_130 /  119.6637, 4.499277, 30.67175,
     &                146.5788, 4.445620, 30.42299,
     &                48015.77, 2.411052, 28.11047 /

      DATA PL2_150 /  532.7928, 5.779973, 50.17758,
     &                0.8503301, 15.44605, 50.08670,
     &                1933096., 3.117151, 48.26210 /

      DATA PL2_185 /  0.04791132, 125.9853, 84.66618,
     &                0.5093279, 33.64824, 84.49390,
     &                0.5714363, 23.78701, 84.56081 /

      DATA PL2_1115 /  6678.104, 11.29337, 112.1739,
     &                 4.385829, 27.29171, 111.8362,
     &                 0.5871940, 52.78446, 112.1049/

      DATA L1CUT / 2.0, 20.0, 35.0, 40.0, 60.0 /
      DATA L2CUT / 18.0, 27.0, 45.0, 75.0, 100.0 /
      DATA MBCUT / 35.0, 40.0, 40.0 /
C----------------------------------------------------------------------
C
C *** Get L1 eff
C
      L1_EFF = 1.0
      ERROR = 1.0
      LLEN = LEN(FILT_NAME)

      IF (FILT_NAME(1:LLEN).EQ.'JET_MIN') THEN
        J_EFF = 0.0
        DO JJ = 1, MIN(NJ,NJMAX)
          IF ( J_ET(JJ).GT.1.0 ) THEN
            CALL PHYETA_DETETA(ZV, J_ETA(JJ), DET_ETA)
            X = J_ET(JJ)
            KK = 2                   ! Get Eta region (CC, ICR, EC)
            IF ( ABS(DET_ETA) .LE.0.6) KK = 1
            IF ( ABS(DET_ETA) .GT.1.6) KK = 3
            IF (X.GE.L1CUT(1)) THEN
              IF (X.GE.MBCUT(KK)) THEN
                J_EFF = 1.0
              ELSE
                J_EFF = TANH(X - PL1_13_00(3,KK))*
     &              (1 - PL1_13_00(1,KK)*EXP(-X/PL1_13_00(2,KK)) )
              ENDIF
            ENDIF
            J_EFF = MIN(J_EFF,1.0)
            J_EFF = MAX(0.0,J_EFF)
            L1_EFF = L1_EFF*(1.0 - J_EFF)
          ENDIF
        ENDDO
        L1_EFF = 1 - L1_EFF
        ERROR = 0.0

      ELSEIF (FILT_NAME(1:LLEN).EQ.'JET_LOW') THEN
        P13 = 0.0
        P17 = 0.0
        J_EFF = 0.0
        DO JJ = 1, MIN(NJ,NJMAX)
          IF ( J_ET(JJ).GT.1.0 ) THEN
            CALL PHYETA_DETETA(ZV, J_ETA(JJ), DET_ETA)
            X = J_ET(JJ)
            KK = 2
            IF ( ABS(DET_ETA) .LE.0.6) KK = 1
            IF ( ABS(DET_ETA) .GT.1.6) KK = 3

            IF (X.GE.L1CUT(2)) THEN
              P17 = TANH(X - PL1_17_13(3,KK))*
     &              (1 - PL1_17_13(1,KK)*EXP(-X/PL1_17_13(2,KK)) )
            ENDIF
            IF (X.GE.L1CUT(1)) THEN
              IF (X.GE.MBCUT(KK)) THEN
                P13 = 1.0
              ELSE
                P13 = (TANH(X - PL1_13_00(3,KK))*
     &              (1 - PL1_13_00(1,KK)*EXP(-X/PL1_13_00(2,KK)) ))
              ENDIF
            ENDIF
            P17 = MIN(P17,1.0)
            P17 = MAX(0.0,P17)
            P13 = MIN(P13,1.0)
            P13 = MAX(0.0,P13)
            J_EFF = P17*P13
            L1_EFF = L1_EFF*(1.0 - J_EFF)
          ENDIF
        ENDDO
        L1_EFF = 1 - L1_EFF
        ERROR = 0.0

      ELSEIF (FILT_NAME(1:LLEN).EQ.'JET_MEDIUM') THEN
        P13 = 0.0
        P17 = 0.0
        P27 = 1.0
        J_EFF = 0.0
        X = -999.9
        DO JJ = 1, MIN(NJ,NJMAX)
          X = MAX(J_ET(JJ),X)
        ENDDO  
        IF (X .GT. L1CUT(3)) THEN
          P27 = TANH(X - PL1_27_17(3,KK))*
     &              (1 - PL1_27_17(1,KK)*EXP(-X/PL1_27_17(2,KK)) )
        ENDIF
        DO JJ = 1, MIN(NJ,NJMAX)
          IF ( J_ET(JJ).GT.1.0 ) THEN
            CALL PHYETA_DETETA(ZV, J_ETA(JJ), DET_ETA)
            X = J_ET(JJ)
            KK = 2
            IF ( ABS(DET_ETA) .LE.0.6) KK = 1
            IF ( ABS(DET_ETA) .GT.1.6) KK = 3

            IF (X.GE.L1CUT(2)) THEN
              P17 = TANH(X - PL1_17_13(3,KK))*
     &              (1 - PL1_17_13(1,KK)*EXP(-X/PL1_17_13(2,KK)) )
            ENDIF
            IF (X.GE.L1CUT(1)) THEN
              IF (X.GE.MBCUT(KK)) THEN
                P13 = 1.0
              ELSE
                P13 = (TANH(X - PL1_13_00(3,KK))*
     &              (1 - PL1_13_00(1,KK)*EXP(-X/PL1_13_00(2,KK)) ))
              ENDIF
            ENDIF
            P17 = MIN(P17,1.0)
            P17 = MAX(0.0,P17)
            P13 = MIN(P13,1.0)
            P13 = MAX(0.0,P13)
            J_EFF = P17*P13
            L1_EFF = L1_EFF*(1.0 - J_EFF)
          ENDIF
        ENDDO
        P27 = MIN(P27,1.0)
        P27 = MAX(0.0,P27)
        L1_EFF = L1_EFF*P27
        L1_EFF = 1 - L1_EFF
        ERROR = 0.0
        
      ELSEIF (FILT_NAME(1:LLEN).EQ.'JET_HIGH') THEN
        P13 = 0.0
        P17 = 0.0
        P27 = 1.0
        P37 = 1.0
        J_EFF = 0.0
        X = -999.9
        DO JJ = 1, MIN(NJ,NJMAX)
          X = MAX(J_ET(JJ),X)
        ENDDO  
        IF (X .GT. L1CUT(3)) THEN
          P27 = TANH(X - PL1_27_17(3,KK))*
     &              (1 - PL1_27_17(1,KK)*EXP(-X/PL1_27_17(2,KK)) )
        ENDIF
        IF (X .GT. L1CUT(4)) THEN
          P37 = TANH(X - PL1_37_27(3,KK))*
     &              (1 - PL1_37_27(1,KK)*EXP(-X/PL1_37_27(2,KK)) )
        ENDIF
        DO JJ = 1, MIN(NJ,NJMAX)
          IF ( J_ET(JJ).GT.1.0 ) THEN
            CALL PHYETA_DETETA(ZV, J_ETA(JJ), DET_ETA)
            X = J_ET(JJ)
            KK = 2
            IF ( ABS(DET_ETA) .LE.0.6) KK = 1
            IF ( ABS(DET_ETA) .GT.1.6) KK = 3

            IF (X.GE.L1CUT(2)) THEN
              P17 = TANH(X - PL1_17_13(3,KK))*
     &              (1 - PL1_17_13(1,KK)*EXP(-X/PL1_17_13(2,KK)) )
            ENDIF
            IF (X.GE.L1CUT(1)) THEN
              IF (X.GE.MBCUT(KK)) THEN
                P13 = 1.0
              ELSE
                P13 = (TANH(X - PL1_13_00(3,KK))*
     &              (1 - PL1_13_00(1,KK)*EXP(-X/PL1_13_00(2,KK)) ))
              ENDIF
            ENDIF
            P17 = MIN(P17,1.0)
            P17 = MAX(0.0,P17)
            P13 = MIN(P13,1.0)
            P13 = MAX(0.0,P13)
            J_EFF = P17*P13
            L1_EFF = L1_EFF*(1.0 - J_EFF)
          ENDIF
        ENDDO
        P37 = MIN(P37,1.0)
        P37 = MAX(0.0,P37)
        P27 = MIN(P27,1.0)
        P27 = MAX(0.0,P27)
        L1_EFF = L1_EFF*P27*P37
        L1_EFF = 1 - L1_EFF
        ERROR = 0.0

      ELSEIF (FILT_NAME(1:LLEN).EQ.'JET_MAX') THEN
        P13 = 0.0
        P45 = 1.0
        J_EFF = 0.0
        X = -999.9
        DO JJ = 1, MIN(NJ,NJMAX)
          X = MAX(J_ET(JJ),X)
        ENDDO  
        IF (X .GT. L1CUT(5)) THEN
          P45 = TANH(X - PL1_45_13(3,KK))*
     &          (1 - PL1_45_13(1,KK)*EXP(-X/PL1_45_13(2,KK)) )
        ENDIF
        DO JJ = 1, MIN(NJ,NJMAX)
          IF ( J_ET(JJ).GT.1.0 ) THEN
            CALL PHYETA_DETETA(ZV, J_ETA(JJ), DET_ETA)
            X = J_ET(JJ)
            KK = 2
            IF ( ABS(DET_ETA) .LE.0.6) KK = 1
            IF ( ABS(DET_ETA) .GT.1.6) KK = 3
            
            IF (X.GE.L1CUT(1)) THEN
              IF (X.GE.MBCUT(KK)) THEN
                P13 = 1.0
              ELSE
                P13 = (TANH(X - PL1_13_00(3,KK))*
     &              (1 - PL1_13_00(1,KK)*EXP(-X/PL1_13_00(2,KK)) ))
              ENDIF
              P13 = MIN(P13,1.0)
              P13 = MAX(0.0,P13)
              J_EFF = P13
              L1_EFF = L1_EFF*(1.0 - J_EFF)
            ENDIF
          ENDIF
        ENDDO
        P13 = MIN(P45,1.0)
        P13 = MAX(0.0,P45)
        L1_EFF = L1_EFF*P45
        L1_EFF = 1 - L1_EFF
        ERROR = 0.0
      ENDIF
C
C
C
C *** Get L2 eff
C
      L2_EFF = 1.0
      LLEN = LEN(FILT_NAME)

      DO JJ = 1, MIN(NJ,NJMAX)
        IF ( J_ET(JJ).GT.1.0 ) THEN

          CALL PHYETA_DETETA(ZV, J_ETA(JJ), DET_ETA)
          X = J_ET(JJ)
          KK = 2
          IF ( ABS(DET_ETA) .LE.0.6) KK = 1
          IF ( ABS(DET_ETA) .GT.1.6) KK = 3

          J_EFF = 0.0
          IF (FILT_NAME(1:LLEN).EQ.'JET_MIN') THEN
            IF (J_ET(JJ).GE.L2CUT(1)) THEN
              J_EFF = TANH(J_ET(JJ) - PL2_120(3,KK))*
     &              (1 - PL2_120(1,KK)*EXP(-X/PL2_120(2,KK)) )
            ENDIF
          ELSEIF (FILT_NAME(1:LLEN).EQ.'JET_LOW') THEN
            IF (J_ET(JJ).GE.L2CUT(2)) THEN
              J_EFF = TANH(J_ET(JJ) - PL2_130(3,KK))*
     &              (1 - PL2_130(1,KK)*EXP(-X/PL2_130(2,KK)) )
            ENDIF
          ELSEIF (FILT_NAME(1:LLEN).EQ.'JET_MEDIUM') THEN
            IF (J_ET(JJ).GE.L2CUT(3)) THEN
              J_EFF = TANH(J_ET(JJ) - PL2_150(3,KK))*
     &              (1 - PL2_150(1,KK)*EXP(-X/PL2_150(2,KK)) )
            ENDIF
          ELSEIF (FILT_NAME(1:LLEN).EQ.'JET_HIGH') THEN
            IF (J_ET(JJ).GE.L2CUT(4)) THEN
              J_EFF = TANH(J_ET(JJ) - PL2_185(3,KK))*
     &              (1 - PL2_185(1,KK)*EXP(-X/PL2_185(2,KK)) )
            ENDIF
          ELSEIF (FILT_NAME(1:LLEN).EQ.'JET_MAX') THEN
            IF (J_ET(JJ).GE.L2CUT(5)) THEN
              J_EFF = TANH(J_ET(JJ) - PL2_1115(3,KK))*
     &              (1 - PL2_1115(1,KK)*EXP(-X/PL2_1115(2,KK)) )
            ENDIF
          ENDIF
          J_EFF = MIN(J_EFF,1.0)
          J_EFF = MAX(0.0,J_EFF)
          L2_EFF = L2_EFF*(1.0 - J_EFF)

        ENDIF
      ENDDO
      L2_EFF = 1.0 - L2_EFF
      ERROR = 0.0
C
C
C *** Get event eff
C         e_fail = e_fail^L1 + [1-e_fail^L1] * e_fail^L2
C
      EFF = (1.0 - L1_EFF) + L1_EFF*(1.0-L2_EFF)
      EFF = 1.0 - EFF 
      ERROR = 0.0

      RETURN
      END

