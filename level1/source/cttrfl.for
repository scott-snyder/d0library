      SUBROUTINE CTTRFL (NOISE, CUTOFF,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills the CTTR bank starting from
C-                         CAEP.
C-
C-   Inputs  : NOISE [L] : If .TRUE. Trigger Tower Electronics noise
C-                         contribution is added to energies.
C-
C-   Outputs : OK [L] : .TRUE. if successful booking and filling.
C-
C-   Controls: None.
C-
C-   Created   9-AUG-1990   Sylvain Tisserant (MSU)
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      LOGICAL NOISE, OK
      INTEGER GZCTTR
      REAL    CUTOFF
C
      LOGICAL FIRST,START
      REAL    NOISE_SIGMA(2,20), SIN_THETA(2,20), PSEUDO_RAPIDITY(2,20)
      REAL    AZIMUTH(32),  TT(2,20,32,2)
      INTEGER CHANNEL, PHI, ETA, ETA_SIGN, LAYERC, IPHIC, IETAC
      INTEGER ERR, PNTR,NR,BITS,ICH,SIGN(2)
      REAL    THETA, TWOPI, DPHI, ENERGY,R1
      LOGICAL EXIST
      INTEGER N(2), EM_TT,        HD_TT
      EQUIVALENCE  (EM_TT,N(1)), (HD_TT,N(2))
C                                             Electronics noise variances in GeV
C                                             ----------------------------------
      DATA NOISE_SIGMA /
     + 0.340, 1.200, 0.340, 1.200, 0.340, 1.300, 0.350, 1.300,
     + 0.360, 0.710, 0.300, 0.710, 0.250, 1.300, 0.640, 1.000,
     + 0.560, 0.800, 0.510, 0.920, 0.490, 0.930, 0.470, 0.860,
     + 0.470, 0.840, 0.360, 0.790, 0.350, 0.750, 0.350, 0.720,
     + 0.270, 0.590, 0.270, 0.580, 0.260, 0.570, 0.000, 0.530  /
C                                                                    Sinus theta
C                                                                    -----------
      DATA SIN_THETA /
     + 0.995, 0.995, 0.956, 0.956, 0.886, 0.886, 0.796, 0.796,
     + 0.697, 0.697, 0.611, 0.600, 0.611, 0.508, 0.425, 0.428,
     + 0.354, 0.350, 0.293, 0.293, 0.241, 0.241, 0.199, 0.199,
     + 0.163, 0.163, 0.137, 0.134, 0.112, 0.110, 0.092, 0.090,
     + 0.077, 0.073, 0.061, 0.057, 0.045, 0.041, 1.000, 0.028  /
C
      DATA FIRST / .TRUE. /, SIGN / 1,-1/
C
C----------------------------------------------------------------------
C
      IF ( GZCTTR().NE.0 ) THEN
        OK=.TRUE.
        RETURN
      ELSE
        OK = .FALSE.
      ENDIF
C                                       Trigger Tower Eta and Phi initialization
C                                       ----------------------------------------
      IF (FIRST) THEN
        DO CHANNEL = 1, 2
          DO ETA = 1, 20
            THETA = ASIN(SIN_THETA(CHANNEL,ETA))
            PSEUDO_RAPIDITY(CHANNEL,ETA) = -ALOG(TAN(THETA/2.))
          ENDDO
        ENDDO
        TWOPI = 2.*ACOS(-1.)
        DPHI  = TWOPI/32.
        DO PHI = 1, 32
          AZIMUTH(PHI) = (PHI-0.5) * DPHI
        ENDDO
        FIRST = .FALSE.
      ENDIF
C                         Energy presetting, with electronics noise if requested
C                         ------------------------------------------------------
      DO CHANNEL = 1, 2
        DO PHI = 1, 32
          DO ETA = 1, 20
            DO ETA_SIGN = 1, 2
              IF(NOISE) THEN
                CALL NORRAN(R1)
                TT(ETA_SIGN,ETA,PHI,CHANNEL) = 
     +            NOISE_SIGMA(CHANNEL,ETA) * R1
              ELSE
                TT(ETA_SIGN,ETA,PHI,CHANNEL) = 0.
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C                                                         Trigger Tower Energies
C                                                         ----------------------
      ERR=0
      START=.TRUE.
      DO WHILE (ERR.EQ.0)
        CALL GTCAEP(START,IETAC,IPHIC,LAYERC,BITS,ENERGY,ICH,ERR)
        START=.FALSE.
        IF(ERR.EQ.-2) RETURN
        IF(ERR.EQ.0) THEN
          CALL WHICH_TRIGGER_TOWER (IETAC, IPHIC, LAYERC,
     +                                  ETA_SIGN, ETA, PHI, CHANNEL,
     +                                  EXIST)
          IF(EXIST) TT(ETA_SIGN, ETA, PHI, CHANNEL) =
     +                  TT(ETA_SIGN, ETA, PHI, CHANNEL) + ENERGY
        ENDIF
      ENDDO
C                               Counts the Trigger Towers to be stored into CTTR
C                               ------------------------------------------------
      DO CHANNEL = 1, 2
        N(CHANNEL) = 0
        DO PHI = 1, 32
          DO ETA = 1, 20
            DO ETA_SIGN = 1, 2
              ENERGY   = TT(ETA_SIGN, ETA, PHI, CHANNEL)
              IF(CHANNEL.EQ.2)
     &          ENERGY=ENERGY+TT(ETA_SIGN,ETA,PHI,1)
              IF(ENERGY.GT.CUTOFF) N(CHANNEL) = N(CHANNEL) + 1
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C                                                              CTTR bank booking
C                                                              -----------------
      CALL BKCTTR (EM_TT, HD_TT, PNTR)
      IF(PNTR.EQ.0) RETURN
C                                                              CTTR bank filling
C                                                              -----------------
      NR=IQ(PNTR+2)
      IF(NOISE) IQ(PNTR+4)=1
      PNTR = PNTR + 5
      DO CHANNEL = 1, 2
        DO PHI = 1, 32
          DO ETA = 1, 20
            DO ETA_SIGN = 1, 2
              ENERGY   = TT(ETA_SIGN, ETA, PHI, CHANNEL)
              IF(CHANNEL.EQ.2)
     &          ENERGY=ENERGY+TT(ETA_SIGN,ETA,PHI,1)
              IF(ENERGY.GT.CUTOFF) THEN
                Q(PNTR)=ENERGY
                Q (PNTR+1) = ENERGY* SIN_THETA(CHANNEL, ETA)
                Q (PNTR+2) = PSEUDO_RAPIDITY(CHANNEL, ETA) *
     +                          SIGN(ETA_SIGN)
                Q (PNTR+3) = AZIMUTH(PHI)
                IQ(PNTR+4) = ETA*SIGN(ETA_SIGN)
                IQ(PNTR+5) = PHI
                IQ(PNTR+6) = CHANNEL
                PNTR = PNTR + NR
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C      
      OK = .TRUE.
      RETURN
      END
