      SUBROUTINE ACTUAL_GEOMETRY (ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extracts from the D0$STP:CALTOWER_STP.DAT
C-                         Calorimeter Geometry file, the Trigger Tower
C-                         coordinates to be used in the Lookup Tables.
C-
C-   Inputs  : D0$STP:CALTOWER_STP.DAT file.
C-
C-   Outputs : ERR : CAISTP error code.
C-             TOWER_RZ_COORD, TOWER_PHI_COORD and ANALOG_INPUT_SCALING
C-             (LEVEL1_LOOKUP common).
C-
C-   Controls: None.
C-
C-   Created  12-APR-1990   Sylvain Tisserant (MSU)
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
C
      INTEGER ERR
C
      INTEGER ETA_SIGN, ETA, PHI, CHANNEL, RZ
      INTEGER IETAC, IPHIC, LAYERC, FLOOR, N, I
      INTEGER LAYER_MIN(8), LAYER_MAX(8)
      REAL    X, Y, Z, TWOPI, AZIMUTH, R
      LOGICAL FIRST, EXIST
C
      INTEGER N_CELL (POS_ETA:NEG_ETA,
     +                ETA_MIN:ETA_MAX,
     +                PHI_MIN:PHI_MAX, 8)
      REAL    X_CELL (POS_ETA:NEG_ETA,
     +                ETA_MIN:ETA_MAX,
     +                PHI_MIN:PHI_MAX, 8, 16),
     +        Y_CELL (POS_ETA:NEG_ETA,
     +                ETA_MIN:ETA_MAX,
     +                PHI_MIN:PHI_MAX, 8, 16),
     +        Z_CELL (POS_ETA:NEG_ETA,
     +                ETA_MIN:ETA_MAX,
     +                PHI_MIN:PHI_MAX, 8, 16)
C
      DATA FIRST /.TRUE./
      DATA LAYER_MIN / 1, 2, 3, 7, 11, 12, 13, 14 /
      DATA LAYER_MAX / 1, 2, 6, 7, 11, 12, 13, 14 /
C
C----------------------------------------------------------------------
C
C     Unmask all Trigger Towers
C     =========================
C
      DO CHANNEL = EM_TOWER, HD_TOWER
        DO PHI = PHI_MIN, PHI_MAX
          DO ETA = ETA_MIN, ETA_MAX
            DO ETA_SIGN = POS_ETA, NEG_ETA
              DAC_BYTE (ETA_SIGN, ETA, PHI, CHANNEL) = 1
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C     Seek Trigger Tower components
C     =============================
C
      IF(FIRST) THEN
        CALL CAISTP ('D0$STP:CALTOWER_STP.DAT',ERR)
        IF(ERR.NE.0) RETURN
        DO FLOOR = 1,8
          DO LAYERC = LAYER_MIN(FLOOR), LAYER_MAX(FLOOR)
            DO IPHIC = 1, NPHIL
              DO IETAC = -NETAL, NETAL
                CALL WHICH_TRIGGER_TOWER (IETAC, IPHIC, LAYERC,
     +                                    ETA_SIGN, ETA, PHI, CHANNEL,
     +                                    EXIST)
                IF(EXIST) THEN
                  CALL CELXYZ (IETAC, IPHIC, LAYERC, X, Y, Z, ERR)
                  N = N_CELL(ETA_SIGN, ETA, PHI, FLOOR)
                  N = N + 1
                  N_CELL(ETA_SIGN, ETA, PHI, FLOOR)    = N
                  X_CELL(ETA_SIGN, ETA, PHI, FLOOR, N) = X
                  Y_CELL(ETA_SIGN, ETA, PHI, FLOOR, N) = Y
                  Z_CELL(ETA_SIGN, ETA, PHI, FLOOR, N) = Z
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        TWOPI = 2. * ACOS(-1.)
        FIRST = .FALSE.
      ENDIF
C
C     Trigger Tower coordinates
C     =========================
C
      DO CHANNEL = EM_TOWER, HD_TOWER
        IF(CHANNEL.EQ.EM_TOWER) THEN
          FLOOR = 3
        ELSE
          FLOOR = 5
        ENDIF
        DO PHI = PHI_MIN, PHI_MAX
          DO ETA = ETA_MIN, ETA_MAX
            DO ETA_SIGN = POS_ETA, NEG_ETA
              X = 0.
              Y = 0.
              Z = 0.
              N = N_CELL(ETA_SIGN, ETA, PHI, FLOOR)
              IF(N.NE.0) THEN
                DO I = 1, N
                  X = X + X_CELL(ETA_SIGN, ETA, PHI, FLOOR, I)
                  Y = Y + Y_CELL(ETA_SIGN, ETA, PHI, FLOOR, I)
                  Z = Z + Z_CELL(ETA_SIGN, ETA, PHI, FLOOR, I)
                ENDDO
                R = SQRT(X**2+Y**2)
                TOWER_RZ_COORD(ETA_SIGN,ETA,PHI,CHANNEL,R_COORD) = R/N
                TOWER_RZ_COORD(ETA_SIGN,ETA,PHI,CHANNEL,Z_COORD) = Z/N
                AZIMUTH = ATAN2(Y,X)
                IF(AZIMUTH.LT.0.) AZIMUTH = AZIMUTH + TWOPI
                TOWER_PHI_COORD(ETA_SIGN, ETA, PHI) = AZIMUTH
                ANALOG_INPUT_SCALING(ETA_SIGN,ETA,PHI,CHANNEL) =
     +                                                R/SQRT(R**2+Z**2)
              ELSE
                TOWER_RZ_COORD(ETA_SIGN,ETA,PHI,CHANNEL,R_COORD) = 0.
                TOWER_RZ_COORD(ETA_SIGN,ETA,PHI,CHANNEL,Z_COORD) = 0.
                TOWER_PHI_COORD(ETA_SIGN, ETA, PHI) = 0.
                ANALOG_INPUT_SCALING(ETA_SIGN,ETA,PHI,CHANNEL) = 1.
                ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      DO RZ = R_COORD, Z_COORD
        DO PHI = PHI_MIN, PHI_MAX
          DO ETA = ETA_MIN, ETA_MAX
            DO ETA_SIGN = POS_ETA, NEG_ETA
              TOWER_RZ_COORD(ETA_SIGN,ETA,PHI,TOT_TOWER,RZ) =
     +          (TOWER_RZ_COORD(ETA_SIGN,ETA,PHI,EM_TOWER,RZ) +
     +           TOWER_RZ_COORD(ETA_SIGN,ETA,PHI,HD_TOWER,RZ)  )/2.
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      ERR = 0
      END
