      SUBROUTINE LDSP_GET_SUMS(TTETA,TTPHI,
     &                         EM_CLUSTER_SIZE,TOT_CLUSTER_SIZE,
     &                         EM_CLUSTER_ET,TOT_CLUSTER_ET,
     &                         TWOBYONE_ETA,TWOBYONE_PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET ENERGY SUMS FOR 2X1,2X2,3X3,5X5
C-                         CLUSTERS
C-
C-   Inputs  :
C-             TTETA    - INTEGER CANDIDATE ETA
C-             TTPHI    - INTEGER CANDIDATE PHI
C-             EM_CLUSTER_SIZE  - CHAR*3 CLUSTER SIZE
C-             TOT_CLUSTER_SIZE - CHAR*3 CLUSTER SIZE
C-               cluster size choices - '2X1','2X2','3X3','5X5'
C-   Outputs :
C-             EM_CLUSTER_ET   -  REAL EM CLUSTER ET
C-             TOT_CLUSTER_ET  -  REAL TOT CLUSTER ET
C-             TWOBYONE_ETA    -  INT DELTA ETA OF 2ND TOWER IN 2X1 CLUSTER
C-             TWOBYONE_PHI    -  INT DELTA PHI OF 2ND TOWER IN 2X1 CLUSTER
C-   Controls:
C-
C-   Created  22-NOV-1993   sFahey
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_REFSET_THRESHOLDS.INC'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_LOCAL_DSP.PARAMS'
      INCLUDE 'D0$INC:L15_LOCAL_DSP.INC'
C
      CHARACTER*3 EM_CLUSTER_SIZE
      CHARACTER*3 TOT_CLUSTER_SIZE
      INTEGER J,X,Y,L1ETAC,L1PHIC
      INTEGER TTETA,TTPHI
      REAL EM_2X1,TOT_2X1
      REAL EM_2X2,TOT_2X2
      REAL EM_3X3,TOT_3X3
      REAL EM_5X5,TOT_5X5
      REAL EM_CLUSTER_ET
      REAL TOT_CLUSTER_ET
      REAL EM_2X1_TEMP,TOT_2X1_TEMP
      REAL EM_2X2_TEMP,TOT_2X2_TEMP
      INTEGER XSTEP(4),YSTEP(4)
      DATA XSTEP/-1,-1, 1, 1/
      DATA YSTEP/-1, 1,-1, 1/
      INTEGER TWOBYONE_ETA,TWOBYONE_PHI
      INTEGER TWOBYONE_X(4),TWOBYONE_Y(4)
      DATA TWOBYONE_X/ 0, 1, 0,-1/
      DATA TWOBYONE_Y/ 1, 0,-1, 0/
C
C----------------------------------------------------------------------
C
C
C   GET 3X3, 5X5 SUMS FOR CANDIDATES
C
      EM_3X3 = 0.0
      TOT_3X3 = 0.0
      EM_5X5 = 0.0
      TOT_5X5 = 0.0
      DO X = -2,2
        L1ETAC = TTETA + X
        IF ((TTETA.GT.0).AND.(L1ETAC.LE.0))
     &                      L1ETAC = L1ETAC - 1     ! CROSSING L1ETA=0
        IF ((TTETA.LT.0).AND.(L1ETAC.GE.0))
     &                      L1ETAC = L1ETAC + 1     !     TOWER
        IF (ABS(L1ETAC).LE.20) THEN
          DO Y = -2,2
            L1PHIC = TTPHI + Y
            IF (L1PHIC.GT.32) L1PHIC = L1PHIC - 32       ! PHI WRAP
            IF (L1PHIC.LT.1) L1PHIC = L1PHIC + 32
            EM_5X5 = EM_5X5 + EMET(L1ETAC,L1PHIC)
            TOT_5X5 = TOT_5X5 + TOTET(L1ETAC,L1PHIC)
            IF ((ABS(X).LE.1).AND.(ABS(Y).LE.1)) THEN
              EM_3X3 = EM_3X3 + EMET(L1ETAC,L1PHIC)
              TOT_3X3 = TOT_3X3 + TOTET(L1ETAC,L1PHIC)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C
C   GET 2X2 SUMS
C
      EM_2X2 = 0.0
      TOT_2X2 = 0.0
C
      DO J = 1,4
        EM_2X2_TEMP = 0.0
        TOT_2X2_TEMP = 0.0
C
        DO X = 0,XSTEP(J),XSTEP(J)
          L1ETAC = TTETA + X
          IF ((TTETA.GT.0).AND.(L1ETAC.LE.0))
     &                      L1ETAC = L1ETAC - 1     ! CROSSING L1ETA=0
          IF ((TTETA.LT.0).AND.(L1ETAC.GE.0))
     &                      L1ETAC = L1ETAC + 1     !     TOWER
          IF (ABS(L1ETAC).LE.20) THEN
C
            DO Y = 0,YSTEP(J),YSTEP(J)
              L1PHIC = TTPHI + Y
              IF (L1PHIC.GT.32) L1PHIC = L1PHIC - 32       ! PHI WRAP
              IF (L1PHIC.LT.1) L1PHIC = L1PHIC + 32
              EM_2X2_TEMP = EM_2X2_TEMP + EMET(L1ETAC,L1PHIC)
              TOT_2X2_TEMP = TOT_2X2_TEMP + TOTET(L1ETAC,L1PHIC)
            ENDDO
            IF (EM_2X2_TEMP.GT.EM_2X2) THEN
              EM_2X2 = EM_2X2_TEMP
              TOT_2X2 = TOT_2X2_TEMP
            ENDIF
C
          ENDIF
        ENDDO
C
      ENDDO
C
C   GET 2X1 SUMS
C
      EM_2X1 = 0.0
      TOT_2X1 = 0.0
      TWOBYONE_ETA = 0
      TWOBYONE_PHI = 0
C
      DO J = 1,4
        EM_2X1_TEMP = EMET(TTETA,TTPHI)
        TOT_2X1_TEMP = TOTET(TTETA,TTPHI)
C
        L1ETAC = TTETA + TWOBYONE_X(J)
        IF ((TTETA.GT.0).AND.(L1ETAC.LE.0))
     &                      L1ETAC = L1ETAC - 1     ! CROSSING L1ETA=0
        IF ((TTETA.LT.0).AND.(L1ETAC.GE.0))
     &                      L1ETAC = L1ETAC + 1     !     TOWER
        IF (ABS(L1ETAC).LE.20) THEN
C
          L1PHIC = TTPHI + TWOBYONE_Y(J)
          IF (L1PHIC.GT.32) L1PHIC = L1PHIC - 32       ! PHI WRAP
          IF (L1PHIC.LT.1) L1PHIC = L1PHIC + 32
          EM_2X1_TEMP = EM_2X1_TEMP + EMET(L1ETAC,L1PHIC)
          TOT_2X1_TEMP = TOT_2X1_TEMP + TOTET(L1ETAC,L1PHIC)
          IF (EM_2X1_TEMP.GT.EM_2X1) THEN
            EM_2X1 = EM_2X1_TEMP
            TOT_2X1 = TOT_2X1_TEMP
            TWOBYONE_ETA = TWOBYONE_X(J)
            TWOBYONE_PHI = TWOBYONE_Y(J)
          ENDIF
C
        ENDIF
C
      ENDDO
C
C
C
      IF     ((EM_CLUSTER_SIZE.EQ.'2X1').OR.
     &        (EM_CLUSTER_SIZE.EQ.'2x1')) THEN
        EM_CLUSTER_ET = EM_2X1
      ELSEIF ((EM_CLUSTER_SIZE.EQ.'2X2').OR.
     &        (EM_CLUSTER_SIZE.EQ.'2x2')) THEN
        EM_CLUSTER_ET = EM_2X2
      ELSEIF ((EM_CLUSTER_SIZE.EQ.'3X3').OR.
     &        (EM_CLUSTER_SIZE.EQ.'3x3')) THEN
        EM_CLUSTER_ET = EM_3X3
      ELSEIF ((EM_CLUSTER_SIZE.EQ.'5X5').OR.
     &        (EM_CLUSTER_SIZE.EQ.'5x5')) THEN
        EM_CLUSTER_ET = EM_5X5
      ELSE
        CALL ERRMSG('LDSP_GET_SUMS','L15_CAL_SIM',
     &    'INVALID EM ET CLUSTER SIZE','E')
      ENDIF
C
      IF     ((TOT_CLUSTER_SIZE.EQ.'2X1').OR.
     &        (TOT_CLUSTER_SIZE.EQ.'2x1')) THEN
        TOT_CLUSTER_ET = TOT_2X1
      ELSEIF ((TOT_CLUSTER_SIZE.EQ.'2X2').OR.
     &        (TOT_CLUSTER_SIZE.EQ.'2x2')) THEN
        TOT_CLUSTER_ET = TOT_2X2
      ELSEIF ((TOT_CLUSTER_SIZE.EQ.'3X3').OR.
     &        (TOT_CLUSTER_SIZE.EQ.'3x3')) THEN
        TOT_CLUSTER_ET = TOT_3X3
      ELSEIF ((TOT_CLUSTER_SIZE.EQ.'5X5').OR.
     &        (TOT_CLUSTER_SIZE.EQ.'5x5')) THEN
        TOT_CLUSTER_ET = TOT_5X5
      ELSE
        CALL ERRMSG('LDSP_GET_SUMS','L15_CAL_SIM',
     &    'INVALID TOTAL ET CLUSTER SIZE','E')
      ENDIF
C
  999 RETURN
      END
