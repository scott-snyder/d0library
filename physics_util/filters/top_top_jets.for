      LOGICAL FUNCTION TOP_TOP_JETS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter TOP--> ALLJETS events from according to
C-                         the following criteria
C-          ETJET > ETMIN
C-          NJET = NJETS_MIN
C-   Inputs  : none
C-   Outputs : none
C-   Controls: TOP_TOP_JETS_RCP
C-
C-   Created  17-DEC-1992   Meenakshi Narain
C-   Updated  15-FEB-1994   Pushpa C. Bhat  Fixed major bug with
C-                          JETS_MAX_ETA and ABS(ETA) which made
C-                          first pass Run1a v11 streaming useless
C-                          for all jets (bug found by Greenlee)
C-   Updated  15-OCT-1994   Cathy E. Cretsinger   Add test for L2 filters,
C-                          controlled by RCP switch. 
C-   Updated  22-NOV-1994   Chip Stewart  got rid of ETTEST 
C-   Updated  27-OCT-1995   Norm Amos, check all vertices.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    ETMIN,ET,NEW_ET,ETA,NEW_ETA,TEMPLATE(5,4),JETS_MAX_ETA
      REAL    ZVTX_INFO(3,10),CAL_TH,TAN_CAL_THETA,SGN
      REAL    NEW_Z_VERT,ORIG_Z_VERT,NEW_THETA,ORIG_THETA,Z_DET,R_DET
      REAL    R_CC,Z_EC
      INTEGER NJETS,NJETS_MIN,IER,ICHOICE
      INTEGER LJETS,GZJETS
      INTEGER TBIT_ON(32),FBIT_ON(128)
      INTEGER NFILT_REQ,NTRIGON,NFILTON
      INTEGER IV,II,JJ
      INTEGER NVERT,NV
      LOGICAL FIRST,PASS_JTS,PASS_L2
      LOGICAL REQ_FILTERS,MATCH_WILD
      LOGICAL LOOP_VERTS,OKZ
      CHARACTER*32 CHFILT_REQ(128),TEST_NAME
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      DATA    FIRST/.TRUE./
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
      PARAMETER (R_CC = 91.6)
      PARAMETER (Z_EC = 178.9)
C
C----------------------------------------------------------------------
C
C ****  initialization
C
      PASS_JTS   = .FALSE.
      TOP_TOP_JETS = .FALSE.
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_TOP_JETS_RCP',IER)
        IF(IER.EQ.0) THEN
          CALL EZPICK('TOP_TOP_JETS_RCP')
          IF (IER.EQ.0) CALL EZGET('SELECT_FILTERS',REQ_FILTERS,IER)
          IF (IER.EQ.0) CALL EZGET('JETS_ALGORITHM',ICHOICE,IER)
          IF (IER.EQ.0) CALL EZGET('JETS_MIN_ET',ETMIN,IER)
          IF (IER.EQ.0) CALL EZGET('NJETS_MIN',NJETS_MIN,IER)
          IF (IER.EQ.0) CALL EZGET('JETS_MAX_ETA',JETS_MAX_ETA,IER)
          IF (IER.EQ.0) CALL EZGET('LOOP_OVER_ALL_VERTS',LOOP_VERTS,IER)
          IF (IER.EQ.0) CALL EZ_GET_CHARS('FILTER_NAMES',NFILT_REQ,
     &      CHFILT_REQ,IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_TOP_JETS','TOP_TOP_JETS_RCP',
     &        'ERROR GETTING TOP_TOP_JETS RCP VALUES','W')
          ENDIF
          CALL EZRSET
        ENDIF
      ENDIF
C
C  Check for appropriate filter bits.
      PASS_L2 = .TRUE.
      IF (REQ_FILTERS) THEN
        PASS_L2 = .FALSE.
        CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
        DO II = 1,NFILT_REQ
          TEST_NAME = CHFILT_REQ(II)
          DO JJ = 1, NFILTON
            PASS_L2=PASS_L2.OR.MATCH_WILD(FNAME_ON(JJ),TEST_NAME)
          ENDDO
        ENDDO
      ENDIF
C
C  Choose jets algorithm.
      IF(ICHOICE.EQ.1) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(ICHOICE.EQ.2) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(ICHOICE.EQ.3) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(ICHOICE.EQ.4) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
C
C  Get vertex list.
      NVERT = 1
      IF (LOOP_VERTS) THEN
        CALL VERTEX_INFO (10,NV,ZVTX_INFO,OKZ)
        IF (OKZ) NVERT = MIN(10,NV)
      ENDIF
C
C  Loop over all vertices and all jets.
      ORIG_Z_VERT = ZVTX_INFO(1,1)
      DO IV = 1, NVERT
        NJETS = 0
        NEW_Z_VERT = ZVTX_INFO(1,IV)
        LJETS=GZJETS()
        IF (LJETS.NE.0) THEN
          CALL ZSORT(IXCOM,LJETS,6)
          LJETS=GZJETS()
          CALL ZTOPSY(IXCOM,LJETS)
          LJETS=GZJETS()
          DO WHILE (LJETS.GT.0)
            ET =SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
            ETA = Q(LJETS+9)
            ORIG_THETA = Q(LJETS+7)
            TAN_CAL_THETA = TAN(CAL_TH(ORIG_THETA,ORIG_Z_VERT))
            Z_DET = R_CC / TAN_CAL_THETA
            IF (ABS(Z_DET).LT.Z_EC) THEN      ! in the CC
              NEW_THETA = ATAN2(R_CC,Z_DET-NEW_Z_VERT)
              NEW_ETA = -LOG(TAN(NEW_THETA/2.))
              NEW_ET = ET*ABS(SIN(NEW_THETA)/SIN(ORIG_THETA))
            ELSE                              ! in the EC
              SGN = SIGN(1.,TAN_CAL_THETA)
              R_DET = TAN_CAL_THETA * Z_EC * SGN
              NEW_THETA = ATAN2(R_DET,Z_EC*SGN-NEW_Z_VERT)
              NEW_ETA = -LOG(TAN(NEW_THETA/2.))
              NEW_ET = ET*ABS(SIN(NEW_THETA)/SIN(ORIG_THETA))
            ENDIF
            IF (NEW_ET.GE.ETMIN) THEN
              IF (ABS(NEW_ETA).LT.JETS_MAX_ETA) THEN
                NJETS=NJETS+1
              ENDIF
            ENDIF
            LJETS=LQ(LJETS)          ! pointer to next jet
          ENDDO
        ENDIF
        IF (NJETS.GE.NJETS_MIN) PASS_JTS = .TRUE.
      ENDDO
C
      CALL RESET_CAPH
C
      IF (PASS_L2.AND.PASS_JTS) TOP_TOP_JETS = .TRUE.
C
  999 RETURN
      END
