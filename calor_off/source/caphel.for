      FUNCTION CAPHEL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find Electrons and Photons
C-
C-   Returned value  : .TRUE. if all OK
C-   Inputs  : CATE,CAEP and CAEH banks
C-   Outputs : CACL,CACH,PELC and PPHO banks
C-   Controls: None
C-
C-   Created  18-APR-1989   Rajendran Raja
C-   Modified 10-OCT-1989   Gerald C. Blazey
C-                          Add SCEXP to CACLFL list.
C-   Updated  12-OCT-1989   Harrison B. Prosper
C-   Added G. Blazey's changes to CLDROP
C-   Updated  19-JAN-1990   N.A. Graf
C-                          Added EMATPREDICTL:
C-                          Longitudinal matrix elements now
C-                          parameterized.
C-   Updated 11-FEB-1990    N.A. Graf
C-                          Added Full transverse prediction
C-                          EMATPREDICT
C-   Modified 30-MAR-1990   N.A. Graf
C-                          Can cut on EM ratio
C-   Modified 4-APR-1990    Remove H matrix analysis to CHMANL
C-   Updated  13-SEP-1990   Harrison B. Prosper
C-      Added CAPH code
C-   Updated  14-SEP-1990   Norman A. Graf  Added ZTRK matching code
C-   Updated  15-SEP-1990   Harrison B. Prosper
C-      Modified CAPH code
C-   Updated  18-MAR-1992   Rajendran Raja  CHMANL_NEW CALL TO callthe Hmatrix
C-   package
C-   Updated  20-OCT-1992   Meenakshi Narain   
C-                          MODIFICATIONS IMPLEMENTED TO RERUN FROM DST
C-   Updated  23-OCT-1992   Meenakshi Narain  Add electron quality  
C-
C-   Updated  27-OCT-1992   Natalie Roe  use position from tuned log weighted
C                                      algorithm; also add switch for CLEANEM
C-   Updated  12-APR-1993   Meenakshi Narain   fix cleanem status word
C-
C-   Updated   8-SEP-1994   Meenakshi Narain   
C-                            Add options for SELECTIVE ROAD tracking 
C-                            depending on Et thresholds or CHISQ cut
C-   Updated  16-SEP-1994   Meenakshi Narain   
C-                            Fill tarck-match significance in word 20 of
C-                           PELC banks
C-   Updated  17-DEC-1994   Meenakshi Narain   Add calls to TRD for
C-                          every ZTRK in road and also for PPHO
C-                          Save TRD links in cacl ref link 5. 
C-   Updated  12-FEB-1995   Meenakshi Narain   CALL CM3POS_PV
C-   Updated  28-FEB-1995   Meenakshi Narain   ADD call to global fit package
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$LINKS:IZZTMP.LINK'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:CIMPACT.INC'
      INCLUDE 'D0$INC:ZTRLNK.INC'
C
      INTEGER GZCAEP,GZCAEH,GZCATE,GZCAPH,GZCACL,GZPELC,GZPPHO
      LOGICAL CAPHEL,CPHINI,OK,DOEM
      LOGICAL DO_HMATRIX,DO_PPHO_IN_TRD
      INTEGER IER,NCLUST
      INTEGER II,I,J
      REAL    PREDI,PTDUM
C
      REAL SCEXP,MXCACL,THREN,THRET,THREM,THRTR,FHLIM
      INTEGER ELECTRON
      PARAMETER( ELECTRON = 1  )        ! Electromagnetic clusters
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      LOGICAL USE_MONTE_VERTEX
C
      REAL    NEW_SHOWER_CENT(3)
      REAL    ZV(14),DZ(14),DEL_ZVERTEX
      REAL    ZVTX_INFO(3,14)
      REAL    PHI_CENTER,PHI_LO,PHI_HI,PHI_ROAD
      REAL    THETA_ROAD_FACT,THETA_LO,THETA_HI,MIN_THETA_ROAD
      REAL    THETA_NEW,THETA_NLO,THETA_NHI
C      REAL    THMIN,THMAX
      INTEGER NVER,ILINKS(3),NV
C
      REAL    RHO,DELZ,TRK_CENT(3),DIR_COS(3),DCL
      INTEGER NTRAKS,ZLINK_TRAKS(zmax),NTEMP(zmax),ZWANT
      INTEGER NEWTRAKS,ZLINK_NEWTRAKS(zmax)
      INTEGER GZZTRH
      LOGICAL LZTRKS,ALL_TRACKS
C
      REAL ECLUS,NEW_ECLUS
      REAL ETAPHI(3),DETAPHI(3)
      REAL DCLA,NZTRAKS,ETRANS_RATIO,EM_RATIO
      REAL    XTEST
C
      INTEGER NCELL, CENTRAL_TOWER,LEVEL
      INTEGER IETA,IPHI,LAYER
      LOGICAL READIN,ARGSOK
C
      LOGICAL ELECT,RERUN,ABORT_PROC
      LOGICAL USE_LOG
      LOGICAL DO_HMATRIX_ANAL,DO_NTUPLE,DO_CLEANEM,DO_ELFIT
      INTEGER GZISV1
C
      LOGICAL RERUN_DST,USE_CASH,USE_CASH_INFO,OKZ
      INTEGER LCLUS,TRK,STATUS,MVAR
      LOGICAL SELECTIVE_EC_TRACKING,SELECTIVE_CC_TRACKING
      LOGICAL SELECTIVE_VERTLOOP,CHISQ_CUTOFF
      REAL    ETCLUS,ETNEW,CHISQ
      REAL    ET_ECTRK_THR,ET_CCTRK_THR,ET_ECTRK_THR_VERTLOOP
      REAL    CHISQ_ECTRK_THR,CHISQ_CCTRK_THR
      REAL    TQUAN(50)

C----------------------------------------------------------------------
      OK = CPHINI()                     ! Needs to be called each time
C
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('RERUN_CAPHEL',RERUN,IER)
        CALL EZGET('RERUN_CAPHEL_FROM_DST',RERUN_DST,IER)
        CALL EZGET('POWER_FOR_SHOWER_CENTER',SCEXP,IER)
        CALL EZGET('MAXIMUM_CAL_CLUSTERS',MXCACL,IER)
        CALL EZGET('CLUSTER_ENERGY_THRESHOLD',THREN,IER)
        CALL EZGET('CLUSTER_ET_THRESHOLD',THRET,IER)
        CALL EZGET('CLUSTER_EM_RATIO_THRESHOLD',THREM,IER)
        CALL EZGET('CLUSTER_ETRANS_THRESHOLD',THRTR,IER)
        CALL EZGET('WEIGHT_CUT',WEIGHT_CUT,IER)
        CALL EZGET('FH_LAYER1_LIMIT',FHLIM,IER)
        CALL EZGET('DO_HMATRIX',DO_HMATRIX,IER)
        CALL EZGET('DO_CLEANEM',DO_CLEANEM,IER)
        CALL EZGET('DO_ELFIT',DO_ELFIT,IER)
        CALL EZGET('PHI_ROAD',PHI_ROAD,IER)
        CALL EZGET('THETA_ROAD_FACT',THETA_ROAD_FACT,IER)
        CALL EZGET('MIN_THETA_ROAD',MIN_THETA_ROAD,IER)
        CALL EZGET('MIN_VERTEX_Z_ERROR',DEL_ZVERTEX,IER)
        CALL EZGET('DO_ZTRAKS',LZTRKS,IER)
        CALL EZGET('USE_LOG_WEIGHTED_CENTER',USE_LOG,IER)
        CALL EZGET('DO_HMATRIX_ANALYSIS',DO_HMATRIX_ANAL,IER)
        CALL EZGET('DO_HMATRIX_NTUPLE',DO_NTUPLE,IER)
        CALL EZGET('USE_MONTE_CARLO_VERTEX',USE_MONTE_VERTEX,IER)
        CALL EZGET('SELECTIVE_EC_TRACKING',SELECTIVE_EC_TRACKING,IER)
        CALL EZGET('SELECTIVE_CC_TRACKING',SELECTIVE_CC_TRACKING,IER)
        CALL EZGET('SELECTIVE_VERTLOOP',SELECTIVE_VERTLOOP,IER)
        CALL EZGET('ET_ECTRK_THR',ET_ECTRK_THR,IER)
        CALL EZGET('ET_CCTRK_THR',ET_CCTRK_THR,IER)
        CALL EZGET('ET_ECTRK_THR_VERTLOOP',ET_ECTRK_THR_VERTLOOP,IER)
        CALL EZGET('APPLY_CHISQ_CUTOFF',CHISQ_CUTOFF,IER)
        CALL EZGET('CHISQ_ECTRK_THR',CHISQ_ECTRK_THR,IER)
        CALL EZGET('CHISQ_CCTRK_THR',CHISQ_CCTRK_THR,IER)
        CALL EZGET('DO_PPHO_IN_TRD',DO_PPHO_IN_TRD,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('CAPHEL','CALORIMETER',
     &        'problem reading CAPHEL RCP parameters','F')
        ENDIF
        CALL EZRSET
        CALL EZPICK('ZTRAKS_RCP')
        CALL EZGET('ALL_TRACKS',ALL_TRACKS,IER)
        CALL EZRSET
C        CALL EZPICK('TRD_RCP')
C        CALL EZGET('THMIN',THMIN,IER)
C        CALL EZGET('THMAX',THMAX,IER)
C        THMIN = RADIAN*THMIN
C        THMAX = RADIAN*THMAX
C        CALL EZRSET
        USE_CASH = .FALSE.
        IF (RERUN_DST) THEN
          USE_CASH = .TRUE.
          CALL EZPICK('HMATRIX_RCP')
          CALL EZGET('USE_CASH_INFO',USE_CASH_INFO,IER)
          IF (.NOT.USE_CASH_INFO) THEN
            USE_CASH_INFO = .TRUE.
            CALL EZSET('USE_CASH_INFO',USE_CASH_INFO,IER)
            CALL ERRMSG('CAPHEL','RERUN_HMATRIX',
     &        'USING Cell energies from CASH BANKS for HMATRIX','W')
          ENDIF
        ENDIF
      ENDIF
C
      CAPHEL = .TRUE.
      ELECT = .FALSE.
C
C ****  CHECK IF PELC/PPHO ALREADY EXIST...
C
      LPELC = GZPELC()
      LPPHO = GZPPHO()
      LCAPH = GZCAPH()
C
      DO WHILE(LCAPH .GT. 0)
        IF(IQ(LCAPH+4).EQ. 1) THEN          ! Electrons
          IF(RERUN) THEN                    ! Recreate PELC and PPHO
            CALL MZDROP(IXCOM,LCAPH,' ')    ! Drop CAPH
            IF (LPELC .GT. 0) CALL MZDROP(IXCOM,LPELC,'L')    ! Drop all
                                                              ! PELC banks
            IF (LPPHO .GT. 0) CALL MZDROP(IXCOM,LPPHO,'L')    ! Drop all
                                                              ! PPHO banks
            LCAPH = 0
          ELSE IF (RERUN_DST) THEN
            IF (LPELC .GT. 0) CALL MZDROP(IXCOM,LPELC,'L')    ! Drop all
                                                              ! PELC banks
            IF (LPPHO .GT. 0) CALL MZDROP(IXCOM,LPPHO,'L')    ! Drop all
                                                              ! PPHO banks
            GOTO 555
          ELSE
            GOTO 900                        ! Analyze existing banks
          ENDIF
        ENDIF
        IF(LCAPH .NE. 0) LCAPH = LQ(LCAPH)
      ENDDO
C
C ****  If here, need following banks to proceed...
C
      LCAEP = GZCAEP()
      IF ( LCAEP.EQ.0 ) THEN
        CALL ERRMSG('CALORIMETER','CAPHEL',
     &    'CAEP BANK NOT SET UP','W')
        CAPHEL = .TRUE.
        RETURN
      ENDIF
C
      LCAEH = GZCAEH()
      IF ( LCAEH.EQ.0 ) THEN
        CALL ERRMSG('CALORIMETER','CAPHEL',
     &    'CAEH BANK NOT SET UP','W')
        CAPHEL = .TRUE.
        RETURN
      ENDIF
C
      LCATE = GZCATE()
      IF ( LCATE.EQ.0 ) THEN
        CALL ERRMSG('CALORIMETER','CAPHEL',
     &    'CATE BANK NOT SET UP','W')
        CAPHEL = .TRUE.
        RETURN
      ENDIF
C
C ****  Start electron/photon finding from scratch here...
C
C
C ****  Book/Fill CAPH bank for Electron-Photon algorithm
C
      CALL BKCAPH(LCAPH)
      IF ( LCAPH .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CAPHEL','Unable to book CAPH','W')
        CAPHEL = .TRUE.
        GOTO 999
      ENDIF
      CALL CAPHFL_INT(K_ALGORITHM,A_ELECTRON)
C
      CALL CALCLU(ELECTRON,IER)             ! Find CLUSTERS
      IF(IER.EQ.0)THEN
        CALL CACLFL(ELECTRON,SCEXP,NCLUST)        ! FILL CACL,CACH BANKS
        CALL CLDROP(MXCACL,THREN,THRET,THREM)     ! drop unwanted clusters
      ELSE
        CALL ERRMSG ('CALORIMETER','CAPHEL',
     &      'ERROR IN CLUSTER FINDING-CACLFL NOT CALLED ','W')
      ENDIF
C
      IF(IER.NE.0)THEN
        CAPHEL = .TRUE.
        GOTO 999
      ENDIF
C
C ****  Now begins the Electron/Photon finding...
C
  555 CONTINUE
C
C ****  Retrieve primary vertex...
C
      IF ( USE_MONTE_VERTEX ) THEN
        CALL ZVERTX(ZV,DZ)                ! Isajet Vertex
      ELSE
        CALL VERTEX_INFO(14,NVER,ZVTX_INFO,OKZ) ! Vertex from tracking
C            Only consider the main primary vertex
        IF ( OKZ ) THEN
          IF(NVER.EQ.0) THEN
            CALL ERRMSG('No Vertices','CAPHEL','z set to 0','W')
            CALL VZERO(ZV,14)
            CALL VZERO(DZ,14)
          ELSE
            NV = MIN(14,NVER)
            DO I = 1, NV
              ZV(I) = ZVTX_INFO(1,I)
              DZ(I) = ZVTX_INFO(2,I)
            ENDDO
          ENDIF
        ELSE
          CALL ERRMSG('Error getting Vertices',
     &      'CAPHEL','z set to 0','W')
          CALL VZERO(ZV,14)
          CALL VZERO(DZ,14)
        ENDIF
      ENDIF
C
C:::   Start of cluster do loop
C
      LCACL = GZCACL()
      DO WHILE(LCACL .NE. 0)
C
C ****  Store some information to start off with...
C
C
C
C ****  VERT IS LATER OVERWRIITEN BY ELECTRON TRACK CENTER FOR
C ****  USE IN CAL_HMATRIX_ANAL. IT SHOULD NOT AFFECT HMATRIX CHISQUARED
C ****  SINCE CAL_HMATRIX_ANAL IS CALLED AT THE END.
C
        VERT(3) = ZV(1)
        IF(DZ(1).LT.DEL_ZVERTEX)DZ(1) = DEL_ZVERTEX   ! Minimum value
C
        IF (.NOT.RERUN_DST) CALL CASHFL
        LCASH = LQ(LCACL-2)
C
        CALL CEMENR(NDPTH,ENDPTH,PEDPTH,ETOT,ET,ETRANS,EMAX,
     &    ETAMX,PHIMX,USE_CASH)
        IF (LCASH.NE.0) 
     &    CALL CASH_ETAPHIMX(LCASH,IETA,IPHI,IER)

        IF(USE_LOG)
     &    CALL CM3POS_PV(LCASH,WEIGHT_CUT,XBAR3,DBAR3,
     &    ETAPHI,DETAPHI,ZV(1))
 
C
        ECLUS = Q(LCACL+7)
        ETCLUS = Q(LCACL+8)
        ETRANS_RATIO = ETRANS/ECLUS
        IF(ETRANS_RATIO .GT. THRTR) GOTO 300
C
C ****  Can also cut on absolute energy in FH layer 1 here...
C
        IF(Q(LCACL+19) .GT. FHLIM) GOTO 300
C
C ****  IF rerunning from DST then setup EM3AV
C
        IF (RERUN_DST) THEN
          CALL UCOPY(Q(LCACL+14),EM3AV,3)
        ENDIF
C
C ****  Now for H matrix analysis...
C
        IF(DO_HMATRIX) THEN
          CALL CHMANL_NEW(ECLUS,NEW_ECLUS,ABORT_PROC,LEVEL)
          CHISQ = CHSQF
          IF ( ABORT_PROC ) THEN
            GO TO 888
          ENDIF
        ENDIF
C
        CALL UCOPY(Q(LCACL+14),SHOWER_CENTER,3)   ! Crude shower center
C
C
C ****  OVERWRITES CACL HERE, USING LOG WEIGHTING as default
C
        XTEST = XBAR3(1)**2 + XBAR3(2)**2 + XBAR3(3)**2
        IF ( XTEST.GT.0.0 .AND. ETAPHI(1).NE.-999.) THEN
C
C ****  LOG WEIGHTING HAS BEEN SUCCESSFUL
C
          RHO = SQRT(XBAR3(2)**2+XBAR3(1)**2)
          Q(LCACL+11) =  ATAN2(RHO,XBAR3(3)-ZV(1))  !Theta
          Q(LCACL+12) =  ETAPHI(2)                  !Phi
          Q(LCACL+13) = -ALOG(TAN(Q(LCACL+11)/2.0)) !Eta
C
          CALL UCOPY(XBAR3,Q(LCACL+14),3)   !x,y,z
          CALL UCOPY(DBAR3,Q(LCACL+20),3)   !dispersion dx,dy,dz
          CALL UCOPY(XBAR3,SHOWER_CENTER,3) !update shower center
        ENDIF
C
        IF ( .NOT.USE_LOG ) THEN
C
C ****  use centroid
C
          CALL UCOPY(Q(LCACL+14),SHOWER_CENTER,3)  !Centroid
          IF ( USE_POSITION_INFO ) THEN
C
C ****  use H matrix
C
            CALL UCOPY(PRED_CENTER,SHOWER_CENTER,3) !USE H MATRIX CENTER
          ENDIF
        ENDIF
C
        IF(LZTRKS) THEN
          IF (SELECTIVE_EC_TRACKING) THEN
            IF (ABS(IETA).GT.12) THEN
              IF (ETCLUS.LT.ET_ECTRK_THR) THEN
                ELECT = .FALSE.
                GOTO 777    !     do not call tracking if below thr
              ENDIF
              IF (CHISQ_CUTOFF.AND.(CHISQ.GT.CHISQ_ECTRK_THR)) 
     &            THEN
                ELECT = .FALSE.
                GOTO 779    !     do not call tracking if above chisq cut
              ENDIF
            ENDIF
          ENDIF
          IF (SELECTIVE_CC_TRACKING) THEN
            IF (ABS(IETA).LE.12) THEN
              IF (ETCLUS.LT.ET_CCTRK_THR) THEN
                ELECT = .FALSE.
                GOTO 777    !     do not call tracking if below thr
              ENDIF
              IF (CHISQ_CUTOFF.AND.(CHISQ.GT.CHISQ_CCTRK_THR)) 
     &            THEN
                ELECT = .FALSE.
                GOTO 779  !     do not call tracking if above chisq cut
              ENDIF
            ENDIF
          ENDIF
C
C ****   Work out road for tracking...
C
          PHI_CENTER = ATAN2(SHOWER_CENTER(2),SHOWER_CENTER(1))
          IF(PHI_CENTER.LT.0.0)PHI_CENTER = PHI_CENTER + TWOPI
          PHI_LO = PHI_CENTER - PHI_ROAD
          PHI_HI = PHI_CENTER + PHI_ROAD
C
          RHO = SQRT(SHOWER_CENTER(2)**2+SHOWER_CENTER(1)**2)
          DELZ = DZ(1)*THETA_ROAD_FACT
          THETA_LO = ATAN2(RHO,SHOWER_CENTER(3)-ZV(1)+DELZ)
          THETA_HI  = ATAN2(RHO,SHOWER_CENTER(3)-ZV(1)-DELZ)
C
C ****  Impose minimum road size...
C
          IF(ABS(THETA_HI-THETA_LO).LT. 2.*MIN_THETA_ROAD) THEN
            THETA_LO = Q(LCACL+11) - MIN_THETA_ROAD
            THETA_HI = Q(LCACL+11) + MIN_THETA_ROAD
          ENDIF
C
          PTDUM = Q(LCACL+8)   !  Et of shower
C
C          CALL ZTRAKS(ZV(1),PHI_LO,PHI_HI,THETA_LO,THETA_HI,PTDUM,
C     &   NTRAKS,ZLINK_TRAKS)
          CALL ZTRELC(ZV(1),PHI_LO,PHI_HI,THETA_LO,THETA_HI,PTDUM,
     &        NTRAKS,ZLINK_TRAKS)
C
C ****  Reserve links in ZLINKA
C
          DO I = 1,NTRAKS
            CALL GSLINK('CPHTRK',NTEMP(I))
            LSLINK(NTEMP(I)) = ZLINK_TRAKS(I)
            CALL ZTFLAG(ZLINK_TRAKS(I),'ELE')
          ENDDO
C
C ****  Will have to check on impact parameter here...
C
          DCLA = 999.
          NZTRAKS = NTRAKS
          LZTRAK_ELECTRON = 0
          IF(NTRAKS.NE.0) THEN
            DO I = 1,NTRAKS
              LZFIT = LQ(LSLINK(NTEMP(I))-IZZFIT)
              DIR_COS(1) = Q(LZFIT+20)
              DIR_COS(2) = Q(LZFIT+22)
              DIR_COS(3) = Q(LZFIT+24)
              TRK_CENT(1) = Q(LZFIT+11)
              TRK_CENT(2) = Q(LZFIT+12)
              TRK_CENT(3) = Q(LZFIT+15)
              CALL CLOSE_DIST(SHOWER_CENTER,TRK_CENT,DIR_COS,DCL)
C ****  Call TTRAKS for each ZTRK in road
              ILINKS(1) = LSLINK(NTEMP(I))
              ILINKS(2) = LCACL
              CALL TTRAKS(ILINKS)
C ****  Fill reference link to TRD in ZTRK bank
              IF(ILINKS(3) .NE. 0) THEN
                LQ(LSLINK(NTEMP(I))-9) = ILINKS(3)
              ENDIF
              IF (DCL .LT. DCLA) THEN
                DCLA = DCL
                ZWANT = I
                LQ(LCACL-5) = ILINKS(3)
              ENDIF
C
            ENDDO
            LZTRAK_ELECTRON = LSLINK(NTEMP(ZWANT))
C             IF(THETA_HI .GT. THMIN .AND. THETA_LO .LT. THMAX) THEN
C
C
C ****  Drop temporary ZTMP banks used by TRD
C
            LZTRH = GZZTRH()
            IF (LZTRH.GT.0) THEN
              IF (LQ(LZTRH - IZZTMP) .GT. 0)
     &          CALL MZDROP(IXCOM,LQ(LZTRH - IZZTMP),'L')
            ENDIF
  660       CONTINUE
            ELECT = .TRUE.
          ELSE
            ELECT = .FALSE.
C ****  Call TRD for PPHOs
            IF (DO_PPHO_IN_TRD) THEN
              ILINKS(1) = 0
              ILINKS(2) = LCACL
              CALL TTRAKS(ILINKS)
C ****  Fill reference link to TRD in CACL bank
              IF(ILINKS(3) .NE. 0) THEN
                LQ(LCACL-5) = ILINKS(3)
              ENDIF
C ****  Drop temporary ZTMP banks used by TRD
              LZTRH = GZZTRH()
              IF (LZTRH.GT.0) THEN
                IF (LQ(LZTRH - IZZTMP) .GT. 0)
     &           CALL MZDROP(IXCOM,LQ(LZTRH - IZZTMP),'L')
              ENDIF
            END IF
          ENDIF
  777     CONTINUE
C
C ****  !!!!!TEMPORARY!!!!! WILL EVENTUALLY LOOP OVER VERTICES CORRECTLY.
C ****  !!!!!THIS LOOP SIMPLY FLAGS TRACKS IN ROADS FORMED AROUND ALL VERTICES
C ****  !!!!!SO THEY WILL BE SAVED ON DST AND CAN BE STUDIED.
C

          DO J = 2,NVER
            IF(DZ(J).LT.DEL_ZVERTEX)DZ(J) = DEL_ZVERTEX   ! Minimum value
            DELZ = DZ(J)*THETA_ROAD_FACT
            THETA_NEW = ATAN2(RHO,SHOWER_CENTER(3)-ZV(J))
            THETA_NLO = ATAN2(RHO,SHOWER_CENTER(3)-ZV(J)+DELZ)
            THETA_NHI  = ATAN2(RHO,SHOWER_CENTER(3)-ZV(J)-DELZ)
            ETNEW = ECLUS *SIN(THETA_NEW)
            IF (SELECTIVE_CC_TRACKING) THEN
              IF (ABS(IETA).LE.12) THEN
                IF (ETNEW.LT.ET_CCTRK_THR) GOTO 778
              ENDIF
            ENDIF
            IF (SELECTIVE_EC_TRACKING) THEN
              IF (ABS(IETA).GT.12) THEN
                IF (ETNEW.LT.ET_ECTRK_THR) GOTO 778
              ENDIF
            ENDIF
            IF (SELECTIVE_VERTLOOP) THEN
              IF (ABS(IETA).GT.12) THEN
                IF (ETNEW.LT.ET_ECTRK_THR_VERTLOOP) GOTO 778
              ENDIF
            ENDIF
C
C ****  Impose minimum road size...
C
            IF(ABS(THETA_NHI-THETA_NLO).LT. 2.*MIN_THETA_ROAD) THEN
              THETA_NLO = THETA_NEW - MIN_THETA_ROAD
              THETA_NHI = THETA_NEW + MIN_THETA_ROAD
            ENDIF
C
C ****  Call ZTRAKS...
C
            CALL ZTRELC(ZV(J),PHI_LO,PHI_HI,THETA_NLO,THETA_NHI,PTDUM,
     &          NEWTRAKS,ZLINK_NEWTRAKS)
            DO II = 1,NEWTRAKS
              CALL ZTFLAG(ZLINK_NEWTRAKS(II),'ELE')            
              IF(.NOT.ELECT) THEN      ! pick closest match for PPHO bank
                NZTRAKS = NZTRAKS+1
                LZFIT = LQ(ZLINK_NEWTRAKS(II)-IZZFIT)
                DIR_COS(1) = Q(LZFIT+20)
                DIR_COS(2) = Q(LZFIT+22)
                DIR_COS(3) = Q(LZFIT+24)
                TRK_CENT(1) = Q(LZFIT+11)
                TRK_CENT(2) = Q(LZFIT+12)
                TRK_CENT(3) = Q(LZFIT+15)
                CALL CLOSE_DIST(SHOWER_CENTER,TRK_CENT,DIR_COS,DCL)
                ILINKS(1) = ZLINK_NEWTRAKS(II)
                ILINKS(2) = LCACL
                CALL TTRAKS(ILINKS)
C ****  Fill reference link to TRD in ZTRK bank
                IF(ILINKS(3) .NE. 0) THEN
                  LQ(ZLINK_NEWTRAKS(II)-9) = ILINKS(3)
                ENDIF
                IF (DCL .LT. DCLA) THEN
                  DCLA = DCL
                  LZTRAK_ELECTRON = ZLINK_NEWTRAKS(II)
C ****  Fill reference link to TRD in CACL banks
                  LQ(LCACL-5) = ILINKS(3)
                ENDIF
              ENDIF
            ENDDO
C
C ****  Drop temporary ZTMP banks
C
            LZTRH = GZZTRH()
            IF (LZTRH.GT.0) THEN
              IF (LQ(LZTRH - IZZTMP) .GT. 0)
     &          CALL MZDROP(IXCOM,LQ(LZTRH - IZZTMP),'L')
            ENDIF
  778       CONTINUE
          ENDDO
  779     CONTINUE
C
        ELSE
          ELECT = .TRUE.     ! DEFAULT FOR NO TRACKING IS ELECTRON
        ENDIF
C
C ****  FILL IN PELC/PPHO BANKS HERE
C
        CALL CMAKE_BANKS(ELECT,ECLUS,DCLA,NZTRAKS,ETRANS,PHI_LO,PHI_HI, 
     &      THETA_LO,THETA_HI,LEVEL)
C
C ****  Unreserve links in ZLINKA
C
        IF ( DO_HMATRIX_ANAL ) THEN
          CALL CAL_HMATRIX_ANAL(DO_NTUPLE)
        ENDIF
C
        DO I = 1,NTRAKS
          CALL RSLINK('CPHTRK',NTEMP(I))
        ENDDO
C
  888   CONTINUE
  300   CONTINUE                          ! GO TO NEXT CLUSTER
        LCACL = LQ(LCACL)
      ENDDO
C
C ****  electron/Photon finding done.
C
C ****  get electron/photon quality
C
      IF(DO_CLEANEM)THEN

        LCLUS = GZPELC()
        TRK   = 1
        ELECT = .TRUE.
        IF (LCLUS.EQ.0) THEN
          LCLUS = GZPPHO()
          TRK   = 0
          ELECT = .FALSE.
        ENDIF
        DO WHILE (LCLUS.NE.0)
          CALL CLEANEM(LCLUS,TRK,OK,STATUS)
          IQ(LCLUS+30) = STATUS
          IF (ELECT) THEN
            CALL CLEANEM_TQUANS(MVAR,TQUAN) 
            Q(LCLUS+20)  = TQUAN(12)          ! fill track-match significance
          ENDIF
          LCLUS = LQ(LCLUS)
          IF (LCLUS.EQ.0..AND.ELECT) THEN
            LCLUS = GZPPHO()
            TRK   = 0
            ELECT = .FALSE.
          ENDIF
        END DO

      ENDIF
C
C ****  CALL global fit package
C
      IF(DO_ELFIT)THEN
        LPELC = GZPELC()
        DO WHILE (LPELC.NE.0)
          CALL CELGLB(LPELC)
          LPELC = LQ(LPELC)
        END DO 
      ENDIF
C
C ****  Call Caphel analysis routine
C
  900 CALL CPHANL              ! CAPHEL ANALYSIS ROUTINE
C
  999 RETURN
      END
