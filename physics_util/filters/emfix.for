      LOGICAL FUNCTION EMFIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Package to "FIX" the shower centroid, direction,
C-                           and Hmatrix Chisq for PELC/PPHO versions <= 2
C-
C-   Returned value  : Always returns true
C-   Inputs  :         None
C-   Outputs :         None
C-   Controls:        HMATRIX.RCP
C-                    CAPHEL.RCP
C-                    ZTRAKS.RCP
C-
C-   Created  11-FEB-1993   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:CIMPACT.INC'
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
      INCLUDE 'D0$INC:MORE_NTUPLE.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IER,I
      INTEGER LEVEL,  GZPELC, GZPPHO
      INTEGER LCLUS, LHMTC
      INTEGER NMORE_PPHO, NMORE_HMTP
      PARAMETER(NMORE_PPHO = 5)
      PARAMETER(NMORE_HMTP = 7)
C
      EQUIVALENCE (CSTLNK(LNKMX-1),LCLUS)
      EQUIVALENCE (CSTLNK(LNKMX),LHMTC)
C
      REAL    ECLUS,NEW_ECLUS,XTEST
      REAL    PHI_CENTER,PHI_LO,PHI_HI,PHI_ROAD
      REAL    THETA_ROAD_FACT,THETA_LO,THETA_HI
      REAL    ETAPHI(3),DETAPHI(3),RHO,DELZ
      REAL    TRK_CENT(3),DIR_COS(3),DCL,DCLA,NZTRAKS
      REAL    ZV(14),DZ(14),DEL_ZVERTEX
      REAL    SHOWER_DIR(3)
      REAL    CLUS_THETA, CLUS_ETA,CLUS_PHI
      INTEGER GZHSTR
      INTEGER GZISV1,NVER,LRCP
      INTEGER NTRAKS,ZLINK_TRAKS(100),NTEMP(100),ZWANT
      LOGICAL PHOTON,ELEC,OK
      LOGICAL USE_MONTE_VERTEX
      LOGICAL FIRST,ABORT_PROC
      LOGICAL USE_CASH_INFO,USE_LOG,DO_HMATRIX
      LOGICAL MKZFIT,VTXON,CDCON,FDCON,TRDON,TURN_OFF
      LOGICAL SKIP_HMATRIX
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****
C
      EMFIX = .TRUE.
C
      IF (FIRST) THEN
        FIRST = .FALSE.
C
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET_l('USE_CASH_INFO',USE_CASH_INFO,IER)
        IF (.NOT.USE_CASH_INFO) THEN
          USE_CASH_INFO = .TRUE.
          CALL EZSET('USE_CASH_INFO',USE_CASH_INFO,IER)
          CALL ERRMSG('EMFIX','RERUN_HMATRIX',
     &          'FORCING use of information from CASH BANKS ','W')
        ENDIF
        CALL EZRSET
C
        CALL EZLOC('CAPHEL_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP('CAPHEL_RCP',IER)       ! read in RCP file
          IF ( IER.NE.0 ) THEN
            CALL ERRMSG('CAPHEL_RCP','EMFIX',
     &        ' ERROR READING CAPHEL RCP','W')
          ENDIF
          CALL EZPICK('CAPHEL_RCP')
          CALL EZGET('WEIGHT_CUT',WEIGHT_CUT,IER)
          CALL EZGET_l('USE_LOG_WEIGHTED_CENTER',USE_LOG,IER)
          CALL EZGET_l('DO_HMATRIX',DO_HMATRIX,IER)
          CALL EZRSET
        ENDIF
C
        CALL EZLOC('ZTRAKS_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP('ZTRAKS_RCP',IER)
          IF (IER.EQ.0) CALL EZPICK('ZTRAKS_RCP')
          IF (IER.EQ.0) CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('EMFIX','INI',
     &        ' ZTRAKS_RCP not found','F')
          ENDIF
          CALL EZRSET
        ENDIF
C
C ****  CHECK ON D0RECO VERSIONS, correct ONLY for D0RECO production
C ****  VERSIONS below 11
C
        LHSTR = GZHSTR()
        IF (IQ(LHSTR+3).GE.11) THEN
          GOTO 999
        ENDIF
C
        USE_MONTE_VERTEX = .FALSE.
        IF ( GZISV1().NE.0.0 ) THEN
          USE_MONTE_VERTEX = .TRUE.
          CALL ERRMSG('CALORIMETER','EMFIX',
     &      'MONTE CARLO VERTEX BEING USED','I')
        ENDIF
      END IF
C
C ****  get vertex information
C
      IF ( USE_MONTE_VERTEX ) THEN
        CALL ZVERTX(ZV,DZ)                ! Isajet Vertex
      ELSE
        CALL ZVERTE(NVER,ZV,DZ)                ! Vertex from tracking
        IF(NVER.EQ.0) THEN
          CALL ERRMSG('No Vertices','EMFIX','z set to 0','W')
          ZV(1)=0.0
        ENDIF
      ENDIF
      VERT(3) = ZV(1)
C
C ****  Loop over all electrons/photons banks in the event
C
      LCLUS  = GZPELC()
      ELEC   = .TRUE.
      PHOTON = .FALSE.
      IF (LCLUS.EQ.0) THEN
        LCLUS  = GZPPHO()
        ELEC   = .FALSE.
        PHOTON = .TRUE.
      END IF

      DO WHILE(LCLUS .NE. 0)
C
        LCACL = LQ(LCLUS-2)  !REFERENCE LINK
        IF ( LCACL.GT.0 ) THEN
          ECLUS = Q(LCACL+7)
          ET = Q(LCACL+8)
          LHMTC = LQ(LCLUS-1)
          LCASH = LQ(LCACL-2)
          DO I = 1, 3
            EM3AV(I) = 0.
          END DO
C
          CALL CEMENR(NDPTH,ENDPTH,PEDPTH,ETOT,ET,ETRANS,EMAX,
     &      ETAMX,PHIMX,USE_CASH_INFO)

          IF(USE_LOG)
     &      CALL CM3POS_FIX(LCASH,WEIGHT_CUT,XBAR3,DBAR3,ETAPHI,DETAPHI)
C
C ****  save the "current" shower centroid information
C
C       start with using the information already in CACL
C
          CALL UCOPY(Q(LCACL+11),SHOWER_DIR,3)      ! Shower direction
          CALL UCOPY(Q(LCACL+14),SHOWER_CENTER,3)   ! ShoweR centroid
C
C ****  Setup EM3AV array... need it if running from DST
C
          CALL UCOPY(Q(LCACL+14),EM3AV,3)
C
C ****  H matrix analysis...
C
          IF(DO_HMATRIX) THEN
            SKIP_HMATRIX = .FALSE.
            LHSTR = GZHSTR()
            IF (IQ(LHSTR+3).EQ.10) THEN
              IF (IQ(LHSTR+4).GE.12) THEN
                SKIP_HMATRIX = .TRUE.
              ENDIF
            ENDIF
            IF (.NOT.SKIP_HMATRIX) THEN
              CALL CHMANL_NEW(ECLUS,NEW_ECLUS,ABORT_PROC,LEVEL)
            ENDIF
          ENDIF
C
C ****  use Log weighted shower centroid as default
C ****  check if log weighting has been successfull, since it only
C ****  works in the EM3 plane. It will fail if cluster has
C ****  NO EM3 energy !
C
          CLUS_THETA  =  0.
          CLUS_PHI    =  0.
          CLUS_ETA    =  0.
          XTEST = XBAR3(1)**2 + XBAR3(2)**2 + XBAR3(3)**2
          IF ( XTEST.GT.0.0 ) THEN
            RHO = SQRT(XBAR3(2)**2+XBAR3(1)**2)
            CLUS_THETA  =  ATAN2(RHO,XBAR3(3)-ZV(1))
            CLUS_PHI    =  ETAPHI(2)
            CLUS_ETA    = -ALOG(TAN(Q(LCACL+11)/2.0))
C
            Q(LCACL+11) =  CLUS_THETA   !Theta
            Q(LCACL+12) =  CLUS_PHI     !Phi
            Q(LCACL+13) =  CLUS_ETA     !Eta
            CALL UCOPY(Q(LCACL+11),SHOWER_DIR,3)      ! update shower direction
          ENDIF
C
C ****  Update PELC/PPHO with recalculated shower centroid information
C
          IF (PHOTON) THEN
            IF (IQ(LCLUS+1).EQ.1) THEN               ! Bank version
C           extend the bank length
              IQ(LCLUS+1)=2
              CALL MZPUSH(IXCOM,LCLUS,0,NMORE_PPHO,' ')
              IF ( XTEST.GT.0.0 ) THEN
                CALL UCOPY(XBAR3,Q(LCACL+14),3)   !x,y,z
                CALL UCOPY(DBAR3,Q(LCACL+20),3)   !dispersion dx,dy,dz
                CALL UCOPY(XBAR3,SHOWER_CENTER,3) !update shower center
              ENDIF
              CALL UCOPY(SHOWER_CENTER,Q(LCLUS+20),3)
              Q(LCLUS+19) = ETAC  ! TOWER WITH MAXIMUM ENERGY IN THE CLUSTER
            ELSE
C
C ****  fill detector eta for photons
C
              IF (Q(LCLUS+19).EQ.0) THEN
                CALL UCOPY(SHOWER_CENTER,Q(LCLUS+20),3)
                Q(LCLUS+19) = ETAC  ! TOWER WITH MAXIMUM ENERGY IN THE CLUSTER
              ENDIF
            ENDIF
C
C ****  Fill shower center and direction
C
            Q(LCLUS+8)  = SHOWER_DIR(1)   !  Theta
            Q(LCLUS+9)  = SHOWER_DIR(3)   !  Eta
            Q(LCLUS+10) = SHOWER_DIR(2)   !  Phi
C
          ELSE
C
C **** Electrons
C
            IF (IQ(LCLUS+1).EQ.1) THEN
              IQ(LCLUS+1)=2
              IF ( XTEST.GT.0.0 ) THEN
                CALL UCOPY(XBAR3,Q(LCACL+14),3)   !x,y,z
                CALL UCOPY(DBAR3,Q(LCACL+20),3)   !dispersion dx,dy,dz
                CALL UCOPY(XBAR3,SHOWER_CENTER,3) !update shower center
              ENDIF
              Q(LCLUS+19) = ETAC    ! TOWER WITH MAXIMUM ENERGY IN THE CLUSTER
              CALL UCOPY(SHOWER_CENTER,Q(LCLUS+23),3)
            END IF
C
C ****  Fill shower center and direction
C
            Q(LCLUS+8)  = SHOWER_DIR(1)   !  Theta
            Q(LCLUS+9)  = SHOWER_DIR(3)   !  Eta
            Q(LCLUS+10) = SHOWER_DIR(2)   !  Phi
C
          ENDIF
C
C ****  Recompute the distance of closest approach since the
C ****  shower centroid info has changed
C
          IF (ELEC) THEN
C
C       SET ZTRAK PARAMETERS to run from DST, ie use only the tracks in
C       the road as determined when the event was originally reconstructed.
C
            CALL EZPICK('ZTRAKS_RCP')
            CALL EZGET_l('MKZFIT',MKZFIT,IER)
            CALL EZGET_l('VTXON',VTXON,IER)
            CALL EZGET_l('CDCON',CDCON,IER)
            CALL EZGET_l('FDCON',FDCON,IER)
            CALL EZGET_l('TRDON',TRDON,IER)
            TURN_OFF = .FALSE.
            CALL EZSET('MKZFIT',TURN_OFF,IER)
            CALL EZSET('VTXON',TURN_OFF,IER)
            CALL EZSET('CDCON',TURN_OFF,IER)
            CALL EZSET('FDCON',TURN_OFF,IER)
            CALL EZSET('TRDON',TURN_OFF,IER)
            CALL EZRSET
C
C ****  setup road for tracking
C
            PHI_LO = Q(LHMTC+9)
            PHI_HI = Q(LHMTC+10)
            THETA_LO = Q(LHMTC+11)
            THETA_HI = Q(LHMTC+12)
C
            LZTRK = LQ(LCLUS-3)             ! Link to associated ZTRAK bank
C
C       get info for all tracks in road
C
            CALL ZTRELC(ZV(1),PHI_LO,PHI_HI,THETA_LO,THETA_HI,ET,
     &        NTRAKS,ZLINK_TRAKS)
            DO I = 1,NTRAKS
              CALL GSLINK('CPHTRK',NTEMP(I))
              LSLINK(NTEMP(I)) = ZLINK_TRAKS(I)
              CALL ZTFLAG(ZLINK_TRAKS(I),'ELE')
            ENDDO
C
C ****  recompute the impact parameter
C
            DCLA = 999.
            NZTRAKS = NTRAKS
            LZTRAK_ELECTRON = 0
            DO I = 1,NTRAKS
              LZFIT = LQ(LSLINK(NTEMP(I))-IZZFIT)
              DIR_COS(1) = Q(LZFIT+20)
              DIR_COS(2) = Q(LZFIT+22)
              DIR_COS(3) = Q(LZFIT+24)
              TRK_CENT(1) = Q(LZFIT+11)
              TRK_CENT(2) = Q(LZFIT+12)
              TRK_CENT(3) = Q(LZFIT+15)
              CALL CLOSE_DIST(SHOWER_CENTER,TRK_CENT,DIR_COS,DCL)
              IF (DCL .LT. DCLA) THEN
                DCLA = DCL
                ZWANT = I
              ENDIF
            ENDDO
            LZTRAK_ELECTRON = LSLINK(NTEMP(ZWANT))
C
C ****  update DCLA and the link to close Ztrak info in PELC
C
            Q(LCLUS+22) = DCLA
C
C ****  RESET link to ZTRK if another track in the road in now the closest one
C
            IF (LZTRK.NE.LZTRAK_ELECTRON) THEN
              LQ(LCLUS-3) = LZTRAK_ELECTRON
            ENDIF
C
            DO I = 1,NTRAKS
              CALL RSLINK('CPHTRK',NTEMP(I))
            ENDDO
C
            CALL EZPICK('ZTRAKS_RCP')
            CALL EZGET_l('MKZFIT',MKZFIT,IER)
            CALL EZGET_l('VTXON',VTXON,IER)
            CALL EZGET_l('CDCON',CDCON,IER)
            CALL EZGET_l('FDCON',FDCON,IER)
            CALL EZGET_l('TRDON',TRDON,IER)
            CALL EZRSET
          ENDIF
C
C ****  fill Hmatrix info in HMTP/HMTE banks
C
          IF (.NOT.ABORT_PROC) THEN
            IF (.NOT.SKIP_HMATRIX) THEN
              IF (LHMTC.GT.0) THEN
                IF (PHOTON) THEN
                  IF (IQ(LHMTC+1).LT.3) THEN               ! Bank version
C             extend the bank length
                    CALL MZPUSH(IXCOM,LHMTC,0,NMORE_HMTP,' ')
                  ENDIF
                  CALL HMTPFL(LHMTC,LEVEL,PHI_LO,PHI_HI,THETA_LO,
     &              THETA_HI)
                ELSE
                  CALL HMTEFL(LHMTC,LEVEL,PHI_LO,PHI_HI,THETA_LO,
     &              THETA_HI)
                END IF
              END IF
            ENDIF
          ENDIF
C
        ELSE
          CALL ERRMSG('CALORIMETER','C_RERUN_HMATRIX',
     &      'PELC/PPHO BANK HAS NO REF LINK TO CLUSTER ','W')
        ENDIF
C
  888   CONTINUE
        LCLUS = LQ(LCLUS)
        IF (LCLUS.EQ.0 .AND. .NOT. PHOTON) THEN
          LCLUS  = GZPPHO()
          ELEC   = .FALSE.
          PHOTON = .TRUE.
        END IF
C
      END DO
C
  999 RETURN
      END
