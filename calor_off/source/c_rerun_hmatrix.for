      LOGICAL FUNCTION C_RERUN_HMATRIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs : ALWAYS RETURNS TRUE
C-   Controls:
C-
C-   Created  11-AUG-1992   Meenakshi Narain
C-   Updated  19-SEP-1992   Meenakshi Narain include refinding of centroid
C-   Updated  19-SEP-1992   Rajendran Raja  cleaned up links
C-   Updated  20-SEP-1992   Meenakshi Narain   readjust the lengths of
C-                               PPHO, PELC, HMTP and HMTE depending on
C-                               version number.
C-   Updated  12-OCT-1992   Meenakshi Narain  keep track of changes in CAPHEL
C-   Updated  12-OCT-1992   Meenakshi Narain  call tracking only for electrons
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
      INTEGER NDIM_ALL,NDIM_TRUNC
      INTEGER LEVEL,  GZPELC, GZPPHO
      INTEGER LCLUS, LHMTC
      INTEGER IER,I
      INTEGER NMORE_PPHO, NMORE_HMTP
      PARAMETER(NMORE_PPHO = 5)
      PARAMETER(NMORE_HMTP = 7)
C
      EQUIVALENCE (CSTLNK(LNKMX-1),LCLUS)
      EQUIVALENCE (CSTLNK(LNKMX),LHMTC)
C
      LOGICAL PHOTON,ELECT
      REAL    ECLUS,NEW_ECLUS,XTEST
      REAL    CHSQ_ALL,PROB_ALL,CHSQ_TRUNC,PROB_TRUNC
      REAL    PHI_CENTER,PHI_LO,PHI_HI,PHI_ROAD
      REAL    THETA_ROAD_FACT,THETA_LO,THETA_HI
      REAL ETAPHI(3),DETAPHI(3)
      REAL    RHO,DELZ
      REAL    TRK_CENT(3),DIR_COS(3),DCL
      LOGICAL ABORT_PROC, USE_CASH_INFO, FIRST, USE_LOG
      LOGICAL DO_HMATRIX_ANAL
      LOGICAL USE_MONTE_VERTEX
      INTEGER NTRAKS,ZLINK_TRAKS(100),NTEMP(100),ZWANT
      REAL    ZV(14),DZ(14),DEL_ZVERTEX
      REAL DCLA,NZTRAKS
      INTEGER GZISV1,NVER
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      C_RERUN_HMATRIX = .TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET_l('USE_CASH_INFO',USE_CASH_INFO,IER)
        IF (.NOT.USE_CASH_INFO) THEN
          USE_CASH_INFO = .TRUE.
          CALL EZSET('USE_CASH_INFO',USE_CASH_INFO,IER)
          CALL ERRMSG('C_RERUN_HMATRIX','RERUN_HMATRIX',
     &          'FORCING use of information from CASH BANKS ','W')
        ENDIF
        CALL INRCP('CAPHEL_RCP',IER)       ! read in RCP file
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('CAPHEL_RCP','C_RERUN_HMATRIX',
     &      ' ERROR READING CAPHEL RCP','W')
        ENDIF
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('WEIGHT_CUT',WEIGHT_CUT,IER)
        CALL EZGET_l('USE_LOG_WEIGHTED_CENTER',USE_LOG,IER)
        CALL EZGET_l('DO_HMATRIX_ANALYSIS',DO_HMATRIX_ANAL,IER)
        CALL EZRSET
        USE_MONTE_VERTEX = .FALSE.
        IF ( GZISV1().NE.0.0 ) THEN
          USE_MONTE_VERTEX = .TRUE.
          CALL ERRMSG('CALORIMETER','C_RERUN_HMATRIX',
     &      'MONTE CARLO VERTEX BEING USED','I')
        ENDIF
      END IF
C
      CALL FLGSET('WRITE_THIS_EVENT',.FALSE.)
C
      IF ( USE_MONTE_VERTEX ) THEN
        CALL ZVERTX(ZV,DZ)                ! Isajet Vertex
      ELSE
        CALL ZVERTE(NVER,ZV,DZ)                ! Vertex from tracking
        IF(NVER.EQ.0) THEN
          CALL ERRMSG('No Vertices','C_RERUN_HMATRIX','z set to 0','W')
          ZV(1)=0.0
        ENDIF
      ENDIF
C
      VERT(3) = ZV(1)
C
      LCLUS  = GZPELC()
      PHOTON = .FALSE.
      IF (LCLUS.EQ.0) THEN
        LCLUS  = GZPPHO()
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
c
          IF (LCASH.EQ.0) THEN
            CALL ERRMSG('CALORIMETER','C_RERUN_HMATRIX',
     &      'PELC/PPHO MISSING ASSOCIATED CELL ENERGY INFORMATION',
     &      'W')
            GOTO 888
          ENDIF
c
          Q(LCLUS+6) = ECLUS      ! fix energy
C
          DO I = 1, 3
            EM3AV(I) = Q(LCACL+13+I)    ! SHOWER CENTER
          END DO
C
C ****  recompute the shower centroid using log  weights
C
C
          CALL CM3POS(LCASH,WEIGHT_CUT,XBAR3,DBAR3,ETAPHI,DETAPHI)
C
C ****  get Hmatrix chisquared
C
          CALL CHMANL_NEW(ECLUS,NEW_ECLUS,ABORT_PROC,LEVEL)
C
C
C ****  IF HERE GOOD CHISQUARED
C
          CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
C
C
C ****  check which shower centroid to use and save it in CACL
C
          IF ( USE_POSITION_INFO ) THEN
C         H matrix centroid
            CALL UCOPY(PRED_CENTER,SHOWER_CENTER,3)
          ELSE
            CALL UCOPY(Q(LCACL+14),SHOWER_CENTER,3)  !Centroid
            XTEST = XBAR3(1)**2 + XBAR3(2)**2 + XBAR3(3)**2
            IF ( XTEST.GT.0.0 ) THEN
              CALL UCOPY(DBAR3,Q(LCACL+20),3) !STORE AWAY IN CACL
            ENDIF
          ENDIF
          IF ( USE_LOG ) THEN
C         Use log weighted center
            XTEST = XBAR3(1)**2 + XBAR3(2)**2 + XBAR3(3)**2
            IF ( XTEST.GT.0.0 ) THEN
              CALL UCOPY(XBAR3,SHOWER_CENTER,3)
              CALL UCOPY(DBAR3,Q(LCACL+20),3)
            ENDIF
          ENDIF
C
C ****  save the used shower center in appropriate banks
C
          IF (PHOTON) THEN
            IF (IQ(LCLUS+1).EQ.1) THEN               ! Bank version
C           extend the bank length
              IQ(LCLUS+1)=2
              CALL MZPUSH(IXCOM,LCLUS,0,NMORE_PPHO,' ')
            ENDIF
            CALL UCOPY(SHOWER_CENTER,Q(LCLUS+20),3)
          ELSE
            IF (IQ(LCLUS+1).EQ.1) THEN
              IQ(LCLUS+1)=2
            END IF
            CALL UCOPY(SHOWER_CENTER,Q(LCLUS+23),3)
          ENDIF
C
C ****  recompute the distance of closest approach to the tracks
C
C
C ****   Work out road for tracking...
C
          PHI_LO = Q(LHMTC+9)
          PHI_HI = Q(LHMTC+10)
          THETA_LO = Q(LHMTC+11)
          THETA_HI = Q(LHMTC+12)
          IF (.NOT. PHOTON) THEN
C
            CALL ZTRELC(ZV(1),PHI_LO,PHI_HI,THETA_LO,THETA_HI,ET,
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
                IF (DCL .LT. DCLA) THEN
                  DCLA = DCL
                  ZWANT = I
                ENDIF
              ENDDO
              LZTRAK_ELECTRON = LSLINK(NTEMP(ZWANT))
              ELECT = .TRUE.
            ELSE
              ELECT = .FALSE.
            ENDIF
            Q(LCLUS+22) = DCLA
C
            DO I = 1,NTRAKS
              CALL RSLINK('CPHTRK',NTEMP(I))
            ENDDO
          ENDIF
C
C ****  fill Hmatrix info in HMTP/HMTE banks
C
          IF (LHMTC.GT.0) THEN
            IF (PHOTON) THEN
              IF (IQ(LHMTC+1).LT.3) THEN               ! Bank version
C             extend the bank length
                CALL MZPUSH(IXCOM,LHMTC,0,NMORE_HMTP,' ')
              ENDIF
              CALL HMTPFL(LHMTC,LEVEL,PHI_LO,PHI_HI,THETA_LO,THETA_HI)
            ELSE
              CALL HMTEFL(LHMTC,LEVEL,PHI_LO,PHI_HI,THETA_LO,THETA_HI)
            END IF
          END IF
C
C ****  get the link to the next object
C
          IF ( DO_HMATRIX_ANAL ) THEN
            CALL C_SETUP_ETAC_PHIC
            CALL CEMENR_CASH(NDPTH,ENDPTH,PEDPTH,ETOT,ETRANS,EMAX)
            CALL CAL_HMATRIX_ANAL(.FALSE.)
            CALL ADD_W_QUANS(MORE_NAMES,MORE_QUANS,NUM_MORE)
            MORE_NAMES(1+NUM_MORE) = 'NTRAKS'
            MORE_QUANS(1+NUM_MORE) =  NTRAKS
            NUM_MORE = NUM_MORE + 1
            CALL HMATRIX_MAKE_NTUPLE(MORE_NAMES,MORE_QUANS,NUM_MORE)
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
          PHOTON = .TRUE.
        END IF
C
      END DO

  999 RETURN
      END
