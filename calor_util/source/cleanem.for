      SUBROUTINE CLEANEM(LCLUS,TRK,OK,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Imposes quality cuts on EM (electron or
C-                         photon) candidates.
C-
C-   Inputs  :  LCLUS  (I) : either LPELC (link to PELC bank) or
C-                             LPPHO (link to PPHO bank)
C-              TRK    (I) : 0 do not analyze tracking information
C-                           1 analyze tracking information
C-
C-   Outputs :  OK     (L) :  .TRUE. if passed all USER cuts
C-                            ALWAYS TRUE if no user cuts specified
C-              STATUS (I) :  bits (0/1)indicating which cuts passed/failed
C-
C-              STATUS BIT   0 : coarse Hmatrix chisquared
C-              STATUS BIT   1 : fine H matrix chisquared
C-              STATUS BIT   2 : CC em flag
C-              STATUS BIT   3 : cluster EM fraction
C-              STATUS BIT   4 : core energy cut
C-              STATUS BIT   5 : transverse dispersion cut
C-              STATUS BIT   6 : sigma5-sigma3
C-              STATUS BIT   7 : Fractional Isolation cut Energy (Cone 1)
C-              STATUS BIT   8 : Fractional Isolation cut Energy (Cone 2)
C-              STATUS BIT   9 : Fractional Isolation cut ET (Cone 1)
C-              STATUS BIT  10 : Fractional Isolation cut ET (Cone 2)
C-              STATUS BIT  11 : Close to crack flag
C-              STATUS BIT  12 : Number of cells below minimum
C-              STATUS BIT  13 : L2/RECO match
C-              STATUS BIT  14 : Isolation cut ET (Cone 2)
C-              STATUS BIT  15 : Spare
C-              STATUS BIT  16 : Distance 1 cut (Rdeltaphi for CC and EC)
C-              STATUS BIT  17 : Distance 2 cut (Delta Z for CC delta R for EC
C-              STATUS BIT  18 : Shower centroid/track match significance
C-              STATUS BIT  19 : Set if another track in the road passes
C-                               track match significance criterion
C-              STATUS BIT  20 : number of tracks in a cone of dR
C-              STATUS BIT  21 : Hits-in-road info available (=1 if problem)
C-              STATUS BIT  22 : CDC ionization (MIP)
C-              STATUS BIT  23 : FDC ionization (MIP)
C-              STATUS BIT  24 : VTX chamber ionization (MIP)
C-              STATUS BIT  25 : TRD information available (=1 if problem)
C-              STATUS BIT  26 : TRD truncated mean cut
C-              STATUS BIT  27 : TRD-ANALYSIS efficiency cut (specified
C-                               by TRD_EFFICIENCY_CUT parameter in CLEANEM.RCP
C-              STATUS BIT  28 : Vertex Transverse impact parameter (in x,y)
C-              STATUS BIT  29 : Vertex Z impact parameter (in Z)
C-              STATUS BIT  30 : Hits-in-road cut
C-              STATUS BIT  31 : isolation fraction cut for missing Et correction
C-
C-   Controls: CLEANEM_RCP, CAPHEL.RCP, TRD.RCP, TRD_ANALYSIS.RCP
C-
C-   Created  27-AUG-1992   Norman A. Graf
C-   Updated  24-OCT-1992   Meenakshi Narain
C-                          1) get input cutvariables from CAPHEL_RCP
C-                          2) implement eta dependent Hmatrix cuts
C-                          3) add sigma5-sigma3 cut
C-   Updated  27-OCT-1992   Meenakshi Narain
C-                          check on the input usermask to compute OK flag
C-   Updated  29-OCT-1992   Meenakshi Narain
C-                          back to using input cut variables from CLEANEM_RCP
C-   Updated   5-DEC-1992   Meenakshi Narain   Major Revisions
C-   Updated  17-DEC-1992   Meenakshi Narain  compute isolation variables
C-                          using CACL instead
C-   Updated  31-MAY-1993   Meenakshi Narain  add calls to DTRAKS and VTRAKS RCP
C-    Updated  24-JUN-1993   Meenakshi Narain  Add cut on ET outside the
C-                                  cluster in an isolation cone of 0.7.
C-                                   The corresponding status bit is 14
C-   Updated  25-JUN-1993   Meenakshi Narain
C-                          FIX PROBLEM with Crack flag computation
C-   Updated   6-JUL-1993   Meenakshi Narain  track match significance is now
C-                          sensitive to RECO versions and MC. ie.
C-                          1) EC theta-offsets are set to zero for MC, also
C-                             RCP controled by TURNOFF_EC_OFFSETS
C-                          2) for reco versions below V11.0 use misalignment
C-                             corrections for CC
C-   Updated  31-JAN-1994   Ian Adam  Check for UDST, take DISPERSION and
C-                          EMFRAC from the UDST bank if it exists.
C-   Updated   2-FEB-1994   Meenakshi Narain
C-                          Now calls TRD analysis package
C-                          NEED : TRD_ANALYSIS.RCP and TRD.RCP
C-                          Add BIT 27 for TRD analysis
C-   Updated  24-FEB-1994   Meenakshi Narain  protect LCLUS by copying it
C-                          to LCLEM in ZLINKC and update LCLEM if LCLEM
C-                          has changed due to Zebra garbage collection
C-   Updated  16-MAR-1994   Meenakshi Narain  :
C-                          call TRD ANALYSIS only if not done in RECO
C-                          ie (q(ltrdt+30)=0)
C-   Updated  17-MAR-1994   Meenakshi Narain  ADD DO_TRD_ANALYSIS switch
C-   Updated  25-AUG-1994   Gregory L. Landsberg  ADD hits in road info
C-   Updated  28-SEP-1994   Meenakshi Narain
C-                              Add options to use cluster zbias correction
C-                              and cdc z bias corrections
C-   Updated  11-SEP-1995   sss - fixed dimension of qnames, quans
C-   Updated   2-APR-1996   Meenakshi Narain  Call trd_analysis with 
C-                          pointer to PELC/PPHO  instead of LTRDT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CHMATR.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER I,N
      INTEGER LCLUS,LHMTR,TRK,NETA
      INTEGER LDTRK,LFDCT,LVTXT,LTRDT,LDTRH,GZDTRH
      INTEGER VERSION, USERMASK, IOFF, UDST_VERSION
      INTEGER RECO_VERSION, RECO_PASS, RECORD_TYPE, GZHSTR
      INTEGER NV
      PARAMETER( NETA = 37 )
      INTEGER STATUS
      LOGICAL OK,MCDATA,OKZ,CDC_DONE
      REAL THETA_ROAD_FACT,MIN_THETA_ROAD,DEL_ZVERTEX
C
      INTEGER N_PHOTON_VARS
      PARAMETER (N_PHOTON_VARS = 20)

      INTEGER NCVAR,NTVAR,NTVAR_CLPHOT
      REAL    CQUAN(50),TQUAN(50),QUANS(N_PHOTON_VARS)
      CHARACTER*8 CQUAN_NAMES(50),TQUAN_NAMES(50),QNAMES(N_PHOTON_VARS)
C
      INTEGER NCELLS,NCELLS_MIN,ETA_DET
      INTEGER NCELL_EMIN,POINTER,PACKED_WORD
      INTEGER CAL_ETA
      INTEGER IETA,IPHI1,IPHI2,ILYRL,ILYRR
      REAL    ZV,THETA,ETAD,RHO
      REAL    ZVTX_INFO(3, 1),DZ,DELZ
      REAL    ETAPHI(3),DETAPHI(3),APHI
      REAL    CORECLUS,ECLUS,ETCLUS
      REAL    EMFRAC,EMFRAC_CUT
      REAL    DISPERSION,DISPERSION_CUT
      REAL    SIGMA5,SIGMA3,DSIGMA,DSIGMA_CUT
      REAL    XBAR3(3),DBAR3(3),WEIGHT_CUT
      REAL    PHI_CLUS,THETA_CLUS,ETA_CLUS,ZCLUS,ZDIST
      REAL    CORECLUS_CUT,DIFF_PHI
      REAL    CHISQ,COARSE_CHISQUARED(NETA),FINE_CHISQUARED(NETA)
      REAL    ISOL_EN(2),ISOL_ET(2)
      REAL    ISOL_ECUT(4),ISOL_ETCUT(4)
      REAL    FISOL_EN(2),FISOL_ET(2)
      REAL    FISOL_ECUT(4),FISOL_ETCUT(4),ISOL_CONE_SIZE(2)
      REAL    CC_CRACK_WIDTH,D_CRACK,EM_ARC,CORE_ET
      REAL    X_CRACK1,X_CRACK2,Y_CRACK1,Y_CRACK2,X_CRACK,Y_CRACK,ZDUM
      REAL    ENERGY,CELL_EMIN
C
      INTEGER NZTRAKS,NZTRAKS_CUT,NCONE_TRK,ITRK
      INTEGER NTRAKS,ZLINK_TRAKS(zmax),NTEMP(zmax)
      INTEGER NTRK_TRD, NTRK_LYR1, NTRK_LYR2, NTRK_LYR3
      REAL    ERR_DIST_CC(2),ERR_DIST_EC(4)
      REAL    OFFSET_DIST_CC(2),OFFSET_DIST_EC(4)
      REAL    DIST_TRKCUT_CC(2),DIST_TRKCUT_EC(2)
      REAL    DIST_CLUSTRK(4), DCLUSTRK(2)
      REAL    SIG_TRKMATCH_CUT,SIG_TRKMATCH(zmax)
      REAL    TRACK_CONE_SIZE,DTRK_ETAPHI(zmax)
      REAL    TRK_IMPACT_XY,TRK_IMPACT_Z
      REAL    TRK_IMPACT_XY_CUT(2),TRK_IMPACT_Z_CUT
      REAL    PHI_LO,PHI_HI,THETA_LO,THETA_HI
      REAL    PHI_TRK(zmax),THETA_TRK(zmax),DIFFPHI,DIFFTHETA
      REAL    XCOG_TRK(zmax),YCOG_TRK(zmax),ZCOG_TRK(zmax),TRKPAR(5)
      REAL    ETOT_LIKE,ETOT_LEFF,ETOT_LEFF_CUT
      REAL    CDCMIP,FDCMIP,VTXMIP
      REAL    CDC_MIP_CUT(2),FDC_MIP_CUT(2),VTX_MIP_CUT(2)
      REAL    TRD_TRUNCATED_MEAN, TRD_TRUNCATED_MEAN_CUT
      REAL    CC_ALIGN_OFF(3)
      REAL    ERRFACT_ZCOR,ERRFACT_CDC_ZCOR,ERRFACT_CLUS_ZCOR
      LOGICAL NOTRKINFO,LMIP,TRD_INFO
      LOGICAL CC_MISALIGN,CORR_CC_MISALIGN
      LOGICAL DO_CC_CLUS_ZCOR, DO_CDC_ZCOR
      LOGICAL CORR_EC_OFFSET,TURNOFF_EC_OFFSET
      LOGICAL TRD_ACCEPTANCE, TRDINI, TRD_ANALYSIS_INI,DO_TRD_ANALYSIS
      LOGICAL TRD_FROM_UDST
      REAL    TRD_EFFICIENCY,TRD_EFFICIENCY_CUT
      REAL    FRAC_ISOL, MISSING_ET_FISOL_CUT
      REAL    CDC_ZCOR_SLOPE
      REAL    SPREAD_VAR(27)
C
      INTEGER MASK, LL2EM,LRCP,CACL_VERSION
      REAL    L1THR,L2THR,ET_L1,ET_L2
      LOGICAL LEVEL2_MATCH, L2_FOUND, CMATCH_TRIGELEC
      INTEGER LUDST,GZUDST
      REAL    UDST_VALUE,TRD_ACC
      INTEGER ITRUST
      INTEGER NZTRAKS_PPHO,LZTRK_PPHO,VERTEX_ID
C
C&IF VAXVMS
      EXTERNAL LIB$FIXUP_FLT
C&ENDIF
C
      INTEGER IER
      CHARACTER*7 MSG
      CHARACTER*4 BANK
      CHARACTER*40 PROGRAM_NAME
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
C----------------------------------------------------------------------
C
C
C ****  protect LCLUS
C
      LCLEM = LCLUS
C

      IF (FIRST) THEN
        FIRST = .FALSE.
C
C ****  Protect against Floating underflow errors in Chisq
C ****  neceessary as data reconstructed on SGI allows soft underflows
C ****  and is illegal in VMS
C
C&IF VAXVMS
        CALL SYS$SETEXV(%VAL(1),LIB$FIXUP_FLT,,)
C&ENDIF
C
C ****  Read in Cleanem and Ztraks RCP file, if not yet read
C
        CALL EZLOC('CLEANEM_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP('CLEANEM_RCP',IER)
          IF (IER.EQ.0) CALL EZPICK('CLEANEM_RCP')
          IF (IER.EQ.0) CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CLEANEM','CLEANEM',
     &        ' CLEANEM_RCP not found','F')
          ENDIF
          CALL EZRSET
        ENDIF
        CALL EZLOC('CAPHEL_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP('CAPHEL_RCP',IER)
          IF (IER.EQ.0) CALL EZPICK('CAPHEL_RCP')
          IF (IER.EQ.0) CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CLEANEM','CLEANEM',
     &        ' CAPHEL_RCP not found','F')
          ENDIF
          CALL EZRSET
        ENDIF

        CALL EZPICK('CAPHEL_RCP')
        CALL EZERR(IER)
        IF (IER.EQ.0) THEN
          CALL EZGET('THETA_ROAD_FACT',THETA_ROAD_FACT,IER)
          CALL EZGET('MIN_THETA_ROAD',MIN_THETA_ROAD,IER)
          CALL EZGET('MIN_VERTEX_Z_ERROR',DEL_ZVERTEX,IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG(' NO_CAPHEL_RCP','CLEANEM',
     &        ' NO RCP file to work with ','F')
        ENDIF
C
C ****  Read in RCP parameters
C
        CALL EZPICK('CLEANEM_RCP')
        CALL EZERR(IER)
        IF (IER.EQ.0) THEN
C
          IF (IER.EQ.0) CALL EZGETA('COARSE_CHISQUARED',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('COARSE_CHISQUARED',1,N,1,
     &      COARSE_CHISQUARED,IER)
          IF (IER.EQ.0) CALL EZGETA('FINE_CHISQUARED',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('FINE_CHISQUARED',1,N,1,
     &      FINE_CHISQUARED,IER)
          IF (IER.EQ.0) CALL EZGET('CLUSTER_EM_RATIO_THRESHOLD',
     &      EMFRAC_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('CORE_ENERGY_FRACTION_CUT',
     &      CORECLUS_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('DISPERSION_CUT',DISPERSION_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('DSIGMA_CUT',DSIGMA_CUT,IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONES',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONES',1,N,1,
     &      ISOL_CONE_SIZE,IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONE_ENERGY_CUT',0,0,0,N,
     &      IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONE_ENERGY_CUT',1,N,1,
     &      FISOL_ECUT,IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONE_ET_CUT',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONE_ET_CUT',1,N,1,
     &      FISOL_ETCUT,IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONE_TOT_ENERGY_CUT',0,0,
     &      0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONE_TOT_ENERGY_CUT',1,N,
     &      1, ISOL_ECUT,IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONE_TOT_ET_CUT',0,0,0,N,
     &      IER)
          IF (IER.EQ.0) CALL EZGETA('ISOLATION_CONE_TOT_ET_CUT',1,N,1,
     &      ISOL_ETCUT,IER)
          IF (IER.EQ.0) CALL EZGET('CC_CRACK_WIDTH',CC_CRACK_WIDTH,IER)
          IF (IER.EQ.0) CALL EZGET('MINIMUM_NUMBER_OF_CELLS',NCELLS_MIN,
     &      IER)
          IF (IER.EQ.0) CALL EZGET('CELL_ENERGY_THRESHOLD',CELL_EMIN,
     &      IER)
          IF (IER.EQ.0) CALL EZGET('MATCH_LEVEL2_ELECTRON',LEVEL2_MATCH,
     &      IER)
          IF (IER.EQ.0) CALL EZGET('L1ET_THRESHOLD',L1THR,IER)
          IF (IER.EQ.0) CALL EZGET('L2ET_THRESHOLD',L2THR,IER)
          IF (IER.EQ.0) CALL EZGET('WEIGHT_CUT',WEIGHT_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('NZTRAKS_CUT',NZTRAKS_CUT,IER)
          IF (IER.EQ.0) CALL EZGETA('CDC_MIP_CUT',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('CDC_MIP_CUT',1,N,1,CDC_MIP_CUT,IER)
          IF (IER.EQ.0) CALL EZGETA('FDC_MIP_CUT',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('FDC_MIP_CUT',1,N,1,FDC_MIP_CUT,IER)
          IF (IER.EQ.0) CALL EZGETA('VTX_MIP_CUT',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('VTX_MIP_CUT',1,N,1,VTX_MIP_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('CC_ALIGN_CORR',CC_MISALIGN,IER)
          IF (IER.EQ.0) CALL EZGETA('CC_ALIGN_OFF',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('CC_ALIGN_OFF',1,N,1,CC_ALIGN_OFF,
     &      IER)
          IF (IER.EQ.0) CALL EZGETA('DISTANCE_CC_CUT',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('DISTANCE_CC_CUT',1,N,1,
     &      DIST_TRKCUT_CC,IER)
          IF (IER.EQ.0) CALL EZGETA('DISTANCE_EC_CUT',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('DISTANCE_EC_CUT',1,N,1,
     &      DIST_TRKCUT_EC,IER)
          IF (IER.EQ.0) CALL EZGETA('ERR_DISTANCE_CC',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('ERR_DISTANCE_CC',1,N,1,ERR_DIST_CC,
     &      IER)
          IF (IER.EQ.0) CALL EZGETA('ERR_DISTANCE_EC',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('ERR_DISTANCE_EC',1,N,1,ERR_DIST_EC,
     &      IER)
          IF (IER.EQ.0) CALL EZGETA('OFFSET_DISTANCE_CC',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('OFFSET_DISTANCE_CC',1,N,1,
     &      OFFSET_DIST_CC,IER)
          IF (IER.EQ.0) CALL EZGET('TURNOFF_EC_OFFSET',
     &      TURNOFF_EC_OFFSET,IER)
          IF (IER.EQ.0) CALL EZGETA('OFFSET_DISTANCE_EC',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('OFFSET_DISTANCE_EC',1,N,1,
     &      OFFSET_DIST_EC,IER)
          IF (IER.EQ.0) CALL EZGET('SIG_TRKMATCH_CUT',SIG_TRKMATCH_CUT,
     &      IER)
          IF (IER.EQ.0) CALL EZGET('TRACK_CONE_SIZE',TRACK_CONE_SIZE,
     &      IER)
          IF (IER.EQ.0) CALL EZGET('ETOT_LEFF_CUT',ETOT_LEFF_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('TRD_TRUNCATED_MEAN_CUT',
     &      TRD_TRUNCATED_MEAN_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('TRD_EFFICIENCY_CUT',
     &      TRD_EFFICIENCY_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('DO_TRD_ANALYSIS',
     &      DO_TRD_ANALYSIS,IER)
          IF (IER.EQ.0) CALL EZGETA('IMPACT_PARAMETER_XY',0,0,0,N,IER)
          IF (IER.EQ.0) CALL EZGETA('IMPACT_PARAMETER_XY',1,N,1,
     &      TRK_IMPACT_XY_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('IMPACT_PARAMETER_Z',
     &      TRK_IMPACT_Z_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('MISSING_ET_FISOL_CUT',
     &      MISSING_ET_FISOL_CUT,IER)
          IF (IER.EQ.0) CALL EZGET('DO_CC_CLUS_ZCOR',
     &      DO_CC_CLUS_ZCOR, IER)
          IF (IER.EQ.0) CALL EZGET('DO_CDC_ZCOR',DO_CDC_ZCOR, IER)
          IF (IER.EQ.0) CALL EZGET('CDC_ZCOR_SLOPE',
     &      CDC_ZCOR_SLOPE, IER)
          IF (IER.EQ.0) CALL EZGET('ERRFACT_ZCOR',ERRFACT_ZCOR, IER)
          IF (IER.EQ.0) CALL EZGET('ERRFACT_CLUS_ZCOR',
     &      ERRFACT_CLUS_ZCOR, IER)
          IF (IER.EQ.0) CALL EZGET('ERRFACT_CDC_ZCOR',
     &      ERRFACT_CDC_ZCOR, IER)
          IF (DO_CC_CLUS_ZCOR.AND.DO_CDC_ZCOR) THEN
            ERR_DIST_CC(2) = ERR_DIST_CC(2) * ERRFACT_ZCOR
          ELSE IF (DO_CC_CLUS_ZCOR) THEN
            ERR_DIST_CC(2) = ERR_DIST_CC(2) * ERRFACT_CLUS_ZCOR
          ELSE IF (DO_CDC_ZCOR) THEN
            ERR_DIST_CC(2) = ERR_DIST_CC(2) * ERRFACT_CDC_ZCOR
          ENDIF
          IF (IER.EQ.0) CALL EZGET('USERMASK',USERMASK,IER)
          DO I = 1,2
            IF (ISOL_CONE_SIZE(I).NE.0.4) THEN
              IF (ISOL_CONE_SIZE(I).NE.0.7) THEN
                CALL ERRMSG('INVALID_CONE','CLEANEM',
     &            'INVALID CONE SIZE, IGNORED... USE 0.4 OR 0.7 ','W')
              ENDIF
            ENDIF
          ENDDO
          IF (IER.NE.0) THEN
            CALL ERRMSG('INCOMPLETE_CLEANEM_RCP','CLEANEM',
     &        ' Error reading RCP parameters ','F')
          ENDIF
C
        ELSE
          CALL ERRMSG(' NO_CLEANEM_RCP','CLEANEM',
     &        ' NO RCP file to work with ','F')
        ENDIF
        CALL EZRSET
C
C ****  Intialize TRD analysis
C
        IF (DO_TRD_ANALYSIS) THEN
          CALL EZLOC('TRD_ANALYSIS_RCP',LRCP)
          OK = LRCP .GT. 0
          IF (.NOT. OK) THEN
            TRDINI = TRD_ANALYSIS_INI()
            IF (.NOT.TRDINI) THEN
              CALL ERRMSG('TRDINI','CLEANEM',
     &            ' CANNOT INTIALIZE TRD ANALYSIS ','F')
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C **** Zero some things...
C
      OK = .TRUE.
      STATUS = 0
      LMIP = .TRUE.
      CDCMIP = 0.
      FDCMIP = 0.
      VTXMIP = 0.
      TRD_INFO = .FALSE.
      TRD_ACCEPTANCE=.FALSE.
      TRD_EFFICIENCY=0.
      CHISQ  = 0.
      NCELLS = 0
      EMFRAC = 1.
      DISPERSION = 0.
C
C ****  Get DATA TYPE, and RECO versions
C
C
      LHSTR = GZHSTR()
      DO WHILE (LHSTR.GT.0)
        CALL UHTOC(IQ(LHSTR+7),40,PROGRAM_NAME,40)
        IF(PROGRAM_NAME(1:11).EQ.'FULL_D0RECO') THEN
          RECO_VERSION = IQ(LHSTR+3)
          RECO_PASS    = IQ(LHSTR+4)
          GOTO 20
        ENDIF
        LHSTR = LQ(LHSTR)
      ENDDO
   20 CONTINUE
C
      MCDATA = .FALSE.
      RECORD_TYPE = IQ(LHEAD+1)
      IF (RECORD_TYPE.GE.1005) MCDATA = .TRUE.
C
C ****  Get version number of the bank
C
      VERSION = IQ(LCLEM+1)
      ECLUS  = Q(LCLEM+6)
      ETCLUS = Q(LCLEM+7)
C
C ****  Get Detector Eta
C
C   first get Z vertex

      CALL VERTEX_INFO(1,NV,ZVTX_INFO,OKZ)
C            Only consider the main primary vertex
      IF ( OKZ ) THEN
        ZV = ZVTX_INFO(1,1)
        DZ = ZVTX_INFO(2,1)
        IF(DZ.LT.DEL_ZVERTEX)DZ = DEL_ZVERTEX   ! Minimum value
      ELSE
        ZV = 0.0
        DZ = 0.0
      ENDIF

C   now compute detector eta
      THETA=Q(LCLEM+8)
      CALL DET_ETA(ZV,THETA,ETAD)
      ETA_DET = 10.* ETAD + SIGN(1.,ETAD)
C
C ****  CC/EC Flag  .. Bit 2 is ON for CC clusters and OFF for EC
C
      CAL_ETA = Q(LCLEM+19)
      IF (CAL_ETA.EQ.0) CAL_ETA = ETA_DET
      IF (IABS(CAL_ETA).LE.12) THEN
        STATUS = STATUS + 2**2
      ENDIF
C
C ****  HMATRIX chisq cut which is dependent on the detector eta
C ****  should be below the CUT value
C
      LHMTR = LQ(LCLEM-1)
      IF (LHMTR.GT.0) THEN
        CHISQ = Q(LHMTR+7)
        IF (CHISQ.GT.COARSE_CHISQUARED(IABS(CAL_ETA))) THEN
          STATUS = STATUS + 2**0
        ENDIF
        IF (CHISQ.GT.FINE_CHISQUARED(IABS(CAL_ETA))) THEN
          STATUS = STATUS + 2**1
        ENDIF
      ENDIF
C
C ****  Get shower centroid information
C       assume version 3 onwards the eta phi are from Log weighted cog
C
      ETAPHI(1) = Q(LCLEM+9)            ! eta
      ETAPHI(2) = Q(LCLEM+10)           ! phi
      ETAPHI(3) = Q(LCLEM+8)           ! theta
      IF (VERSION.GE.3) THEN
        CALL UHTOC(IQ(LCLEM-4),4,BANK,4)
        IF(BANK.EQ.'PPHO' .AND. VERSION.EQ.3) THEN
          DO I = 1, 3
            XBAR3(I) = Q(LCLEM+19+I)
          ENDDO
        ELSE
          DO I = 1, 3
            XBAR3(I) = Q(LCLEM+22+I)
          ENDDO
        ENDIF
      ENDIF
      LCACL = LQ(LCLEM-2)             ! Link to associated CACL bank
      IF (LCACL.GT.0) THEN
        CACL_VERSION = IQ(LCACL+1)
C
        DO I = 1, 3
          DBAR3(I) = Q(LCACL+19+I)
        ENDDO
C
        LCASH = LQ(LCACL-2)
        IF (LCASH .GT. 0) THEN
          NCELLS = IQ(LCASH+2)
          NCELL_EMIN = 0
          POINTER = 1
          DO I = 1,NCELLS
            POINTER = POINTER+2
            PACKED_WORD = IQ(LCASH+POINTER)
            ENERGY = Q(LCASH+POINTER+1)
            IF (ENERGY.GT.CELL_EMIN) THEN
              NCELL_EMIN = NCELL_EMIN+1
            ENDIF
          ENDDO
          IF (VERSION.LT.3) THEN
            IF (VERSION.EQ.1) THEN
              CALL CM3POS(LCASH,WEIGHT_CUT,XBAR3,DBAR3,ETAPHI,DETAPHI)
              IF(ETAPHI(1).EQ.-999)THEN  ! if CM3POS failed
                CALL ERRMSG('CM3POS','CLEANEM',
     &            'failed - use PELC/PPHO coordinates','W')
                XBAR3(1)=Q(LCACL+14)
                XBAR3(2)=Q(LCACL+15)
                XBAR3(3)=Q(LCACL+16)
                DO I = 1, 3
                  DBAR3(I) = 0.
                ENDDO
              ENDIF
            ELSE
              DO I = 1, 3
                XBAR3(I) = Q(LCACL+13+I)
                DBAR3(I) = Q(LCACL+19+I)
              ENDDO
            ENDIF
C         here correct Theta and eta returned by CM3POS for Zvertex
            ETAPHI(2) = ATAN2(XBAR3(2),XBAR3(1))    ! phi
            IF (ETAPHI(2).LT.0) ETAPHI(2) = ETAPHI(2) + TWOPI
            RHO = SQRT(XBAR3(2)**2+XBAR3(1)**2)
            ETAPHI(3) =  ATAN2(RHO,XBAR3(3)-ZV)     !Theta
            ETAPHI(1) = -ALOG(TAN(ETAPHI(3)/2.0))   !Eta
          ENDIF
C
C ****  correct for cluter Z-position bias
C
          IF (DO_CC_CLUS_ZCOR) THEN
cc            IF (VERSION.GE.6) THEN
Cc              CALL ERRMSG('CM3POS_PV','CLEANEM',
Cc     &        'CC Z Corrections already applied- skip corrections','W')
cc            ELSE
              CALL CM3POS_PV(LCASH, WEIGHT_CUT, XBAR3, DBAR3,
     &        ETAPHI, DETAPHI, ZV)
C         here correct Theta and eta returned by CM3POS for Zvertex
              RHO = SQRT(XBAR3(2)**2+XBAR3(1)**2)
              ETAPHI(3) =  ATAN2(RHO,XBAR3(3)-ZV)     !Theta
              ETAPHI(1) = -ALOG(TAN(ETAPHI(3)/2.0))   !Eta
cc            ENDIF
          ENDIF
        ENDIF
C
C* check for udst
C
        LUDST=GZUDST()                  ! Link to udst
        IF (LUDST.GT.0) THEN
          IF (BANK.EQ.'PELC') THEN
            DISPERSION=UDST_VALUE(BANK,'XDISPE',IQ(LCLEM-5),IER)
          ELSE IF (BANK.EQ.'PPHO') THEN
            DISPERSION=UDST_VALUE(BANK,'XDISPP',IQ(LCLEM-5),IER)
          ENDIF
          IF(IER.GT.0)THEN
            WRITE (MSG,100) IER
  100       FORMAT('ERROR=',I1)
            CALL ERRMSG('UDST_VALUE','CLEANEM',MSG,'W')
          ENDIF
        ELSE
          DISPERSION = DBAR3(1)+DBAR3(2)+DBAR3(3)
          IF (DISPERSION.GT.0) THEN
            DISPERSION = SQRT(DISPERSION)
          ELSE
            CALL ERRMSG('DISPERSION','CLEANEM',
     &        ' Value of dispersion is negative ','W')
          ENDIF
        ENDIF
C
C **** Theta, eta and phi of the cluster
C
        ETA_CLUS   = ETAPHI(1)
        PHI_CLUS   = ETAPHI(2)
        THETA_CLUS = ETAPHI(3)
C
C ****  Compute EM_FRACTION (EMtot(clus)-EFH1(clus))/Etot(EM+had)
C
C * CHECK FOR UDST
        LUDST=GZUDST()
        IF (LUDST.GT.0) THEN
          IF (BANK.EQ.'PELC') THEN
            EMFRAC=UDST_VALUE(BANK,'FEME',IQ(LCLEM-5),IER)
            WRITE (MSG,100) IER
            IF(IER.GT.0)CALL ERRMSG('UDST_VALUE','CLEANEM',MSG,'W')
          ELSE IF (BANK.EQ.'PPHO') THEN
            EMFRAC=UDST_VALUE(BANK,'FEMP',IQ(LCLEM-5),IER)
            WRITE (MSG,100) IER
            IF(IER.GT.0)CALL ERRMSG('UDST_VALUE','CLEANEM',MSG,'W')
          ENDIF
        ELSE
          IF (Q(LCACL+17).GT.0) THEN
            EMFRAC =(Q(LCACL+7)-Q(LCACL+19))/Q(LCACL+17) ! EM fraction
          ENDIF
        ENDIF
      ELSE
        CALL ERRMSG(' NO CACL OR CASH BANK','CLEANEM',
     &        'DOING MINIMAL ANALYSIS','W')
      ENDIF
C
C ****  Cut on EM fraction : cluster cleanup
C
      IF (EMFRAC.LT.EMFRAC_CUT) THEN
        STATUS = STATUS + 2**3
      ENDIF
C
C ****  Energy in cone (R=0.2) should be within CUT of cluster energy
C
      CORECLUS = (Q(LCLEM+15) - Q(LCLEM+6))/Q(LCLEM+6)
      IF (ABS(CORECLUS) .GT. CORECLUS_CUT) THEN
        STATUS = STATUS + 2**4
      ENDIF
C
C ****  Check on transverse dispersion
C
      IF (DISPERSION.GT.DISPERSION_CUT) THEN
        STATUS = STATUS + 2**5
      ENDIF
C
C ****  Compute Sigma5 - Sigma3 ala Level2
C
      CALL CF3SIGMA(SIGMA5,SIGMA3)
      DSIGMA = SIGMA5-SIGMA3
      IF (DSIGMA .GT. DSIGMA_CUT) THEN
        STATUS = STATUS + 2**6
      ENDIF
C
C ****  ISOLATION REQUIREMENT: Energy in cone (R=0.4/0.7) around
C ****  electron is less than CUT more than EM energy in cone (R=0.2)
C
      CORE_ET=Q(LCLEM+17)*SIN(Q(LCLEM+8))
      IF(Q(LCLEM+17).GT.0.AND.CORE_ET.GT.0)THEN
        DO I = 1, 2
          FISOL_EN(I)  = -1.0
          FISOL_ET(I)  = -1.0
          ISOL_EN(I)  = -1.0
          ISOL_ET(I)  = -1.0
          IF (ISOL_CONE_SIZE(I).EQ.0.4) THEN
            FISOL_EN(I)  = (Q(LCLEM+16) - Q(LCLEM+17))/Q(LCLEM+17)
            ISOL_EN(I)   = Q(LCLEM+16) - ECLUS
            IF (VERSION.GT.3 .AND. LCACL.GT.0) THEN
              FISOL_ET(I)  = (Q(LCACL+29) - CORE_ET)/CORE_ET
              ISOL_ET(I)   = Q(LCACL+29) - ETCLUS
            ENDIF
          ELSEIF (ISOL_CONE_SIZE(I).EQ.0.7) THEN
            IF (VERSION.GT.3 .AND. LCACL.GT.0) THEN
              FISOL_EN(I)  = (Q(LCACL+30) - Q(LCLEM+17))/Q(LCLEM+17)
              FISOL_ET(I)  = (Q(LCACL+31) - CORE_ET)/CORE_ET
              ISOL_EN(I)   = Q(LCACL+30) - ECLUS
              ISOL_ET(I)   = Q(LCACL+31) - ETCLUS
            ENDIF
          ENDIF
        ENDDO
      ELSE
        IF(Q(LCLEM+17).LE.0)CALL ERRMSG('CORE ENERGY','CLEANEM','=0',
     &    'W')
        IF(CORE_ET.LE.0)CALL ERRMSG('CORE ET','CLEANEM','=0','W')
        DO I=1,2
          FISOL_EN(I)=0.
          FISOL_ET(I)=0.
          ISOL_EN(I)=0.
          ISOL_ET(I)=0.
        ENDDO
      ENDIF
      IOFF = 0
      IF (IAND(STATUS,4).EQ.0) IOFF = 2     ! EC flag
      DO I = 1, 2
        IF (FISOL_EN(I) .GT. FISOL_ECUT(I+IOFF)) THEN
          STATUS = STATUS + 2**(6+I)
        ENDIF
        IF (FISOL_ET(I) .GT. FISOL_ETCUT(I+IOFF)) THEN
          STATUS = STATUS + 2**(8+I)
        ENDIF
        IF (I.EQ.2) THEN
          IF (ISOL_ET(I) .GT. ISOL_ETCUT(I+IOFF)) THEN
            STATUS = STATUS + 2**14
          ENDIF
        ENDIF
      ENDDO
C
C ****  ISOLATION FRACTION FRACTION IN 0.4 CONE FOR MISSING ET CORRECTIONS
C
      IF(Q(LCLEM+17).GT.0.AND.CORE_ET.GT.0)THEN
        FRAC_ISOL = (Q(LCLEM+16) - Q(LCLEM+17))/Q(LCLEM+17)
        IF ( FRAC_ISOL .GT. MISSING_ET_FISOL_CUT ) THEN
          STATUS = IBSET(STATUS,31)
        ENDIF
      ENDIF
C
C **** compute distance to closest phi crack in CC
C
C
      IF (IAND(STATUS,4).EQ.4) THEN                  ! if in CC
        IF (LCGEH.EQ.0) THEN
          CALL ERRMSG('Package CALOR not included','CLEANEM',
     &        'Cannot call CELXYZ to compute CRACK flag ','W')
          GOTO 550
        ENDIF
        EM_ARC = TWOPI/64.
        APHI = ATAN2(XBAR3(2),XBAR3(1))
        IF (APHI.LT.0) APHI = APHI + TWOPI
        IPHI1  = IFIX(APHI/EM_ARC)+1
        IETA   =  ETA_DET
        IF (IETA.EQ.-12) THEN   ! get layer index for EM3 cells next to crack
          ILYRL = 5             ! IETA=-12 doesn't have ILYR=3,4
          ILYRR = 6
        ELSE
          ILYRL = 3
          ILYRR = 4
        ENDIF
        IF (MOD(IPHI1,2).NE.0) THEN         ! if odd phi index
          CALL CELXYZ(IETA,IPHI1,ILYRL,X_CRACK1,Y_CRACK1,ZDUM,IER)
          IF (IER.EQ.0) THEN
            IPHI2 = IPHI1-1             ! get phi index on other side of crack
            IF (IPHI2.LE.0) IPHI2 = IPHI2+64
            CALL CELXYZ(IETA,IPHI2,ILYRR,X_CRACK2,Y_CRACK2,ZDUM,IER)
          ENDIF
        ELSE                                      ! if even phi index
          CALL CELXYZ(IETA,IPHI1,ILYRR,X_CRACK1,Y_CRACK1,ZDUM,IER)
          IF (IER.EQ.0) THEN
            IPHI2 = IPHI1+1
            IF (IPHI2.GT.64) IPHI2 = IPHI2-64
            CALL CELXYZ(IETA,IPHI2,ILYRL,X_CRACK2,Y_CRACK2,ZDUM,IER)
          ENDIF
        ENDIF
        IF (IER.EQ.0) THEN
          X_CRACK = (X_CRACK1+X_CRACK2)/2.  ! compute averages between cell
          Y_CRACK = (Y_CRACK1+Y_CRACK2)/2.  ! centers (=crack)
          D_CRACK = SQRT((X_CRACK-XBAR3(1))**2+(Y_CRACK-XBAR3(2))**2)
          D_CRACK = D_CRACK/SQRT(XBAR3(1)**2+XBAR3(2)**2)
          IF (D_CRACK.GT.TWOPI*1.05/64.) THEN  !this is not supposed to happen
            CALL ERRMSG('D_CRACK too big','CLEANEM',
     &        'don''t trust the crack flag ','W')
          ELSE
            IF (D_CRACK.LT.CC_CRACK_WIDTH/2.) THEN
              STATUS = STATUS + 2**11
            ENDIF
          ENDIF
        ELSE                              ! error from CELXYZ
          CALL ERRMSG('Error from CELXYZ','CLEANEM',
     &      'crack flag not set','W')
        ENDIF
  550   CONTINUE
      ENDIF
C
C ****  Number of cells in the cluster should be greater than CUT value
C
      IF (NCELLS .LT. NCELLS_MIN) THEN
        STATUS = STATUS + 2**12
      ENDIF
C
C ****  Check whether this electron/poton candidate was also found by Level2
C
      IF (LEVEL2_MATCH) THEN
        MASK = 0
        L2_FOUND = CMATCH_TRIGELEC(LCLEM,L1THR,L2THR,MASK,0.,0.,
     &    ET_L1,ET_L2,LL2EM)
        IF (.NOT.L2_FOUND) THEN
          STATUS = STATUS + 2**13
        ENDIF
      ENDIF
C
C
C ****  ANALYZE TRACK INFORMATION
C
      VERTEX_ID = 1
      NOTRKINFO = .FALSE.
      CALL UHTOC(IQ(LCLEM-4),4,BANK,4)
      IF (TRK.EQ.0) THEN 
        NOTRKINFO = .TRUE.
        VERTEX_ID = 0
        GOTO 900
      ENDIF
      IF (BANK.EQ.'PPHO') THEN
C        LZTRK = LQ(LCLEM-3)             ! Link to associated ZTRAK bank
C        IF (LZTRK .LE. 0) THEN 
          CALL GET_PPHO_TRACKS(LCLEM,NZTRAKS_PPHO,LZTRK_PPHO,VERTEX_ID)
          IF (NZTRAKS_PPHO.EQ.0) THEN
            NOTRKINFO = .TRUE.
            VERTEX_ID = 0
            GOTO 800      ! CHECK TO SEE IF TRD INFO PRESENT FOR PHOTON
          ENDIF
C        ENDIF
      ENDIF

C
C ****  TRACKING INFORMATION
C
      LZTRK = LQ(LCLEM-3)             ! Link to associated ZTRAK bank
      IF (LZTRK .LE. 0) THEN          ! This should not happen
        IF (BANK.EQ.'PPHO') THEN
          IF(NZTRAKS_PPHO.GT.0) THEN
            LZTRK = LZTRK_PPHO
          ENDIF
        ELSE
          CALL ERRMSG('NOZTRACK','CLEANEM',
     &    'No Ztrack associated with electron ! ','W')
          NOTRKINFO = .TRUE.
          GOTO 900      ! No Ztrack associated with the electron
        ENDIF
      ENDIF
      LZFIT = LQ(LZTRK-1)             ! Link to global fit
      IF (LZFIT .LE. 0) THEN          ! This should not happen
        CALL ERRMSG('NOZFIT','CLEANEM',
     &    'No ZFIT info associated with electron ZTRK ! ','W')
        NOTRKINFO = .TRUE.
        GOTO 900      ! No ZFIT info
      ENDIF
      LVTXT = LQ(LZTRK-6)
      LDTRK = LQ(LZTRK-7)
      LFDCT = LQ(LZTRK-8)
      PHI_TRK(1)   = Q(LZFIT+10)
      THETA_TRK(1) = Q(LZFIT+13)
      XCOG_TRK(1) = Q(LZFIT+11)
      YCOG_TRK(1) = Q(LZFIT+12)
      ZCOG_TRK(1) = Q(LZFIT+15)
      IF (DO_CDC_ZCOR) THEN
C  if want to correct for CDC z position bias for 
C  track match significance then use DTRK info only
C  Use CDC Z-bias correction as parameterized from muon studies
        LDTRH = GZDTRH()
        cdc_done = .false.
        IF (LDTRH .GT. 0) THEN
          IF (IBITS(IQ(LDTRH),0,1) .NE. 0) cdc_done = .true.
          IF (IBITS(IQ(LDTRH),1,1) .NE. 0) cdc_done = .true.
          IF (IBITS(IQ(LDTRH),2,1) .NE. 0) cdc_done = .true.
          IF (cdc_done) THEN
C            CALL ERRMSG('CLEANEM','CLEANEM',
C     &        ' CDC-ZCORR already done','W')
            GOTO 222
          ENDIF
        ENDIF
        IF (LDTRK.NE.0) THEN
          PHI_TRK(1)   = Q(LDTRK+6)
          THETA_TRK(1) = Q(LDTRK+9)
          XCOG_TRK(1) = Q(LDTRK+7)
          YCOG_TRK(1) = Q(LDTRK+8)
          ZCOG_TRK(1) = Q(LDTRK+11)
          ZCOG_TRK(1) = ZCOG_TRK(1) - CDC_ZCOR_SLOPE * ZCOG_TRK(1)
        ENDIF
  222   CONTINUE
      ENDIF
      IF (LDTRK.NE.0) CDCMIP = Q(LDTRK+20) ! Ionization in chamber
      IF (LFDCT.NE.0) FDCMIP = Q(LFDCT+20)
      IF (LVTXT.NE.0) VTXMIP = Q(LVTXT+20)
      TRK_IMPACT_XY = Q(LZFIT+32)
      TRK_IMPACT_Z  = Q(LZFIT+33)
C
C
C ****  TRD INFORMATION
C
      LUDST=GZUDST()                  ! Link to udst
      LTRDT = LQ(LZTRK-9)           ! Link to associated TRD bank
  800 CONTINUE
      IF (BANK.EQ.'PPHO') THEN
        IF (IQ(LCACL+1).GE.6) THEN
          LTRDT = LQ(LCACL-5)
        ELSE
          GOTO 900
        ENDIF
      ENDIF
      TRD_TRUNCATED_MEAN = 0.
      TRD_EFFICIENCY = 0.
      TRD_ACCEPTANCE = .FALSE.
      TRD_FROM_UDST = .FALSE.
      IF (LUDST.GT.0) THEN
        IF (DO_TRD_ANALYSIS) THEN
          IF (IQ(LUDST+1).LT.3) THEN
            TRD_FROM_UDST = .TRUE.
            IF (BANK.EQ.'PELC') THEN
              TRD_ACC =UDST_VALUE(BANK,'TRDACC',IQ(LCLEM-5),IER)
              IF(IER.GT.0) THEN
                WRITE (MSG,100) IER
                CALL ERRMSG('UDST_VALUE','CLEANEM',MSG,'W')
              ENDIF
              IF (TRD_ACC.EQ.0) THEN  
                TRD_ACCEPTANCE=.FALSE.
              ELSE
                TRD_ACCEPTANCE=.TRUE.
              ENDIF
              TRD_EFFICIENCY=UDST_VALUE(BANK,'TRDEFF',IQ(LCLEM-5),IER)
              IF(IER.GT.0) THEN
                WRITE (MSG,100) IER
                CALL ERRMSG('UDST_VALUE','CLEANEM',MSG,'W')
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF (LTRDT .GT. 0. .AND. (.NOT.TRD_FROM_UDST)) THEN
        ETOT_LIKE = Q(LTRDT+6)        ! Likelihood based on total
                                      ! anode energy
        ETOT_LEFF = Q(LTRDT+16)       ! Efficiency derived from likelihood
        NTRK_TRD = IQ(LTRDT + 2)
        NTRK_LYR1 = MOD(NTRK_TRD,10) + 1
        NTRK_TRD = NTRK_TRD/10
        NTRK_LYR2 = MOD(NTRK_TRD,10) + 1
        NTRK_TRD = NTRK_TRD/10
        NTRK_LYR3 = MOD(NTRK_TRD,10) + 1
        TRD_INFO  = (NTRK_LYR1.EQ.1).AND.(NTRK_LYR2.EQ.1).AND.
     &    (NTRK_LYR3.EQ.1)
        TRD_TRUNCATED_MEAN = Q(LTRDT+5)
      ENDIF
      IF (DO_TRD_ANALYSIS) THEN
        IF (.NOT.TRD_FROM_UDST)
     &    CALL TRD_ANALYSIS(LCLEM,TRD_ACCEPTANCE,TRD_EFFICIENCY)
      ELSE
        IF (LTRDT .GT. 0) THEN
          IF (Q(LTRDT+30).EQ.1) THEN
          TRD_ACCEPTANCE = Q(LTRDT+31).EQ.1
          TRD_EFFICIENCY = Q(LTRDT+32)
          ENDIF
        ENDIF
      ENDIF
C
      IF (NOTRKINFO) GOTO 900  ! SKIP IF NO ZTRK LINK
C
      NZTRAKS    = Q(LCLEM+21)
      IF (BANK.EQ.'PPHO') THEN
        IF(NZTRAKS_PPHO.GT.0) THEN
          NZTRAKS = NZTRAKS_PPHO
        ENDIF
      ENDIF
      DIFFPHI    = DIFF_PHI(PHI_CLUS,PHI_TRK(1))
      DIFFTHETA  = THETA_CLUS - THETA_TRK(1)
C
C ****  get the links of all Ztracks in the electron road
C
      IF (NZTRAKS.GT.1)  THEN
        PHI_LO = Q(LHMTR+9)
        PHI_HI = Q(LHMTR+10)
        THETA_LO = Q(LHMTR+11)
        THETA_HI = Q(LHMTR+12)
        IF (RECO_VERSION.EQ.11) THEN
C fix theta road computation..
          RHO = SQRT(XBAR3(2)**2+XBAR3(1)**2)
          DELZ = DZ*THETA_ROAD_FACT
          THETA_LO = ATAN2(RHO,XBAR3(3)-ZV+DELZ)
          THETA_HI  = ATAN2(RHO,XBAR3(3)-ZV-DELZ)
C ****  Impose minimum road size...
          IF(ABS(THETA_HI-THETA_LO).LT. 2.*MIN_THETA_ROAD) THEN
            THETA_LO = THETA_CLUS - MIN_THETA_ROAD
            THETA_HI = THETA_CLUS + MIN_THETA_ROAD
          ENDIF
        ENDIF
C
        CALL ZTRK_IN_ROAD(ZV,PHI_LO,PHI_HI,THETA_LO,THETA_HI,
     &        NTRAKS,ZLINK_TRAKS)
        IF (NTRAKS.EQ.0) THEN
          NTRAKS=1
          CALL ERRMSG('wrong ntraks','CLEANEM',
     &        'number of tracks in PELC road found =0, expect >=2','W')
          GOTO 110 
        ENDIF
C ****  Reserve links in ZLINKA
        DO I = 1,NTRAKS
          CALL GSLINK('CPHTRK',NTEMP(I))
          LSLINK(NTEMP(I)) = ZLINK_TRAKS(I)
          CALL ZTFLAG(ZLINK_TRAKS(I),'ELE')
        ENDDO
C      get info for all tracks in road
        ITRK = 1
        DO I = 1, NTRAKS
          LZTRAK_ELECTRON = LSLINK(NTEMP(I))
          IF (LZTRAK_ELECTRON.NE.LZTRK) THEN
            ITRK  = ITRK + 1
            LZFIT = LQ(LZTRAK_ELECTRON-1)
            PHI_TRK(ITRK)   = Q(LZFIT+10)
            THETA_TRK(ITRK) = Q(LZFIT+13)
            XCOG_TRK(ITRK) = Q(LZFIT+11)
            YCOG_TRK(ITRK) = Q(LZFIT+12)
            ZCOG_TRK(ITRK) = Q(LZFIT+15)
          ENDIF
        ENDDO
C ****  Release links in ZLINKA
        DO I = 1,NTRAKS
          CALL RSLINK('CPHTRK',NTEMP(I))
        ENDDO
C
        LZFIT = LQ(LZTRK-1)             ! reset Link primary ztrack in road
      ELSE
        NTRAKS = NZTRAKS
      ENDIF
  110 CONTINUE
C
C ****  TRACK MATCH REQUIREMENT
C ****  distance between EM cluster and primary CD track
C
      NCONE_TRK = 0
      DO ITRK = 1, NTRAKS
        TRKPAR(1) = PHI_TRK(ITRK)
        TRKPAR(2) = THETA_TRK(ITRK)
        TRKPAR(3) = XCOG_TRK(ITRK)
        TRKPAR(4) = YCOG_TRK(ITRK)
        TRKPAR(5) = ZCOG_TRK(ITRK)
        SIG_TRKMATCH(ITRK) = 0.
C
        CALL CLUSTRK_MATCH(XBAR3,TRKPAR,DIST_CLUSTRK,IER)
C
        ZCLUS = XBAR3(3)
        ZDIST = Q(LZFIT+33)
        IF (ABS(ZCLUS).LE.150.) THEN                        ! CC
C
C       take care of misalignment between CC and CDC
C       use NO MISALIGNMENT correction  for 1) MC data 
C                                 2) RECO versions 11.0 and above 
C        
C       AND for other versions correct for Mis-alignment ONLY IF 
C       CC_MISALIGN is set to TRUE in the RCP file
C
          CORR_CC_MISALIGN = .FALSE.
          CORR_CC_MISALIGN = .NOT.((RECO_VERSION.GE.11).OR.MCDATA)
C
          IF (CC_MISALIGN.AND.CORR_CC_MISALIGN) THEN
            DIST_CLUSTRK(1) = DIST_CLUSTRK(1) +
     &        CC_ALIGN_OFF(1)*SIN(PHI_TRK(ITRK)-CC_ALIGN_OFF(2))
     &        +CC_ALIGN_OFF(3)
          ENDIF
C
C       compute track parameters
          DO I = 1, 2
            IF (ITRK.EQ.1) THEN
              IF (ABS(DIST_CLUSTRK(I)).GT.DIST_TRKCUT_CC(I)) THEN
                STATUS = IOR(STATUS,2**(15+I))
              ENDIF
              DCLUSTRK(I) = DIST_CLUSTRK(I)
            ENDIF
C
            SIG_TRKMATCH(ITRK) = SIG_TRKMATCH(ITRK) +
     &        ((DIST_CLUSTRK(I)-OFFSET_DIST_CC(I))/ERR_DIST_CC(I))**2
          ENDDO
        ELSE                                                ! EC
          DO I = 1, 2
            IF (ITRK.EQ.1) THEN
              IF (ABS(DIST_CLUSTRK(I)).GT.DIST_TRKCUT_EC(I)) THEN
                STATUS = IOR(STATUS,2**(15+I))
              ENDIF
              DCLUSTRK(I) = DIST_CLUSTRK(I)
            ENDIF
C
            IF (ZCLUS.GT.0.) IOFF = 0
            IF (ZCLUS.LT.0.) IOFF = 1
C
C ****     Correction for EC radial offsets are controled by TURNOFF_EC_OFFSET
C ****     parameter. 
C ****        1) do not correct for EC_OFFSET for MC DATA
C ****        2) do not correct for EC_OFFSET for Data reco'd with version
C ****            11.17 and above
C ****        3) CORRECT for all other versions of reco
C
            IF (MCDATA) THEN
              CORR_EC_OFFSET = .FALSE.
            ELSE
              CORR_EC_OFFSET = .TRUE.
              IF (RECO_VERSION.GT.11) THEN
                CORR_EC_OFFSET = .NOT.TURNOFF_EC_OFFSET
              ELSE IF (RECO_VERSION.EQ.11) THEN
                IF (RECO_PASS.GE.17) THEN
                  CORR_EC_OFFSET = .NOT.TURNOFF_EC_OFFSET
                ENDIF
              ENDIF
            ENDIF
C
            IF (CORR_EC_OFFSET) THEN
              SIG_TRKMATCH(ITRK) = SIG_TRKMATCH(ITRK) +
     &          ((DIST_CLUSTRK(I)-OFFSET_DIST_EC(2*IOFF+I))
     &          /ERR_DIST_EC(2*IOFF+I))**2
            ELSE
              SIG_TRKMATCH(ITRK) = SIG_TRKMATCH(ITRK) +
     &          (DIST_CLUSTRK(I)/ERR_DIST_EC(2*IOFF+I))**2
            ENDIF
          ENDDO
        ENDIF
        DTRK_ETAPHI(ITRK) = SQRT(DIST_CLUSTRK(3)**2+DIST_CLUSTRK(4)**2)
        IF (DTRK_ETAPHI(ITRK).LE.TRACK_CONE_SIZE) THEN
          NCONE_TRK = NCONE_TRK + 1
        ENDIF
        SIG_TRKMATCH(ITRK) = SQRT(SIG_TRKMATCH(ITRK))
      ENDDO
C
C ****  Significance of cluster and track match
C
      IF (SIG_TRKMATCH(1).GT.SIG_TRKMATCH_CUT) THEN
        STATUS = STATUS + 2**18
      ENDIF
      DO I = 2, NZTRAKS
        IF (SIG_TRKMATCH(I).LE.SIG_TRKMATCH_CUT) THEN
          STATUS = IBSET(STATUS,19)
        ENDIF
      ENDDO
C
C ****  Number of tracks in a cone of dETAxdPHI
C
      IF (NCONE_TRK .GT. NZTRAKS_CUT) THEN
        STATUS = STATUS+2**20
      ENDIF
C
C ****  Use CDC/FDC track segment to check on track ionization (MIP)
C
      IF (LDTRK.GT.0) THEN
        IF (CDCMIP .GT. CDC_MIP_CUT(1) .AND.
     &         CDCMIP .LT. CDC_MIP_CUT(2)) STATUS = STATUS + 2**22
      ELSEIF (LFDCT.GT.0) THEN
        IF ((FDCMIP .GT. FDC_MIP_CUT(1)) .AND.
     &         (FDCMIP .LT. FDC_MIP_CUT(2))) STATUS = STATUS + 2**23
      ENDIF
      IF (LVTXT.GT.0) THEN
        IF ((VTXMIP .GT. VTX_MIP_CUT(1)) .AND.
     &         (VTXMIP .LT. VTX_MIP_CUT(2))) STATUS = STATUS + 2**24
      ENDIF
C
C ****  TRD information
C
C      IF (TRD_INFO .AND. ETOT_LEFF .GT. ETOT_LEFF_CUT) THEN
C        STATUS = STATUS+2**25
C      ENDIF
      IF (TRD_INFO) THEN
        IF (TRD_TRUNCATED_MEAN.LT.TRD_TRUNCATED_MEAN_CUT) THEN
          STATUS = STATUS+2**26
        ENDIF
      ELSE
        STATUS = STATUS + 2**25
      ENDIF
      IF (TRD_ACCEPTANCE) THEN
        IF (TRD_EFFICIENCY.GT.TRD_EFFICIENCY_CUT) THEN
          STATUS = STATUS+2**27
        ENDIF
      ENDIF
C
C ****  Check on TRK IMPACT PARAMETER  (if Zfit information exists)
C
      IF (LZFIT.NE.0) THEN
        IF (ABS(ZCLUS).LE.150.) THEN                        ! CC
          IF (TRK_IMPACT_XY.GT.TRK_IMPACT_XY_CUT(1)) THEN
            STATUS = STATUS+2**28
          ENDIF
        ELSE                                                ! EC
          IF (TRK_IMPACT_XY.GT.TRK_IMPACT_XY_CUT(2)) THEN
            STATUS = STATUS+2**28
          ENDIF
        ENDIF
        IF (ABS(TRK_IMPACT_Z).GT.TRK_IMPACT_Z_CUT) THEN
          STATUS = STATUS+2**29
        ENDIF
      ENDIF
C
  900 CONTINUE
C
C ****  Hits in road information
C
      CALL CLEAN_PHOTON(LHMTR,ITRUST)
      If (ITRUST .eq. -1) THEN
        STATUS = STATUS + 2**21   ! Bit set if the HITSINFO failed
      ELSEIF (ITRUST .eq. 0) THEN
        STATUS = STATUS + 2**30   ! Bit set if it is a photon-like cluster
      ENDIF
      CALL CLEAN_PHOTON_VAR(NTVAR_CLPHOT,QUANS)
      IF (NTVAR_CLPHOT .GT. N_PHOTON_VARS) THEN
         CALL ERRMSG ('n_photon_vars', 'cleanem',
     &                'photon vbl list overrun', 'F')
      ENDIF
C
C ****  Call routine to get spread and circularity
C
        CALL CLUSTER_SPREAD(LCLEM,SPREAD_VAR)
C
C ****  FINALLY Check USERMASK
C
      OK = .TRUE.
      IF (IAND(STATUS,USERMASK).NE.0) THEN
        OK = .FALSE.
      ENDIF
C
C ****  Update LCLUS if it has changed
C
      IF (LCLUS.NE.LCLEM) THEN
        LCLUS = LCLEM
      ENDIF
  950 RETURN
C===========================================================================

      ENTRY CLEANEM_CQUANS(NCVAR,CQUAN)
C
C  Return Calorimeter releated quantities associated with this cluster
C
      NCVAR = 30
      CQUAN(1) = 0
      CQUAN(2) = ECLUS
      CQUAN(3) = ETCLUS
      CQUAN(4) = CHISQ
      CQUAN(5) = CAL_ETA
      DO I = 1, 3
        CQUAN(5+I) = XBAR3(I)
      ENDDO
      CQUAN(9 ) = EMFRAC
      CQUAN(10) = CORECLUS
      CQUAN(11) = DISPERSION
      CQUAN(12) = DSIGMA
      DO I = 1, 2
        CQUAN(12+I) = FISOL_EN(I)
        CQUAN(14+I) = FISOL_ET(I)
      ENDDO
      CQUAN(17) = ETA_CLUS
      CQUAN(18) = PHI_CLUS
      CQUAN(19) = THETA_CLUS
      CQUAN(20) = D_CRACK
      CQUAN(21) = NCELLS
      CQUAN(22) = NCELL_EMIN
      DO I = 1, 2
        CQUAN(22+I) = ISOL_EN(I)
        CQUAN(24+I) = ISOL_ET(I)
      ENDDO
      CQUAN(27) = SPREAD_VAR(3)
      CQUAN(28) = SPREAD_VAR(8)
      CQUAN(29) = SPREAD_VAR(11)
      CQUAN(30) = SPREAD_VAR(18)
      RETURN
C
C ****  Names of the calorimeter variables
C
      ENTRY CLEANEM_CQUAN_NAMES(NCVAR,CQUAN_NAMES)
      NCVAR = 30
      CQUAN_NAMES(1) = 'DUMMY'
      CQUAN_NAMES(2) = 'ECLUS'
      CQUAN_NAMES(3) = 'ETCLUS'
      CQUAN_NAMES(4) = 'CHISQ'
      CQUAN_NAMES(5) = 'CAL_ETA'
      CQUAN_NAMES(6) = 'EM3XCOG'
      CQUAN_NAMES(7) = 'EM3YCOG'
      CQUAN_NAMES(8) = 'EM3ZCOG'
      CQUAN_NAMES(9) = 'EMFRAC'
      CQUAN_NAMES(10)= 'CORECLUS'
      CQUAN_NAMES(11) = 'DISP'
      CQUAN_NAMES(12) = 'SIG53'
      CQUAN_NAMES(13) = 'E_FISO1'
      CQUAN_NAMES(14) = 'E_FISO2'
      CQUAN_NAMES(15) = 'ET_FISO1'
      CQUAN_NAMES(16) = 'ET_FISO2'
      CQUAN_NAMES(17) = 'ETA_CLUS'
      CQUAN_NAMES(18) = 'PHI_CLUS'
      CQUAN_NAMES(19) = 'THETA_CL'
      CQUAN_NAMES(20) = 'D_CRACK'
      CQUAN_NAMES(21) = 'NCELLS'
      CQUAN_NAMES(22) = 'NCELLTHR'
      CQUAN_NAMES(23) = 'E_ISO1'
      CQUAN_NAMES(24) = 'E_ISO2'
      CQUAN_NAMES(25) = 'ET_ISO1'
      CQUAN_NAMES(26) = 'ET_ISO2'
      CQUAN_NAMES(27) = 'CIRCEM3'
      CQUAN_NAMES(28) = 'CIRCALL'
      CQUAN_NAMES(29) = 'SPRDEM3'
      CQUAN_NAMES(30) = 'SPRDALL'
      RETURN
C
C  Return tracking quantities and cluster/trcak match variables associated with
C  this cluster
C
      ENTRY CLEANEM_TQUANS(NTVAR,TQUAN)
      NTVAR = 23+NTVAR_CLPHOT
      DO I =1, NTVAR
        TQUAN(I)=0
      ENDDO
      IF (NOTRKINFO) THEN
        GOTO 990
      ENDIF
      TQUAN(1) = NZTRAKS
      TQUAN(2) = NCONE_TRK
      TQUAN(3) = DIFFPHI
      TQUAN(4) = DIFFTHETA
      TQUAN(5) = PHI_TRK(1)
      TQUAN(6) = THETA_TRK(1)
      TQUAN(7) = XCOG_TRK(1)
      TQUAN(8) = YCOG_TRK(1)
      TQUAN(9) = ZCOG_TRK(1)
      DO I =1, 2
        TQUAN(9+I) = DCLUSTRK(I)
      ENDDO
      TQUAN(12) = SIG_TRKMATCH(1)
      TQUAN(13) = CDCMIP
      TQUAN(14) = FDCMIP
      TQUAN(15) = VTXMIP
      TQUAN(16) = ETOT_LIKE
      TQUAN(17) = ETOT_LEFF
      IF(TRD_INFO)THEN
        TQUAN(18) = -1.
      ELSE
        TQUAN(18) = 0.
      ENDIF
      TQUAN(19) = TRD_TRUNCATED_MEAN
      TQUAN(20) = TRK_IMPACT_XY
      TQUAN(21) = TRK_IMPACT_Z
  990 CONTINUE
      IF (TRD_ACCEPTANCE) THEN
        TQUAN(22) = -1
      ELSE
        TQUAN(22) = 0
      ENDIF
      TQUAN(23) = TRD_EFFICIENCY
      DO I=1,NTVAR_CLPHOT
        TQUAN(23+I) = QUANS(I)
      END DO
      TQUAN(24+NTVAR_CLPHOT) = VERTEX_ID
      RETURN
C
C ****  tracking / cluster-track match variables
C
      ENTRY CLEANEM_TQUAN_NAMES(NTVAR,TQUAN_NAMES)
      TQUAN_NAMES(1) = 'NZTRAKS'
      TQUAN_NAMES(2) = 'NTRKCONE'
      TQUAN_NAMES(3) = 'DIFFPHI'
      TQUAN_NAMES(4) = 'DIFFTHET'
      TQUAN_NAMES(5) = 'PHI_TRK'
      TQUAN_NAMES(6) = 'THE_TRK'
      TQUAN_NAMES(7) = 'XPT_TRK'
      TQUAN_NAMES(8) = 'YPT_TRK'
      TQUAN_NAMES(9) = 'ZPT_TRK'
      TQUAN_NAMES(10) = 'RDPHI'
      TQUAN_NAMES(11) = 'DZ_DR'
      TQUAN_NAMES(12) = 'MATCHSIG'
      TQUAN_NAMES(13) = 'CDCMIP'
      TQUAN_NAMES(14) = 'FDCMIP'
      TQUAN_NAMES(15) = 'VTXMIP'
      TQUAN_NAMES(16) = 'TRD_LIKE'
      TQUAN_NAMES(17) = 'TRD_LEFF'
      TQUAN_NAMES(18) = 'TRD_INFO'
      TQUAN_NAMES(19) = 'TRD_MEAN'
      TQUAN_NAMES(20) = 'XY_IMP'
      TQUAN_NAMES(21) = 'Z_IMPACT'
      TQUAN_NAMES(22) = 'TRD_ACC'
      TQUAN_NAMES(23) = 'TRD_EFF'
      CALL CLEAN_PHOTON_NAMES(NTVAR_CLPHOT,QNAMES)
      IF (NTVAR_CLPHOT .GT. N_PHOTON_VARS) THEN
         CALL ERRMSG ('n_photon_vars', 'cleanem',
     &                'photon vbl list overrun', 'F')
      ENDIF
      DO I=1,NTVAR_CLPHOT
        TQUAN_NAMES(23+I) = QNAMES(I)
      END DO
      NTVAR = 23+NTVAR_CLPHOT+1
      TQUAN_NAMES(23+NTVAR_CLPHOT+1) = 'VERTID'
C
  999 RETURN
      END

