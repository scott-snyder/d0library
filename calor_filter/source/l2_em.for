      SUBROUTINE L2_EM(PAR_SET,HAD_BIT,RETURN_FLAG,E_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Driving routine of electron tool
C-
C-   Inputs  : Data from Calorimeter
C-             Criteria for electron identification
C-   Outputs : Set Return_Flag = .TRUE., if enough candidates passed
C-   Controls: ET_IN_CAEP = .TRUE. means the CAEP bank contains ET instead of E
C-             PHOTON controls em vs photon cuts
C-             CD_TR_C  controls track requirement
C-             There are other flags set up in L2_EM_RCP; see L2_EM_PARAMETERS
C-
C-   Created 15-MAY-1990   Yi  Xia
C-   Updated  16-DEC-1991   James T. Linnemann  split fill from cutting
C-        redo some high level logic, rename variables, simplify geometry
C-        fix track logic, add cut on ET of candidate
C-   Updated   8-FEB-1992   James T. Linnemann  Debugging by Scott Snyder
C-      remove old unpacking; Test ALL relevant candidates so ESUM fully filled
C-   Updated   19-FEB_1992  Yi  Xia -1. off set of shower position in -ETA
C-   Updated   21-FEB-1992  Yi  Xia Add memory array to record the filtering
C-      result and avoid the unnecessary check for the event which was done
C-   Updated   24-FEB-1992  Yi  Xia put results into L2EM bank
C-   Updated   25-FEB-1992  Yi  Xia add isolation cut code
C-   Updated   6-SEP-1992   James T. Linnemann  remove memory code
C-      (calorimeter code is fast; tracking code has its own memory)
C-   Updated   06-SEP-1992  James T. McKinley  calculate all shape variables
C-                          whether or not candidate failed prior cut to
C-                          facilitate filling of L2EM bank with all variables
C-   Updated   27-OCT-1992  James T. McKinley  define new IFAILED code = 666
C-                          to fail candidates where we should never get a
C-                          trigger (ieta=13, ieta>32).  If PASSED is false
C-                          from L2_EM_CUT_BINS fail the candidate.
C-   Updated    7-DEC-1992  James T. McKinley correct eta for vertex position
C-                          and put in ESUMFL call, add ETA_PHYS
C-                          to L2EMFL call.
C-   Updated   29-MAR-1993  James T. McKinley add shower leakage fix for
C-                          Et of candidate, also used in isolation routine
C-   Updated  13-NOV-1993   James T. Linnemann   CDC and FDC control
C-   Updated  23-SEP-1994   Lewis T. Goss moved code to L2_EM_GET_CAND to 
C-                          reduce net processing time
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      INCLUDE 'D0$INC:L2_EM_STP.INC'
      INCLUDE 'D0$INC:L2_EM_CUTS.INC'
      INCLUDE 'D0$INC:L2_EM_CUTS_C.INC'
      INCLUDE 'D0$INC:L2_EM_CUTS_L.INC'
      INCLUDE 'D0$INC:L2JETS_HOT.INC'            ! The Hot Towers from L1
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INTEGER IETA,IPHI,LYR,ICAND,NE,K,IER,IFAILED,
     &  IFAILED_LONG,IFAILED_TRANS
      INTEGER PAR_SET,HAD_BIT,BIT_MASK
      INTEGER MAX_PASS
      PARAMETER( MAX_PASS = 20 )
      INTEGER ETAPASS(MAX_PASS),PHIPASS(MAX_PASS),CUTBITS
      REAL ET_MIN,ET_CAND,ET_VTX0,ET_ZCORR,EFLOOR(8),SUMEM,ET_Z_E_CORR
      REAL EM3(-2:2,-2:2),ISOL,SIG3_MID,CONE_FRAC,E_CAND,E3,E5,E7
      REAL AETA,APHI,XYZ_CLUS(3),SIG_ETA,SIG_PHI,DETA,DPHI
      LOGICAL OK,E_FLAG,RETURN_FLAG,PASSED,L2JETS_HOTFL
      LOGICAL CC,PHOTON,ELECTRON,LONG,TRANS,VETO_TRACK,CHECK_TRACK
      LOGICAL DO_ISOL_CUT,TIGHT,TRY_CDC,TRY_FDC,NO_DECISION
      INTEGER JENRG_BIN,JETA_BIN      ! bin indices for cuts
      INTEGER TGETA,TGPHI
      REAL SIGMA3, S3, RA35, RA57, ESH2, ESH4, RA24, SIGMA5
      REAL RBF,RA1,RA12,RA3,RA4
      REAL SH13,SH24,SH35,SH57,GOLD,CL2_ET_CORR_FINE
      REAL ETA_PHYS,ZVTX,L2_VERT
      LOGICAL DO_ZCORR,DO_LEAK_CORR
      SAVE DO_ZCORR,DO_LEAK_CORR
      DATA DO_ZCORR/.TRUE./
      DATA DO_LEAK_CORR/.TRUE./
C----------------------------------------------------------------------
C
C
C...get parameter set number
      IF ((PAR_SET.LE.0).OR.(PAR_SET.GT.NPARIN)) THEN
        CALL ERRMSG('L2_EM','L2_EM','Parameter set # out of range','E')
      ENDIF
C
      CALL L2_EM_PARSE_CUTS(CD_TR_C(PAR_SET),SHAPE_C(PAR_SET),
     &  DO_ISOLATION(PAR_SET),CHECK_TRACK,VETO_TRACK,TRY_CDC,TRY_FDC,
     &  DO_ISOL_CUT,ELECTRON,PHOTON,LONG,TRANS,TIGHT,CUTBITS)
C
      RETURN_FLAG = .FALSE.
      NE = 0
C
C     Find all EM shower candidates
      OK = L2JETS_HOTFL()            ! Fill the hot tower tables
C
C      Loop through these e and Gamma candidates
C
      BIT_MASK = HAD_BIT  ! will look at ONLY candidates from this L1 trigger
C
      ZVTX = L2_VERT()
      DO 500 ICAND = 1, NEMHOT
        TGETA = -999    !check for candidate irrelevant to this bit
        CALL L2_EM_GET_CAND(ICAND,BIT_MASK,ZVTX,DO_ZCORR,DO_LEAK_CORR,
     &    IETA,IPHI,LYR,EFLOOR,ET_VTX0,ET_CAND,E_CAND,EM3,E3,E5,E7,
     &    TGETA,TGPHI,PASSED,AETA,APHI,XYZ_CLUS,SIG_ETA,SIG_PHI,
     &    ETA_PHYS,ET_Z_E_CORR)
C
        IF (TGETA.EQ.-999) GO TO 500  !irrelevant; don't even book a bank
C
C...cannot fill shape variables in this case, may be unpacking problems
        IFAILED = 2  !candidate not found, not unpacked, EM3 empty ,or neg Et
        IF(.NOT.PASSED) GO TO 300
C
C...select cuts
        IFAILED = 0     !IFAILED will contain the code of 1st failure found
        CC = IABS(IETA).LE.12
        IF (CC) THEN
          ET_MIN = CCET_CUT(PAR_SET)
        ELSE
          ET_MIN = ECET_CUT(PAR_SET)
        ENDIF
C...threshold cut
        IF (ET_CAND.LT.ET_MIN) IFAILED = 1    ! Et cut
C
C...shape cuts
C
        CALL L2_EM_CUT_BINS(E_CAND,IETA,JENRG_BIN,JETA_BIN,PASSED)
        IF(.NOT.PASSED.AND.IFAILED.EQ.0) IFAILED=666  !trigger at unexpected eta
C
        CALL L2_EM_LONG_CUT(IETA,JENRG_BIN,JETA_BIN,EFLOOR,PHOTON,
     &        TIGHT,SUMEM,RBF,RA1,RA12,RA3,RA4,IFAILED_LONG,LONG,PASSED)
        IF (IFAILED.EQ.0) IFAILED = IFAILED_LONG  ! report 1st failure only
C
        CALL L2_EM_TRANS_CUT(JENRG_BIN,JETA_BIN,IETA,CC,EM3,
     &        E3,E5,E7,TIGHT,IFAILED_TRANS,TRANS,PASSED,
     &        SIGMA3,S3,SH13,SH35,SH57,ESH2,ESH4,SH24,SIGMA5,SIG3_MID)
        IF (IFAILED.EQ.0) IFAILED = IFAILED_TRANS  ! report 1st failure only
C
        IF(IFAILED.NE.0) GOTO 300
C
C...track match
        IF (CHECK_TRACK) THEN
C...see if in a detector where trying a match
          IF ( (CC.AND.TRY_CDC).OR.( (.NOT.CC).AND.TRY_FDC) ) THEN
            CALL L2_EM_TRACK_ROADS(PAR_SET,E_CAND,SIG_ETA,SIG_PHI,
     &        XYZ_CLUS,DETA,DPHI)
            CALL L2_CD_MATCH(AETA,APHI,XYZ_CLUS,DETA,DPHI,
     &        PASSED,NO_DECISION)
            IF (.NOT.NO_DECISION) THEN  !then in a region where search performed
              IF (.NOT.PASSED.AND..NOT.VETO_TRACK) IFAILED = 100 !no track match
              IF (VETO_TRACK.AND.PASSED) IFAILED = 101 ! unwanted track match
              IF (IFAILED.NE.0) GO TO 300
            ENDIF
          ENDIF
        ENDIF
C...Apply the isolation cut
        IF (DO_ISOL_CUT) THEN
          CALL L2_EM_ISOL(IETA,IPHI,LYR,EFLOOR,CONE_R(PAR_SET),
     &        CONE_FRACT_MAX(PAR_SET),CONE_FRAC,PASSED)
          IF (.NOT. PASSED) IFAILED = 70  ! fail on Isolation cut
          IF (IFAILED .NE. 0) GO TO 300
        ENDIF
C...check if this is an old candidate refound
        DO K = 1,NE
          IF(IABS(IETA-ETAPASS(NE))+IABS(IPHI-PHIPASS(NE)).LE.1)GOTO 300
        ENDDO
C...save this new candidate
        IF (NE.LT.MAX_PASS) THEN
          NE = NE + 1
          ETAPASS(NE) = IETA
          PHIPASS(NE) = IPHI
        ENDIF
        IF (ELECTRON) THEN                        !only meant to be an electron
          CALL ESUMFL('FILT',ID_ELECTRON,ET_CAND,ETA_PHYS,
     &        AETA,APHI,CUTBITS)
        ELSEIF (PHOTON) THEN                        !only meant to be a photon
          CALL ESUMFL('FILT',ID_PHOTON,ET_CAND,ETA_PHYS,
     &        AETA,APHI,CUTBITS)
        ELSE                                          !could be either
          CALL ESUMFL('FILT',ID_ELECTRON,ET_CAND,ETA_PHYS,
     &        AETA,APHI,CUTBITS)
          CALL ESUMFL('FILT',ID_PHOTON,ET_CAND,ETA_PHYS,
     &        AETA,APHI,CUTBITS)
        ENDIF
C
C   filling the L2EM bank with results of this filter
C
  300   CONTINUE
C
C...warning!! ALL these variables are zero'd inside this routine, except those
C     read directly from arrays, and CUTBITS
        CALL L2EMFL (TGETA,TGPHI,IETA,IPHI,LYR,ET_VTX0,SUMEM,RA1,
     &      RA12,RA3,RA4,RBF,SIGMA3,SIGMA5,SIG3_MID,SH13,SH24,SH35,SH57,
     &      CONE_R(PAR_SET),CONE_FRAC,DETA,DPHI,
     &      CHECK_TRACK,IFAILED,PAR_SET,AETA,APHI,XYZ_CLUS,ET_Z_E_CORR
     &      ,CUTBITS,ETA_PHYS)
  500 CONTINUE
C
  999 CONTINUE
C...do test on count ONLY at end, so all good candiates go into ESUM for
C       topology filters
      RETURN_FLAG = (NE.GE.NUMBER_EM(PAR_SET))
      RETURN
      END
