      LOGICAL FUNCTION PARTICLE_SELECT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This package makes a list of the reference links to
C-                        GOOD ELECTRONS, PHOTONS, JETS AND MUONS in an event.
C-                        These links are then stored in the ZLINKA area and
C-                        therefore are accessible via any other user analysis
C-                        package.
C-
C-                        The kinematic quantities and the MASKs of all
C-                        objects (electrons, photons, muons, jets) are
C-                        obtained from PARTICLE_SELECT.RCP
C-
C-     To obtain the list of links to GOOD objects from the user analysis
C-     package one can use the routine GTSLINK.
C-
C-     For example to get the links of GOOD MUONS
C-
C-         CALL GTSLINK(object_name,nwant,nmuon,muon_links)
C-
C-          input     object_name CHAR*8 : name of object link
C-                                 defined by ELE_NAMES, PHOT_NAMES,MUON_NAMES
C-                                array
C-                    nwant    : maximum number of links wanted
C-          returns   nmuon    : number of selected muons
C-                    muon_links[1:nwant] : is the array of links to the
C-                                          selected muons
C-
C-
C-   Returned value  : ALWAYS TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: PARTICLE_SELECT RCP
C-
C-   Created  16-JAN-1994   Meenakshi Narain
C-   Updated  Mar-08-1994   Bob Kehoe -- add JET_MIN_DELTA_R jet selection cut
C-   Updated  21-MAR-1994   Meenakshi Narain
C-                          Add switch to selectively match electrons and
C-                          photons to jets
C-   Updated   2-APR-1994   Meenakshi Narain
C-                          make jets names RCP parameter
C-   Updated  May-30-1994   Bob Kehoe --  remove jets --> pelc/ppho link check
C-   Updated  Jul-18-1994   Bob Kehoe --  allow several jet definitions
C-   Updated  31-AUG-1995   sss - Reduce maximum object counts from 20
C-                                to 12 to prevent overflows in gslink.
C-
C-   Updated  10-SEP-1995   Meenakshi Narain
C-                          add match to ISAL electrons for MCDATA
C-                          option to turn off elike cut for MCDATA
C-
C-   Updated   1-OCT-1995   Meenakshi Narain
C-                          Add MC_MUONS on JDH's request
C-
C-   Updated  12-OCT-1995   John Hobbs
C-                          Change muon eta cut to region cut ala
C-                          latest MUON_SELECT.FOR
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER RECORD_TYPE
      CHARACTER*8 LINK_TYPE(NSLINK)
      INTEGER LINK_LOC(NSLINK)
      INTEGER LRCP,IER,I,J,K
      INTEGER NLINKS,OBJECT_LINK(100),MAX_OBJ
      INTEGER NELE_MAX, NPHOT_MAX, NJET_MAX, NMUON_MAX
      PARAMETER( MAX_OBJ   = 8 )
      PARAMETER( NELE_MAX  = 12 )
      PARAMETER( NPHOT_MAX = 12 )
      PARAMETER( NJET_MAX  = 12 )
      PARAMETER( NMUON_MAX = 12 )
      INTEGER NELE_TYPE, NELEC, ELE_MASK(MAX_OBJ)
      INTEGER NPHOT_TYPE, NPHOT, PHOT_MASK(MAX_OBJ)
      INTEGER NMUON_TYPE, NMUON, MUON_MASK(MAX_OBJ)
      INTEGER NMC_MUON_TYPE, MC_MUON_MASK(MAX_OBJ)
      INTEGER NJET_TYPE, NJET, JET_ALGORITHM(MAX_OBJ)
      INTEGER NREMOVE_ELEC_JETS_TYPE
      INTEGER NREMOVE_PHOTON_JETS_TYPE
      INTEGER JTYPE,IETA,JER,IRGN,IFW2
      REAL    PT,ET,ETA,PHI,DELTA_R,DELTA_PHI
      REAL    ELE_MIN_ET(MAX_OBJ),ELE_MAX_ETA(MAX_OBJ)
      REAL    ELE_MAX_LKLY_CC(MAX_OBJ),ELE_MAX_LKLY_EC(MAX_OBJ)
      REAL    PHOT_MIN_ET(MAX_OBJ),PHOT_MAX_ETA(MAX_OBJ)
      REAL    MUON_MIN_PT(MAX_OBJ),MC_MUON_MIN_PT(MAX_OBJ)
      INTEGER MUON_MAX_RGN(MAX_OBJ),MC_MUON_MAX_RGN(MAX_OBJ)
      REAL    JET_MIN_ET(MAX_OBJ),JET_MAX_ETA(MAX_OBJ)
      REAL    JET_MIN_DELTA_R(MAX_OBJ)
      REAL    ELE_PHI,PHOT_PHI,JET_PHI,ELE_LKL
      REAL    ELE_ETA,PHOT_ETA,JET_ETA,ELIKE
      REAL    ELE_MAX_LKLY,FHAD
      REAL    ELIKE_SET_MASK_CC,DUMMY,ELIKE_SET_MASK_EC
C      INTEGER MASK_TRDOFF,MASK_EC
      INTEGER MASK_CC,MASK_EC
      INTEGER ELIKE_MASK_CC(MAX_OBJ),ELIKE_MASK_EC(MAX_OBJ)
C      PARAMETER(MASK_TRDOFF  = '0F'X )
C      PARAMETER(MASK_EC      = '0F'X )
      INTEGER LPELC,LPPHO,LJETS,LPMUO,LMUOT
      INTEGER GZPELC,GZPPHO,GZJETS,GZPMUO
      INTEGER NS,NL,IVAR,STATUS,NZTRK
      INTEGER NELE_TOTAL(MAX_OBJ),ELE_LINKS(NELE_MAX,MAX_OBJ)
      INTEGER NPHOT_TOTAL(MAX_OBJ),PHOT_LINKS(NPHOT_MAX,MAX_OBJ)
      REAL   TQUAN(50),VAR,PHOT_TRKSIG(MAX_OBJ)
      REAL   ZVTXEM,ETZV(4),THZV(4),ETMAX, ETAMX
      INTEGER LZTRKPV
      CHARACTER*8 OBJECT,ELE_TYPE(MAX_OBJ),PHOT_TYPE(MAX_OBJ)
      CHARACTER*8 MUON_TYPE(MAX_OBJ),JET_TYPE(MAX_OBJ)
      CHARACTER*8 MC_MUON_TYPE(MAX_OBJ)
      CHARACTER*8 REMOVE_ELEC_JETS_TYPE(MAX_OBJ)
      CHARACTER*8 REMOVE_PHOTON_JETS_TYPE(MAX_OBJ)
      LOGICAL PARTICLE_SELECT_FIN,TURN_OFF_ELIKE_MC,MCDATA
      LOGICAL OK,MATCH,QMCMATCH,REQ_MCMATCH,MATCHEM_MC_DATA
      LOGICAL first,USE_MUON_MCNAMES,LOOP_OVER_VERTICES
      SAVE first
      DATA first / .true. /


C----------------------------------------------------------------------
      PARTICLE_SELECT = .TRUE.
      NLINKS = 0
C
C ****  Read / Intialize Parameters from RCP
C
      IF( first ) THEN
        first = .false.
C
C ****  Read RCP file
C
        CALL EZLOC('PARTICLE_SELECT_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP('PARTICLE_SELECT_RCP',IER)
          IF (IER.EQ.0) CALL EZPICK('PARTICLE_SELECT_RCP')
          IF (IER.EQ.0) CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('PARTICLE_SELECT','PARTICLE_SELECT',
     &        ' PARTICLE_SELECT_RCP not found','F')
          ENDIF
          CALL EZRSET
        ENDIF
C
C ****  Read in RCP parameters
C
        CALL EZPICK('PARTICLE_SELECT_RCP')
        CALL EZERR(IER)
        IF (IER.EQ.0) THEN
          IF (IER.EQ.0) CALL EZGET('LOOP_OVER_VERTICES',
     &      LOOP_OVER_VERTICES,IER)
          IF (IER.EQ.0) CALL EZ_GET_CHARS('ELE_NAMES',NELE_TYPE,
     &      ELE_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MASK',0,0,0,NELE_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MASK',1,NELE_TYPE,1,ELE_MASK,
     &      IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MIN_ET',0,0,0,NELE_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MIN_ET',1,NELE_TYPE,1,
     &      ELE_MIN_ET,IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MAX_ETA',0,0,0,NELE_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MAX_ETA',1,NELE_TYPE,1,
     &      ELE_MAX_ETA,IER)
          IF (IER.EQ.0) CALL EZGETA('ELIKE_MASK_CC',
     &      0,0,0,NELE_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('ELIKE_MASK_CC',
     &      1,NELE_TYPE,1,ELIKE_MASK_CC,IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MAX_LIKELIHOOD_CC',
     &      0,0,0,NELE_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MAX_LIKELIHOOD_CC',
     &      1,NELE_TYPE,1,ELE_MAX_LKLY_CC,IER)
          IF (IER.EQ.0) CALL EZGETA('ELIKE_MASK_EC',
     &      0,0,0,NELE_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('ELIKE_MASK_EC',
     &      1,NELE_TYPE,1,ELIKE_MASK_EC,IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MAX_LIKELIHOOD_EC',
     &      0,0,0,NELE_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('ELE_MAX_LIKELIHOOD_EC',
     &      1,NELE_TYPE,1,ELE_MAX_LKLY_EC,IER)
          IF (IER.EQ.0) CALL EZ_GET_CHARS('PHOT_NAMES',NPHOT_TYPE,
     &      PHOT_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('PHOT_MASK',0,0,0,NPHOT_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('PHOT_MASK',1,NPHOT_TYPE,1,
     &      PHOT_MASK,IER)
          IF (IER.EQ.0) CALL EZGETA('PHOT_MIN_ET',0,0,0,NPHOT_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('PHOT_MIN_ET',1,NPHOT_TYPE,1,
     &      PHOT_MIN_ET,IER)
          IF (IER.EQ.0) CALL EZGETA('PHOT_MAX_ETA',0,0,0,NPHOT_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('PHOT_MAX_ETA',1,NPHOT_TYPE,1,
     &      PHOT_MAX_ETA,IER)
          IF (IER.EQ.0) CALL EZGETA('PHOT_TRKSIG',0,0,0,NPHOT_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('PHOT_TRKSIG',1,NPHOT_TYPE,1,
     &      PHOT_TRKSIG,IER)
          IF (IER.EQ.0) CALL EZ_GET_CHARS('MUON_NAMES',NMUON_TYPE,
     &      MUON_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('MUON_MASK',0,0,0,NMUON_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('MUON_MASK',1,NMUON_TYPE,1,
     &      MUON_MASK,IER)
          IF (IER.EQ.0) CALL EZGETA('MUON_MIN_PT',0,0,0,NMUON_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('MUON_MIN_PT',1,NMUON_TYPE,1,
     &      MUON_MIN_PT,IER)
          IF (IER.EQ.0) CALL EZGETA('MUON_MAX_RGN',0,0,0,NMUON_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('MUON_MAX_RGN',1,NMUON_TYPE,1,
     &      MUON_MAX_RGN,IER)
          IF (IER.EQ.0) CALL EZGET('USE_MUON_MCNAMES',USE_MUON_MCNAMES,
     &      IER)
          IF (IER.EQ.0) CALL EZ_GET_CHARS('MC_MUON_NAMES',NMC_MUON_TYPE,
     &      MC_MUON_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('MC_MUON_MASK',0,0,0,
     &      NMC_MUON_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('MC_MUON_MASK',1,NMC_MUON_TYPE,1,
     &      MC_MUON_MASK,IER)
          IF (IER.EQ.0) CALL EZGETA('MC_MUON_MIN_PT',0,0,0,
     &      NMC_MUON_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('MC_MUON_MIN_PT',1,NMC_MUON_TYPE,1,
     &      MC_MUON_MIN_PT,IER)
          IF (IER.EQ.0) CALL EZGETA('MC_MUON_MAX_RGN',0,0,0,
     &      NMC_MUON_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('MC_MUON_MAX_RGN',1,NMC_MUON_TYPE,1,
     &      MC_MUON_MAX_RGN,IER)
          IF (IER.EQ.0) CALL EZ_GET_CHARS('JET_NAMES',NJET_TYPE,
     &      JET_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('JET_ALGORITHM',0,0,0,NJET_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('JET_ALGORITHM',1,NJET_TYPE,1,
     &      JET_ALGORITHM,IER)
          IF (IER.EQ.0) CALL EZGETA('JET_MIN_ET',0,0,0,NJET_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('JET_MIN_ET',1,NJET_TYPE,1,
     &      JET_MIN_ET,IER)
          IF (IER.EQ.0) CALL EZGETA('JET_MAX_ETA',0,0,0,NJET_TYPE,IER)
          IF (IER.EQ.0) CALL EZGETA('JET_MAX_ETA',1,NJET_TYPE,1,
     &      JET_MAX_ETA,IER)
          IF (IER.EQ.0) CALL EZGETA('JET_MIN_DELTA_R',0,0,0,NJET_TYPE,
     &      IER)
          IF (IER.EQ.0) CALL EZGETA('JET_MIN_DELTA_R',1,NJET_TYPE,1,
     &      JET_MIN_DELTA_R,IER)
          IF (IER.EQ.0) CALL EZ_GET_CHARS('REMOVE_PHOTON_JETS',
     &      NREMOVE_PHOTON_JETS_TYPE,REMOVE_PHOTON_JETS_TYPE,IER)
          IF (IER.EQ.0) CALL EZ_GET_CHARS('REMOVE_ELEC_JETS',
     &      NREMOVE_ELEC_JETS_TYPE,REMOVE_ELEC_JETS_TYPE,IER)
          IF (IER.EQ.0) CALL EZGET('REQ_MCMATCH',REQ_MCMATCH,
     &      IER)
          IF (IER.EQ.0) CALL EZGET('TURN_OFF_ELIKE_MC',
     &      TURN_OFF_ELIKE_MC,IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG(' INVALID_PARAMS','PARTICLE_SELECT',
     &        ' Invalid RCP parameters ','F')
          ENDIF
          IF (NELE_TYPE.GT.MAX_OBJ.OR.NPHOT_TYPE.GT.MAX_OBJ) THEN
            CALL ERRMSG(' MAX_OBJECTS','PARTICLE_SELECT',
     &        ' exceeded maximum number of ele/gam types ','F')
          ENDIF
          IF (NMUON_TYPE.GT.MAX_OBJ) THEN
            CALL ERRMSG(' MAX_OBJECTS','PARTICLE_SELECT',
     &        ' exceeded maximum number of muon types ','F')
          ENDIF
          IF (NJET_TYPE.GT.MAX_OBJ) THEN
            CALL ERRMSG(' MAX_OBJECTS','PARTICLE_SELECT',
     &        ' EXCEEDED MAXIMUM NUMBER OF JET TYPES ','F')
          ENDIF
        ELSE
          CALL ERRMSG(' NO_PARTICLE_SELECT_RCP','PARTICLE_SELECT',
     &        ' NO RCP file to work with ','F')
        ENDIF
      ENDIF
C
C ****  Check for MCDATA
C
      MCDATA = .FALSE.
      RECORD_TYPE = IQ(LHEAD+1)
      IF (RECORD_TYPE.GE.1005) MCDATA = .TRUE.
C
C ****  Select Electrons
C
C          sort banks so they are in increasing order of Et
      LPELC=GZPELC()
      CALL ZSORT(IXCOM,LPELC,7)
      LPELC=GZPELC()
      CALL ZTOPSY(IXCOM,LPELC)
      LPELC=GZPELC()
C
      DO JTYPE = 1, NELE_TYPE
        OBJECT = ELE_TYPE(JTYPE)
        NELEC = 0
        LPELC = GZPELC()
        DO WHILE (LPELC.GT.0)
          CALL CHECK_EM_QUALITY(LPELC, ELE_MASK(JTYPE), OK)
          IF(OK)THEN
            ET = Q(LPELC+7)
            ETA = Q(LPELC+9)
            PHI = Q(LPELC+10)
            IETA  = Q(LPELC+19)
            JER = 0
            IF(ET .LT. ELE_MIN_ET(JTYPE)) THEN
              IF (LOOP_OVER_VERTICES) THEN
                LZTRKPV = 0
                CALL EM_MVTXVAR(LPELC,LZTRKPV,ZVTXEM,ETZV,THZV)
                ETMAX = -999
                DO I=1,4
                  IF (ETZV(I).GT.ETMAX) THEN
                    ETMAX = ETZV(I)
                    IF(THZV(I) .NE.0) ETAMX=-ALOG(ABS(TAN(THZV(I)/2)))
                  ENDIF
                END DO
                IF (ETMAX.GT.ET) THEN
                  ET= ETMAX
                  ETA = ETAMX
                ENDIF
              ENDIF
            ENDIF
C- elike
            IF (ABS(IETA).LE.12) THEN
              ELE_MAX_LKLY = ELE_MAX_LKLY_CC(JTYPE)
              FHAD = 0.52
              MASK_CC = ELIKE_MASK_CC(JTYPE)
              DUMMY = ELIKE_SET_MASK_CC(MASK_CC)
              ELE_LKL = ELIKE(LPELC,FHAD,JER)
            ELSE
              ELE_MAX_LKLY = ELE_MAX_LKLY_EC(JTYPE)
              FHAD = 0.62
              MASK_EC = ELIKE_MASK_EC(JTYPE)
              DUMMY = ELIKE_SET_MASK_EC(MASK_EC)
              ELE_LKL = ELIKE(LPELC,FHAD,JER)
            ENDIF
            IF (JER.LT.0) ELE_LKL = 99999.0
C- option to turn of ELIKE for MC electrons
            IF (MCDATA.AND.TURN_OFF_ELIKE_MC) THEN
              ELE_LKL = -999.
            ENDIF
C- option to require match with MC electrons
            IF (MCDATA.AND.REQ_MCMATCH) THEN
              QMCMATCH = MATCHEM_MC_DATA(ETA,PHI)
              IF (.NOT.QMCMATCH) GOTO 100
            ENDIF
            IF(ET .GT. ELE_MIN_ET(JTYPE) .AND.
     &        ABS(ETA).LT.ELE_MAX_ETA(JTYPE).AND.
     &        ELE_LKL.LE.ELE_MAX_LKLY)THEN
              NELEC = NELEC + 1
              NLINKS = NLINKS + 1
              CALL GSLINK(OBJECT,OBJECT_LINK(NLINKS))
              LSLINK(OBJECT_LINK(NLINKS)) = LPELC
              LINK_LOC(NLINKS)=OBJECT_LINK(NLINKS)
              LINK_TYPE(NLINKS)=OBJECT
            ENDIF
          ENDIF
  100     CONTINUE
          LPELC = LQ(LPELC)
          IF (NELEC.GE.NELE_MAX)  THEN
            CALL ERRMSG(' MAX_ELECTRONS','PARTICLE_SELECT',
     &        ' exceeded maximum number of electron links ','W')
            LPELC = -1
          ENDIF
        END DO
      END DO
C
C ****  Select Photons
C
C          sort banks so they are in increasing order of Et
      LPPHO=GZPPHO()
      CALL ZSORT(IXCOM,LPPHO,7)
      LPPHO=GZPPHO()
      CALL ZTOPSY(IXCOM,LPPHO)
      LPPHO=GZPPHO()
C
      DO JTYPE = 1, NPHOT_TYPE
        OBJECT = PHOT_TYPE(JTYPE)
        NPHOT = 0
        LPPHO = GZPPHO()
        DO WHILE (LPPHO.GT.0)
          CALL CHECK_EM_QUALITY(LPPHO, PHOT_MASK(JTYPE), OK)
          IF(OK)THEN
            ET = Q(LPPHO+7)
            ETA = Q(LPPHO+9)
            PHI = Q(LPPHO+10)
            IF(ET .LT. PHOT_MIN_ET(JTYPE)) THEN
              IF (LOOP_OVER_VERTICES) THEN
                LZTRKPV = 0
                CALL EM_MVTXVAR(LPPHO,LZTRKPV,ZVTXEM,ETZV,THZV)
                ETMAX = -999
                DO I=1,4
                  IF (ETZV(I).GT.ETMAX) THEN
                    ETMAX = ETZV(I)
                    IF(THZV(I) .NE.0) ETAMX=-ALOG(ABS(TAN(THZV(I)/2)))
                  ENDIF
                END DO
                IF (ETMAX.GT.ET) THEN
                  ET= ETMAX
                  ETA = ETAMX
                ENDIF
              ENDIF
            ENDIF
            IF(ET .GT. PHOT_MIN_ET(JTYPE) .AND.
     &        ABS(ETA).LT.PHOT_MAX_ETA(JTYPE))THEN
C- option to require match with MC ELECTRONS (NOT photons!)
              IF (MCDATA.AND.REQ_MCMATCH) THEN
                QMCMATCH = MATCHEM_MC_DATA(ETA,PHI)
                IF (.NOT.QMCMATCH) GOTO 200
              ENDIF
              CALL CLEANEM(LPPHO,1,OK,STATUS)
              CALL CLEANEM_TQUANS(IVAR,TQUAN)
              NZTRK = TQUAN(1)               ! # tracks in road
              VAR = TQUAN(12)               ! track match significance
              IF (NZTRK.GT.0) THEN
                IF (VAR.LT.PHOT_TRKSIG(JTYPE)) THEN
                  GOTO 200
                ENDIF
              ENDIF
              NPHOT = NPHOT + 1
              NLINKS = NLINKS + 1
              CALL GSLINK(OBJECT,OBJECT_LINK(NLINKS))
              LSLINK(OBJECT_LINK(NLINKS)) = LPPHO
              LINK_LOC(NLINKS)=OBJECT_LINK(NLINKS)
              LINK_TYPE(NLINKS)=OBJECT
            ENDIF
          ENDIF
  200     CONTINUE
          LPPHO = LQ(LPPHO)
          IF (NPHOT.GE.NPHOT_MAX) THEN
            CALL ERRMSG(' MAX_PHOTONS','PARTICLE_SELECT',
     &        ' exceeded maximum number of photon links ','W')
            LPPHO = -1
          ENDIF
        END DO
      END DO
C
C ****  Select Muons
C
      LPMUO = GZPMUO(0)
      CALL ZSORT(IXCOM,LPMUO,14)
      LPMUO = GZPMUO(0)
      CALL ZTOPSY(IXCOM,LPMUO)
      LPMUO = GZPMUO(0)
      IF (USE_MUON_MCNAMES.AND.MCDATA) THEN
        DO JTYPE = 1, NMC_MUON_TYPE
          OBJECT = MC_MUON_TYPE(JTYPE)
          NMUON = 0
          LPMUO = GZPMUO(0)
          DO WHILE (LPMUO.GT.0)
            CALL CHECK_MU_QUALITY(LPMUO,MC_MUON_MASK(JTYPE), OK)
            IF(OK)THEN
              PT = Q(LPMUO+14)
              IF( IQ(LPMUO+7).LE.4 ) THEN           ! CF
                IRGN = 1
              ELSE IF( IQ(LPMUO+7).LE.12 ) THEN     ! Pure EF
                IRGN=2
                LMUOT=LQ(LPMUO-2)
                IF (LMUOT.GT.0) THEN
                  IFW2 = IQ(LMUOT+5)
                  IF (BTEST(IFW2,10)) IRGN=4        ! SSW
                  IF (BTEST(IFW2,11)) IRGN=3        ! SWW
                ENDIF
              ELSE
                IRGN=5                              ! Samus
              ENDIF
              IF(PT .GT. MC_MUON_MIN_PT(JTYPE) .AND.
     &          IRGN.LE.MC_MUON_MAX_RGN(JTYPE))THEN
                NMUON = NMUON + 1
                NLINKS = NLINKS + 1
                CALL GSLINK(OBJECT,OBJECT_LINK(NLINKS))
                LSLINK(OBJECT_LINK(NLINKS)) = LPMUO
                LINK_LOC(NLINKS)=OBJECT_LINK(NLINKS)
                LINK_TYPE(NLINKS)=OBJECT
              ENDIF
            ENDIF
            LPMUO = LQ(LPMUO)
            IF (NMUON.GE.NMUON_MAX) THEN
              CALL ERRMSG(' MAX_MUONS','PARTICLE_SELECT',
     &          ' exceeded maximum number of muon links ','W')
              LPMUO = -1
            ENDIF
          END DO
        END DO
      ELSE
        DO JTYPE = 1, NMUON_TYPE
          OBJECT = MUON_TYPE(JTYPE)
          NMUON = 0
          LPMUO = GZPMUO(0)
          DO WHILE (LPMUO.GT.0)
            CALL CHECK_MU_QUALITY(LPMUO, MUON_MASK(JTYPE), OK)
            IF(OK)THEN
              PT = Q(LPMUO+14)
              IF( IQ(LPMUO+7).LE.4 ) THEN           ! CF
                IRGN = 1
              ELSE IF( IQ(LPMUO+7).LE.12 ) THEN     ! Pure EF
                IRGN=2
                LMUOT=LQ(LPMUO-2)
                IF (LMUOT.GT.0) THEN
                  IFW2 = IQ(LMUOT+5)
                  IF (BTEST(IFW2,10)) IRGN=4        ! SSW
                  IF (BTEST(IFW2,11)) IRGN=3        ! SWW
                ENDIF
              ELSE
                IRGN=5                              ! Samus
              ENDIF
              IF(PT .GT. MUON_MIN_PT(JTYPE) .AND.
     &          IRGN.LE.MUON_MAX_RGN(JTYPE))THEN
                NMUON = NMUON + 1
                NLINKS = NLINKS + 1
                CALL GSLINK(OBJECT,OBJECT_LINK(NLINKS))
                LSLINK(OBJECT_LINK(NLINKS)) = LPMUO
                LINK_LOC(NLINKS)=OBJECT_LINK(NLINKS)
                LINK_TYPE(NLINKS)=OBJECT
              ENDIF
            ENDIF
            LPMUO = LQ(LPMUO)
            IF (NMUON.GE.NMUON_MAX) THEN
              CALL ERRMSG(' MAX_MUONS','PARTICLE_SELECT',
     &          ' exceeded maximum number of muon links ','W')
              LPMUO = -1
            ENDIF
          END DO
        END DO
      ENDIF
C
C ****  Select jets : First the jet algorithm used is set.  Then links to all
C ****                jets passing the kinematic cuts are kept as long as they
C ****                are not within JET_MIN_DELTA_R of good EM objects defined
C ****                in REMOVE_ELEC_JETS_TYPE and REMOVE_PHOTON_JETS_TYPE.
      DO K = 1,NJET_TYPE
        CALL SET_CAPH_ALG(JET_ALGORITHM(K))
        LJETS = GZJETS()
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS = GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS = GZJETS()
        DO JTYPE=1, NREMOVE_ELEC_JETS_TYPE
          CALL GTSLINK(REMOVE_ELEC_JETS_TYPE(JTYPE),NELE_MAX,
     &      NELE_TOTAL(JTYPE),ELE_LINKS(1,JTYPE))
        ENDDO
        DO JTYPE=1, NREMOVE_PHOTON_JETS_TYPE
          CALL GTSLINK(REMOVE_PHOTON_JETS_TYPE(JTYPE),NPHOT_MAX,
     &      NPHOT_TOTAL(JTYPE),PHOT_LINKS(1,JTYPE))
        ENDDO
        NJET = 0
        ELE_PHI = 0.
        PHOT_PHI = 0.
        JET_PHI = 0.
        ELE_ETA = 0.
        PHOT_ETA = 0.
        JET_ETA = 0.
        OBJECT = JET_TYPE(K)
        LJETS = GZJETS()
        DO WHILE (LJETS.GT.0)
          MATCH = .FALSE.
          NL = IQ(LJETS-3)
          NS = IQ(LJETS-2)
          JET_PHI = Q(LJETS + 8)
          JET_ETA = Q(LJETS + 9)
          DO JTYPE=1,NREMOVE_ELEC_JETS_TYPE
            DO J = 1,NELE_TOTAL(JTYPE)
              ELE_PHI = Q(ELE_LINKS(J,JTYPE) + 10)
              ELE_ETA = Q(ELE_LINKS(J,JTYPE) + 9)
              DELTA_PHI = ABS(JET_PHI - ELE_PHI)
              IF (DELTA_PHI.GT.PI) DELTA_PHI = 2.*PI - DELTA_PHI
              DELTA_R = SQRT(DELTA_PHI**2.+(JET_ETA-ELE_ETA)**2.)
              IF (DELTA_R.LT.JET_MIN_DELTA_R(K)) MATCH=.TRUE.
            ENDDO
          ENDDO
          DO JTYPE=1,NREMOVE_PHOTON_JETS_TYPE
            DO J = 1,NPHOT_TOTAL(JTYPE)
              PHOT_PHI = Q(PHOT_LINKS(J,JTYPE) + 10)
              PHOT_ETA = Q(PHOT_LINKS(J,JTYPE) + 9)
              DELTA_PHI = ABS(JET_PHI - PHOT_PHI)
              IF (DELTA_PHI.GT.PI) DELTA_PHI = 2.*PI - DELTA_PHI
              DELTA_R = SQRT(DELTA_PHI**2.+(JET_ETA-PHOT_ETA)**2.)
              IF (DELTA_R.LT.JET_MIN_DELTA_R(K)) MATCH=.TRUE.
            ENDDO
          ENDDO
          IF(.NOT.MATCH)THEN
            ET = Q(LJETS+6)
            ETA = Q(LJETS+9)
            IF ((ET.GT.JET_MIN_ET(K)).AND.(ABS(ETA).LT.JET_MAX_ETA(K)))
     &            THEN
              NJET = NJET + 1
              NLINKS = NLINKS + 1
              CALL GSLINK(OBJECT,OBJECT_LINK(NLINKS))
              LSLINK(OBJECT_LINK(NLINKS)) = LJETS
              LINK_LOC(NLINKS)=OBJECT_LINK(NLINKS)
              LINK_TYPE(NLINKS)=OBJECT
            ENDIF
          ENDIF
          LJETS = LQ(LJETS)
          IF (NJET.GE.NJET_MAX) THEN
            CALL ERRMSG(' MAX_JETS','PARTICLE_SELECT',
     &        ' EXCEEDED MAXIMUM NUMBER OF JETS LINKS ','W')
            LJETS = -1
          ENDIF
        ENDDO
        CALL RESET_CAPH
      ENDDO

      GOTO 999
C#######################################################################
      ENTRY PARTICLE_SELECT_FIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Release all links reserved in ZLINKA
C-
C-   Created  25-JAN-1994   Meenakshi Narain
C-
C----------------------------------------------------------------------
      PARTICLE_SELECT_FIN=.TRUE.
      DO I=1,NLINKS
        CALL RSLINK(LINK_TYPE(I),LINK_LOC(I))
      END DO
  999 RETURN
      END

