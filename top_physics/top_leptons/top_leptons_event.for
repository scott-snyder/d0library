      FUNCTION TOP_LEPTONS_EVENT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Event steering routine for top analysis
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  28-FEB-1992   Stephen J. Wimpenny
C-
C-   Modified 13-JUL-1992   jc -  to add NTUPLE filling
C-   Modified 15-JUL-1992   muon,electron,photon,jet selection added
C-   Modified 20-JUL-1992   ENTRY TOP_LEPTONS_END() added to do all
C-                          end-of-run stuff
C-   Modified  3-AUG-1992   Histogramming and Ntuple fills added on
C-                          RCP switches. Crude W-finder inplemented.
C-   Modified 17-Aug-1992   Changed to allow parallel Histogramming
C-                          and Ntuple filling
C-   Modified 15-Sep-1992   Direct RCP reads added
C-   Updated  24-SEP-1992   Event writeout flag added
C-   Modified 18-Oct-1992   Top finder switch added
C-   Modified  1-Nov-1992   Cuts on Primary Vertex Information added
C-   Modified  2-Nov-1992   Bad Run Checking Added
C-   Modified 13-Nov-1992   Lepton+Jets ttbar search flags added
C-   Modified  4-Dec-1992   Level 1 Trigger Nos added
C-   Modified 29-Jan-1993   Dummy Z-finder hook added
C-   Modified  1-Feb-1993   WWGamma finder switched to stand-alone
C-   Modified 12-Feb-1993   Specific Event Fixup hook added
C-   Modified 16-Mar-1993   Good_Vertex,Good_Run,Good_Trigger logicals
C-                          changed
C-   Modified 25-Mar-1993   Top_Leptons_Event function set .TRUE. only if
C-                          event passes at least one set of finder cuts
C-   Modified 29-Mar-1993   LUN 14 daignostic summary added for WWG and Z
C-                          finders
C-   Modified  3-Apr-1993   QCD Finder hook added for background studies
C-   Modified 18-May-1993   Wpair finder hook added
C-   Modified 16-Jun-1993   Structure for Etmiss corrections added
C-   Modified 27-Jul-1993   Muon select and Jet select intercahnged to
C-                          allow muon code to access reduced jet list
C-                          for dE/dx corrections.
C-   Modified 20-Nov-1993   em-Et correction logic absorbed into jet corrections
C-                          Jet Et definition changed
C-   Modified 4-Dec-1993    call to Top_Finder modified to allow a call
C-                          even if no lepton is found (for background
C-                          analysis). Call to QCD_Finder modified to allow
C-                          entry with no cut on no. of leptons.    
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_TRIGGER,TOP_LEPTONS_GOOD_VERTEX
      EXTERNAL TOP_LEPTONS_GOOD_RUN
C
      LOGICAL TOP_LEPTONS_EVENT,LONG_EVENT_DUMP
      LOGICAL FIRST,PRINT_LEPSUM,DUMP_EVENT
      LOGICAL DIAGNOSTIC_DUMPS,FIX_SPECIAL_EVENTS,ANALYSIS_TEST
      LOGICAL TOP_LEPTONS_END,DO_EVENT_WRITE,WRITE_DST,WRITE_STA
      LOGICAL DO_FINDW,DO_FINDZ,DO_FINDTOP,DO_FINDWWG,DO_FINDQCD
      LOGICAL DO_NTUPLES,DO_HISTOGRAMS,DO_FINDWPAIR
      LOGICAL OK,CORR_JETS
      LOGICAL TOP_LEPTONS_GOOD_TRIGGER,TOP_LEPTONS_GOOD_VERTEX
      LOGICAL TOP_LEPTONS_GOOD_RUN
C
      INTEGER LEPTONS_LUNSUM,LEPTONS_LUNERR,LUNEVT
      INTEGER LENGTH,DUMP_LIMIT,TEST_LIMIT
      INTEGER TOP_FLAG(6),W_FLAG(3),Z_FLAG(3),WWG_FLAG(3),QCD_FLAG(6)
      INTEGER WPAIR_FLAG(6)
      INTEGER NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT
      INTEGER NOJT,NOJT_UNCUT,JET_ALG,GZHEAD
      INTEGER NO_EVT,NO_RUN,NO_EVT_L1PT1,NO_EVT_L1PT2,NO_EVT_LOCAL
      INTEGER ITEST,LUN,IER,IDUMP,IV,IOUT,NDUM,IDUMP2
      INTEGER IKEEP_TT,IKEEP_WW,IKEEP_TOP(5),IKEEP_W(2),IKEEP_NO_T
      INTEGER IKEEP_Z(2),IKEEP_ZZ,IKEEP_NO_T2
      INTEGER IKEEP_QCD(5),IKEEP_QCDD,IKEEP_NO_T4
      INTEGER IKEEP_WWG(2),IKEEP_WWWG,IKEEP_NO_T3
      INTEGER IKEEP_WWPAIR,IKEEP_WPAIR(5),IKEEP_NO_T5
C
      REAL VDUM,MET_VEC(3),MET_DUM(3)
      REAL CONE_TEMPLATE_7(3),CONE_TEMPLATE_5(3)
      REAL CONE_TEMPLATE_3(3),NN_TEMPLATE(5)
C
      CHARACTER*8 CDUM
      CHARACTER*80 OUTPUT_FILE
      CHARACTER*3 EVENT_STREAM
      CHARACTER*1 MODE
C
      DATA FIRST,NO_EVT_LOCAL/.TRUE.,0/
      DATA IKEEP_TT,IKEEP_WW,IKEEP_TOP,IKEEP_W/0, 0, 0,0,0,0,0, 0,0/
      DATA IKEEP_Z,IKEEP_ZZ/ 0,0, 0/
      DATA IKEEP_WWG,IKEEP_WWWG/ 0,0, 0/
      DATA IKEEP_QCD,IKEEP_QCDD,IKEEP_NO_T3/ 0,0,0,0,0, 0, 0/
      DATA IKEEP_NO_T,IKEEP_NO_T2,IKEEP_NO_T3,IDUMP2/ 0,0,0,0/
      DATA CONE_TEMPLATE_7/ 1., 6., 0.7/
      DATA CONE_TEMPLATE_5/ 1., 6., 0.5/
      DATA CONE_TEMPLATE_3/ 1., 6., 0.3/
      DATA NN_TEMPLATE    / 2., 7., 2., 8., 2./
      DATA CDUM,MODE /'DUMMY','X'/
      REAL    x(3)
C
C *** Define HBook Directory Path
C
      CALL DHDIR('TOP_LEPTONS_RCP','HBOOK_DIRECTORY',IER,' ')
      IDUMP=0
C
C *** Get cuts for this run
C
      IF(FIRST) THEN
        IER = 0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
C *** Job steering and option information
C
        CALL EZGET_l('PRINT_CUTSUM',PRINT_LEPSUM,IER)
        IF (IER.EQ.0) CALL EZGET_i('PRINT_LUN',LEPTONS_LUNSUM,IER)
        IF (IER.EQ.0) CALL EZGET_i('ERROR_LUN',LEPTONS_LUNERR,IER)
        IF (IER.EQ.0) CALL EZGET_l('PLOT_HISTOGRAMS',DO_HISTOGRAMS,IER)
        IF (IER.EQ.0) CALL EZGET_l('FILL_NTUPLES',DO_NTUPLES,IER)
C
C *** Enable writing of selected events
C
        IF(IER.EQ.0) CALL EZGET_l('WRITE_EVENTS_OUT',DO_EVENT_WRITE,IER)
        IF(DO_EVENT_WRITE) THEN
          IF(IER.EQ.0) CALL EZGET_l('WRITE_STA_FILE',WRITE_STA,IER)
          IF(IER.EQ.0) CALL EZGET_l('WRITE_DST_FILE',WRITE_DST,IER)
          IF(WRITE_STA) THEN
            IF(IER.EQ.0)
     1         CALL EZGETS('OUTPUT_STA',1,OUTPUT_FILE,LENGTH,IER)
            EVENT_STREAM='STA'
          ENDIF
          IF(WRITE_DST) THEN
            IF(WRITE_STA) CALL ERRMSG('DST and STA write requested',
     1        'TOP_LEPTONS_EVENT',' ','F')
            IF(IER.EQ.0)
     1         CALL EZGETS('OUTPUT_DST',1,OUTPUT_FILE,LENGTH,IER)
            EVENT_STREAM='DST'
          ENDIF
        ENDIF
C
C *** Event fixup/repair for special events
C
        IF(IER.EQ.0) CALL EZGET_l('DO_EVENT_FIX',FIX_SPECIAL_EVENTS,IER)
C
C *** Event dump enable/disable
C
        IF (IER.EQ.0) CALL EZGET_l('DO_DUMPS',DUMP_EVENT,IER)
C
C *** enable/disable full length dumps
C
        IF (IER.EQ.0) CALL EZGET_l('LONG_DUMPS',LONG_EVENT_DUMP,IER)
C
C *** Force dump of all events (for program diagnostic tests)
C
        IF (IER.EQ.0) CALL EZGET('DO_DIAGNOSTIC_DUMPS',DIAGNOSTIC_DUMPS,
     &    IER)
        IF (IER.EQ.0) CALL EZGET_i('NO_EVT_DIAGNOSTICS',DUMP_LIMIT,IER)
C
C *** Analysis Test Mode ( for finder code diagnostic testing )
C
        IF (IER.EQ.0) CALL EZGET_l('DO_ANAL_TEST',ANALYSIS_TEST,
     &    IER)
        IF (IER.EQ.0) CALL EZGET_i('NO_EVT_TEST',TEST_LIMIT,IER)
C
C *** W-finder
C
        IF (IER.EQ.0) CALL EZGET('FINDW_CAND',DO_FINDW,IER)
C
C *** Z0 finder
C
        IF (IER.EQ.0) CALL EZGET('FINDZ_CAND',DO_FINDZ,IER)
C
C *** WWgamma finder
C
        IF (IER.EQ.0) CALL EZGET('FINDWWG_CAND',DO_FINDWWG,IER)
C
C *** ttbar-finder
C
        IF (IER.EQ.0) CALL EZGET('FINDTOP_CAND',DO_FINDTOP,IER)
C
C *** ttbar/QCD background finder
C
        IF (IER.EQ.0) CALL EZGET('FINDQCD_CAND',DO_FINDQCD,IER)
C
C *** ttbar/WPair background finder.
C
        IF (IER.EQ.0) CALL EZGET('FINDWPAIR_CAND',DO_FINDWPAIR,IER)
C
C *** finish finder selection
C
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_EVENT',' ','F')
C
C *** Jet energy corrections
C
        IF (IER.EQ.0) CALL EZGET('JETS_CORR',CORR_JETS,IER)
C
C *** Jet algorithm
C
        CALL EZGET_i('JETS_ALGORITHM',JET_ALG,IER)
C
        CALL EZRSET
C
        IF(PRINT_LEPSUM) THEN
          CALL TOP_LEPTONS_INIT_SUMMRY
        ENDIF
C
C *** Initialise Event write
C
        IF(DO_EVENT_WRITE) THEN
          CALL EVOPWO(EVENT_STREAM,OUTPUT_FILE,MODE,OK)
          IF(.NOT.OK) CALL ERRMSG('Output filestream open failure',
     1      'TOP_LEPTONS_EVENT',' ','F')
        ENDIF
C
        FIRST=.FALSE.
C
      ENDIF
C
C *** Set write flags to .FALSE. to prevent DST/STA write
C
      CALL FLGSET('WRITE_STREAM_DST',.FALSE.)
      CALL FLGSET('WRITE_STREAM_STA',.FALSE.)
C
C
C *** Set Function to .FALSE. to prevent further event processing
C
      TOP_LEPTONS_EVENT=.FALSE.
C
C *** Initialize Etmiss Correction Array
C
      CALL VZERO(MET_VEC,3)
C
C *** Program Diagnostic mode -> skip all selection and go straight to
C *** event dumps. Also check on no of events wnated
C
      IF(DIAGNOSTIC_DUMPS) THEN
        IDUMP2=IDUMP2+1
        IF(IDUMP2.LT.DUMP_LIMIT) THEN
          IDUMP=1
          NOJT=0
          NOJT_UNCUT=0
          CALL TOP_LEPTONS_JET_ET_DEF
          CALL TOP_LEPTONS_JET_SELECT(NOEL,NOPH,NOJT,NOJT_UNCUT,met_vec)
C
C *** Re-set Jet Algorithm to analysis default
C
          IF(JET_ALG.EQ.1) THEN
            CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_7,IER)
          ELSEIF(JET_ALG.EQ.2) THEN
            CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_5,IER)
          ELSEIF(JET_ALG.EQ.3) THEN
            CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_3,IER)
          ELSEIF(JET_ALG.EQ.4) THEN
            CALL SET_CAPH('NN_JET',NN_TEMPLATE,IER)
          ELSE
            WRITE(12,1200) JET_ALG
            CALL ERRMSG('Illegal Jet Algorithm',
     1        'TOP_LEPTONS_EVENT',' ','F')
          ENDIF
          GO TO 100
        ELSE
          STOP
        ENDIF
      ENDIF
C
C *** define local event number and get D0 run and event nos
C
      LUN=LEPTONS_LUNSUM
      NO_EVT_LOCAL=NO_EVT_LOCAL+1
      NO_EVT=-9999
      NO_RUN=-9999
      NO_EVT_L1PT1=-9999
      NO_EVT_L1PT2=-9999
      LHEAD=GZHEAD()
      IF(LHEAD.NE.0) THEN
        NO_RUN=IQ(LHEAD+6)
        NO_EVT_L1PT1=IQ(LHEAD+7)
        NO_EVT_L1PT2=IQ(LHEAD+8)
        NO_EVT=IQ(LHEAD+9)
      ENDIF
C
C *** If running in analysis test mode -> check local event no.
C *** against run limit
C
      IF(ANALYSIS_TEST) THEN
        WRITE(LUN,2000) NO_EVT_LOCAL,NO_EVT,NO_RUN
        IF(NO_EVT_LOCAL.GT.TEST_LIMIT) STOP
      ENDIF
C
C *** Fixup for special events
C
      IF(FIX_SPECIAL_EVENTS) THEN
        CALL TOP_LEPTONS_EVENT_FIX(NO_RUN,NO_EVT,NO_EVT_L1PT1)
      ENDIF
C
C *** Test for Bad Runs
C
      IF(.NOT.TOP_LEPTONS_GOOD_RUN(NO_RUN)) THEN
        GO TO 999
      ENDIF        
C
C *** Check that we have a valid trigger bit/filter set for this event
C
      IF(.NOT.TOP_LEPTONS_GOOD_TRIGGER()) THEN
        GO TO 999
      ENDIF
C
C *** Check that we have valid Vertex information
C
      IF(.NOT.TOP_LEPTONS_GOOD_VERTEX()) THEN
        GO TO 999
      ENDIF
C
C *** Preliminary lepton and jet selection
C ***  Note :
C ***     the electron and photon selection should be called before
C ***     TOP_LEPTONS_JET_SELECT because it uses the results to remove
C ***     selected electron and photon candidates from the JETS banks
C
      NOEL=0
      NOEL_UNCUT=0
      CALL TOP_LEPTONS_ELECTRON_SELECT(NOEL,NOEL_UNCUT)
C
      NOPH=0
      NOPH_UNCUT=0
      CALL TOP_LEPTONS_PHOTON_SELECT(NOPH,NOPH_UNCUT)
C
C *** Fixup JETS definition of Jet Et
C
      CALL TOP_LEPTONS_JET_ET_DEF
C
C *** Calculate Missing Et change due to em and jet scale corrections
C
      IF(CORR_JETS) THEN
        CALL TOP_LEPTONS_JET_ETM_CORR(MET_DUM)
        CALL VADD(MET_DUM(1),MET_VEC(1),MET_VEC(1),3)
      ENDIF
C
C *** Jet selection
C
      NOJT=0
      NOJT_UNCUT=0
      CALL TOP_LEPTONS_JET_SELECT(NOEL,NOPH,NOJT,NOJT_UNCUT,MET_VEC)
C
C *** Re-set Jet Algorithm to analysis default
C
      IF(JET_ALG.EQ.1) THEN
        CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_7,IER)
      ELSEIF(JET_ALG.EQ.2) THEN
        CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_5,IER)
      ELSEIF(JET_ALG.EQ.3) THEN
        CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_3,IER)
      ELSEIF(JET_ALG.EQ.4) THEN
        CALL SET_CAPH('NN_JET',NN_TEMPLATE,IER)
      ELSE
        WRITE(12,1200) JET_ALG
        CALL ERRMSG('Illegal Jet Algorithm',
     1    'TOP_LEPTONS_EVENT',' ','F')
      ENDIF
C
C *** Muon Selection
C
      NOMU=0
      NOMU_UNCUT=0
      CALL TOP_LEPTONS_MUON_SELECT(NOMU,NOMU_UNCUT,NOJT)
C
C *** Rebuild PNUT3 using only muons accepted by selection routine
C
      CALL TOP_LEPTONS_REBUILD_PNUT3(MET_VEC)
C
C *** Go to Phyiscs selection code
C
      CALL VZERO_i(TOP_FLAG,6)
      IF(DO_FINDTOP) THEN
        ITEST=NOMU+NOEL+NOPH
        IF(ITEST.GE.0) THEN
          CALL TOP_LEPTONS_FINDTOP(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     &      NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,TOP_FLAG,MET_VEC)
          IF(TOP_FLAG(1).NE.0) THEN
            IKEEP_TT=IKEEP_TT+1
            IF(TOP_FLAG(2).NE.0) THEN
              IKEEP_TOP(1)=IKEEP_TOP(1)+1
            ENDIF
            IF(TOP_FLAG(3).NE.0) THEN
              IKEEP_TOP(2)=IKEEP_TOP(2)+1
            ENDIF
            IF(TOP_FLAG(4).NE.0) THEN
              IKEEP_TOP(3)=IKEEP_TOP(3)+1
            ENDIF
            IF(TOP_FLAG(5).NE.0) THEN
              IKEEP_TOP(4)=IKEEP_TOP(4)+1
            ENDIF
            IF(TOP_FLAG(6).NE.0) THEN
              IKEEP_TOP(5)=IKEEP_TOP(5)+1
            ENDIF
            IDUMP=IDUMP+1
          ENDIF
        ENDIF
      ENDIF
C
C *** Now do W-selection
C
      CALL VZERO_i(W_FLAG,3)
      IF(DO_FINDW) THEN
        CALL TOP_LEPTONS_FINDW(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     &    NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,W_FLAG,MET_VEC)
C
       IF(W_FLAG(1).NE.0) THEN
          IKEEP_WW=IKEEP_WW+1
          IF(W_FLAG(2).NE.0) THEN
            IKEEP_W(1)=IKEEP_W(1)+1
          ENDIF
          IF(W_FLAG(3).NE.0) THEN
            IKEEP_W(2)=IKEEP_W(2)+1
          ENDIF
          IF(TOP_FLAG(1).EQ.0) IKEEP_NO_T=IKEEP_NO_T+1
          IDUMP=IDUMP+1
        ENDIF
      ENDIF
C
C *** Next do Z0-selection
C
      CALL VZERO_i(Z_FLAG,3)
      IF(DO_FINDZ) THEN
        CALL TOP_LEPTONS_FINDZ(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     &    NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,Z_FLAG,MET_VEC)
C
        IF(Z_FLAG(1).NE.0) THEN
          IKEEP_ZZ=IKEEP_ZZ+1
          IF(Z_FLAG(2).NE.0) THEN
            IKEEP_Z(1)=IKEEP_Z(1)+1
          ENDIF
          IF(Z_FLAG(3).NE.0) THEN
            IKEEP_Z(2)=IKEEP_Z(2)+1
          ENDIF
          IF(TOP_FLAG(1).EQ.0) 
     1      IKEEP_NO_T2=IKEEP_NO_T2+1
          IDUMP=IDUMP+1
        ENDIF
      ENDIF
C
C *** Next do WWgamma-selection
C
      CALL VZERO_i(WWG_FLAG,3)
      IF(DO_FINDWWG) THEN
        CALL TOP_LEPTONS_FINDWWG(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     &    NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,WWG_FLAG,MET_VEC)
C
       IF(WWG_FLAG(1).NE.0) THEN
          IKEEP_WWWG=IKEEP_WWWG+1
          IF(WWG_FLAG(2).NE.0) THEN
            IKEEP_WWG(1)=IKEEP_WWG(1)+1
          ENDIF
          IF(WWG_FLAG(3).NE.0) THEN
            IKEEP_WWG(2)=IKEEP_WWG(2)+1
          ENDIF
          IF(TOP_FLAG(1).EQ.0) 
     1      IKEEP_NO_T3=IKEEP_NO_T3+1
          IDUMP=IDUMP+1
        ENDIF
      ENDIF
C
C *** Next Do Top/QCD background selection
C
      CALL VZERO_i(QCD_FLAG,6)
      IF(DO_FINDQCD) THEN
        ITEST=NOMU+NOEL+NOPH
        IF(ITEST.GE.0) THEN
          CALL TOP_LEPTONS_FINDQCD(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     &      NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,QCD_FLAG,MET_VEC)
          IF(QCD_FLAG(1).NE.0) THEN
            IKEEP_QCDD=IKEEP_QCDD+1
            IF(QCD_FLAG(2).NE.0) THEN
              IKEEP_QCD(1)=IKEEP_QCD(1)+1
            ENDIF
            IF(QCD_FLAG(3).NE.0) THEN
              IKEEP_QCD(2)=IKEEP_QCD(2)+1
            ENDIF
            IF(QCD_FLAG(4).NE.0) THEN
              IKEEP_QCD(3)=IKEEP_QCD(3)+1
            ENDIF
            IF(QCD_FLAG(5).NE.0) THEN
              IKEEP_QCD(4)=IKEEP_QCD(4)+1
            ENDIF
            IF(QCD_FLAG(6).NE.0) THEN
              IKEEP_QCD(5)=IKEEP_QCD(5)+1
            ENDIF
            IF(TOP_FLAG(1).EQ.0) 
     1        IKEEP_NO_T4=IKEEP_NO_T4+1
            IDUMP=IDUMP+1
          ENDIF
        ENDIF
      ENDIF
C
C *** Now do WPAIR - selection
C
      CALL VZERO_i(WPAIR_FLAG,6)
      IF(DO_FINDWPAIR) THEN
        ITEST=NOMU+NOEL+NOPH
        IF(ITEST.GE.1) THEN
          CALL TOP_LEPTONS_FIND_WPAIR(NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,
     &      NOPH,NOPH_UNCUT,NOJT,NOJT_UNCUT,WPAIR_FLAG,MET_VEC)
          IF(WPAIR_FLAG(1).NE.0) THEN
            IKEEP_WWPAIR=IKEEP_WWPAIR+1
            IF(WPAIR_FLAG(2).NE.0) THEN
              IKEEP_WPAIR(1)=IKEEP_WPAIR(1)+1
            ENDIF
            IF(WPAIR_FLAG(3).NE.0) THEN
              IKEEP_WPAIR(2)=IKEEP_WPAIR(2)+1
            ENDIF
            IF(WPAIR_FLAG(4).NE.0) THEN
              IKEEP_WPAIR(3)=IKEEP_WPAIR(3)+1
            ENDIF
            IF(WPAIR_FLAG(5).NE.0) THEN
              IKEEP_WPAIR(4)=IKEEP_WPAIR(4)+1
            ENDIF
            IF(WPAIR_FLAG(6).NE.0) THEN
              IKEEP_WPAIR(5)=IKEEP_WPAIR(5)+1
            ENDIF
            IF(TOP_FLAG(1).EQ.0) 
     1        IKEEP_NO_T5=IKEEP_NO_T5+1
            IDUMP=IDUMP+1
          ENDIF
        ENDIF
      ENDIF
C
C *** Histogramming
C
      IF(DO_HISTOGRAMS) THEN
        IF(IDUMP.GT.0) CALL TOP_LEPTONS_HIST_FILL(TOP_FLAG,W_FLAG,
     1    Z_FLAG,WWG_FLAG,QCD_FLAG,WPAIR_FLAG,MET_VEC)
      ENDIF
C
C *** Ntuples
C
      IF(DO_NTUPLES) THEN
       NDUM = 0
        VDUM = 1.
        IF(IDUMP.GT.0) CALL TOP_LEPTONS_NTUPLE(500,CDUM,VDUM,NDUM,
     1    MET_VEC)
      ENDIF
C
C *** Event Dumps
C
  100 CONTINUE
C
      IF(DUMP_EVENT.OR.DIAGNOSTIC_DUMPS) THEN
        IF(IDUMP.GT.0) CALL TOP_LEPTONS_EVENT_SUMMRY(NO_EVT_LOCAL,
     1    NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT,NOJT,
     2    NOJT_UNCUT,TOP_FLAG,W_FLAG,Z_FLAG,WWG_FLAG,QCD_FLAG,
     3    WPAIR_FLAG,MET_VEC)
      ENDIF
C
C *** Write event out - if selected
C
      IF(DO_EVENT_WRITE) THEN
        IF(IDUMP.GT.0) THEN
          IF(WRITE_DST) THEN
            CALL FLGSET('WRITE_STREAM_DST',.TRUE.)
          ELSEIF(WRITE_STA) THEN
            CALL FLGSET('WRITE_STREAM_STA',.TRUE.)
          ENDIF
        ENDIF
      ENDIF
C
C *** One line summary for selected events
C
      LUNEVT=14
      IOUT=TOP_FLAG(1)+W_FLAG(1)+Z_FLAG(1)+WWG_FLAG(1)+QCD_FLAG(1)
     1 +WPAIR_FLAG(1)
      IF(IOUT.GT.0) THEN
        WRITE(LUNEVT,1100)NO_RUN,NO_EVT,NO_EVT_L1PT1,
     1     NO_EVT_L1PT2,noel,nomu,noph,nojt
        TOP_LEPTONS_EVENT=.TRUE.
      ENDIF
C
C *** Re-set Jet Algorithm for next event
C
      CALL RESET_CAPH
C
      GO TO 999
C
C *** Job ternmination summary + close files
C
      ENTRY TOP_LEPTONS_END()
C
      TOP_LEPTONS_END=.TRUE.
C
C *** call TOP_LEPTONS_NTUPLES for last time - to fill final event
C
      CALL TOP_LEPTONS_NTUPLE(-1,'DUMMY',1.,0,X)
C
C *** Close Ntuples
C
      IF(DO_NTUPLES) THEN
        CALL NTUPLE_END
      ENDIF
C
C *** Termination summary
C
      IF(PRINT_LEPSUM) THEN
        WRITE(LUN,1000) NO_EVT_LOCAL
        WRITE(LUN,1010) IKEEP_TT,(IKEEP_TOP(IV),IV=1,5)
        WRITE(LUN,1020) IKEEP_WW,IKEEP_NO_T,(IKEEP_W(IV),IV=1,2)
        WRITE(LUN,1030) IKEEP_ZZ,IKEEP_NO_T2,(IKEEP_Z(IV),IV=1,2)
        WRITE(LUN,1040) IKEEP_WWWG,IKEEP_NO_T3,(IKEEP_WWG(IV),IV=1,2)
        WRITE(LUN,1050) IKEEP_QCDD,IKEEP_NO_T4,(IKEEP_QCD(IV),IV=1,5)
        WRITE(LUN,1060) IKEEP_WWPAIR,IKEEP_NO_T5,
     1        (IKEEP_WPAIR(IV),IV=1,5)
      ENDIF
 999  RETURN
C----------------------------------------------------------------------
 1000 FORMAT(1H1,//,10X,'TOP_LEPTONS Job Termination Summary',
     1 /10X,35(1H-),//,
     2 5X,' No of events read                   = ',I8)
 1010 FORMAT(/,10X,' a.) Top selection                : ',//,
     1 5X,' No of events selected               = ',I5,/,
     2 5X,' No of ttbar->emu candidates         = ',I5,/,
     3 5X,' No of ttbar->ee candidates          = ',I5,/,
     4 5X,' No of ttbar->mumu candidates        = ',I5,/,
     5 5X,' No of ttbar->e+jets candidates      = ',I5,/,
     6 5X,' No of ttbar->mu+jets candidates     = ',I5)
 1020 FORMAT(/,10X,' b.) W selection                  : ',//,
     1 5X,' No of events selected               = ',I5,/,
     2 5X,' No not found by top selection       = ',I5,/,
     3 5X,' No of W->enu candidates             = ',I5,/,
     4 5X,' No of W->munu candidates            = ',I5)
 1030 FORMAT(/,10X,' c.) Z0 selection                 : ',//,
     1 5X,' No of events selected               = ',I5,/,
     2 5X,' No not found by top selection       = ',I5,/,
     3 5X,' No of Z->ee candidates              = ',I5,/,
     4 5X,' No of Z->munu candidates            = ',I5)
 1040 FORMAT(/,10X,' d.) WWgamma selection            : ',//,
     1 5X,' No of events selected               = ',I5,/,
     2 5X,' No not found by top selection       = ',I5,/,
     3 5X,' No of W->enu+gamma candidates       = ',I5,/,
     4 5X,' No of W->munu+gamma candidates      = ',I5)
 1050 FORMAT(/,10X,' e.) Top/QCD background selection : ',//,
     1 5X,' No of events selected               = ',I5,/,
     2 5X,' No not found by top selection       = ',I5,/,
     3 5X,' No of QCD->emu candidates           = ',I5,/,
     4 5X,' No of QCD->ee candidates            = ',I5,/,
     5 5X,' No of QCD->mumu candidates          = ',I5,/,
     6 5X,' No of QCD->e+jets candidates        = ',I5,/,
     7 5X,' No of QCD->mu+jets candidates       = ',I5)
 1060 FORMAT(/,10X,' e.) Top/WPair background selection : ',//,
     1 5X,' No of events selected               = ',I5,/,
     2 5X,' No not found by top selection       = ',I5,/,
     3 5X,' No of WPair->emu candidates           = ',I5,/,
     4 5X,' No of WPair->ee candidates            = ',I5,/,
     5 5X,' No of WPair->mumu candidates          = ',I5,/,
     6 5X,' No of WPair->e+jets candidates        = ',I5,/,
     7 5X,' No of WPair->mu+jets candidates       = ',I5)
 1100 FORMAT(2I8,2I10,' #e,#mu,#ph,#jt = ',
     1 4(I2,2X))
 1200 FORMAT(//,' ==> TOP_LEPTONS_EVENT - Illegal Jet Algorithm',
     1 ' Requested : Type = ',I2,' <==',//)
2000  FORMAT(80(1h-),///,10x,'Processing event ',i5,
     1  ' event = ',i8,' run = ',i8,/////)
      END
