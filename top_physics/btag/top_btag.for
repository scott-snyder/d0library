      FUNCTION TOP_BTAG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Analysis subroutine for TOP_BTAG package.
C-
C-   Created  5-Aug-1993   Herbert Greenlee
C-
C-  Ntuple variables
C-  ================
C-
C-  Global event quantities:
C-
C-  1.   RUN      - Run number.
C-  2.   EVENT    - Event number.
C-  3.   WEIGHT   - Isajet event weight (ISAE).
C-  4.   UWGHT    - User weight (RCP parameter).
C-  5.   DWGHT    - Showerlib. event weight (WGHT).
C-  6.   NELEC    - Number of electrons.
C-  7.   NPHOT    - Number of photons.
C-  8.   NJET     - Number of jets.
C-  9.   NMUON    - Number of high-pt muons.
C-  10.  NMUTAG   - Number of tagging muons.
C-  11.  MET2U    - PNUT2 missing ET.
C-  12.  MPHI2U   - PNUT2 missing phi.
C-  13.  MET2     - PNUT2 missing ET w/em and jet corrections.
C-  14.  MPHI2    - PNUT2 missing phi w/em and jet corrections.
C-  15.  MET3     - PNUT3 missing ET w/em and jet correctinos.
C-  16.  MPHI3    - PNUT3 missing phi w/em and jet corrections.
C-  17.  HT       - HT
C-  18.  SERBAN   - Serban's central ET ratio.
C-  19.  CLNCAL   - CLEAN_CAL_JUNK flag.
C-  20.  TRIG     - Trigger bitmask
C-  21.  NVERT    - Number of vertices.
C-  22,  ZVERT    - Z of primary vertex.
C-  23.  MCDATA   - MC data flag (0/1)
C-  24.  ZMCVTX   - Z of isajet vertex
C-  25.  NT       - Number of final state t quarks (ISAJ).
C-  26.  NB       - Number of final state b quarks (ISAQ).
C-  27.  NC       - Number of final state c quarks (ISAQ).
C-  28.  NS       - Number of final state s quarks (ISAQ).
C-  29.  ND       - Number of final state d quarks (ISAQ).
C-  30   NU       - Number of final state u quarks (ISAQ).
C-  31.  NG       - Number of final state gluons (ISAQ).
C-  32.  NE       - Number of prompt electrons (ISAL).
C-  33.  NMU      - Number of prompt muons (ISAL).
C-  34.  NTAU     - Number of prompt taus (ISAQ).
C-  35.  NGI      - Number of initial state gluons (ISAQ).
C-  36.  NUI      - Number of initial state u (ISAQ).
C-  37.  NDI      - Number of initial state d (ISAQ).
C-  38.  NSI      - Number of initial state s (ISAQ).
C-  39.  NCI      - Number of initial state c (ISAQ).
C-  40.  NBI      - Number of initial state b (ISAQ).
C-  41.  SET      - Total scalar ET.
C-  42.  SETC     - Scalar ET in CC.
C-  43.  SETE     - Scalar ET in EC.
C-  44.  SETMR    - Scalar ET near main ring.
C-  45.  HOTE     - Hot E (cal_hot_et)
C-  46.  HOTET    - Hot ET (calc_hot_et)
C-  47.  NJET10   - Number of jets w/ET.gt.10.
C-  48.  NJET15   - Number of jets w/ET.gt.15.
C-  49.  NJET20   - Number of jets w/ET.gt.20.
C-  50.  NJET25   - Number of jets w/ET.gt.25.
C-  51.  NJET30   - Number of jets w/ET.gt.30.
C-  52.  NJET35   - Number of jets w/ET.gt.35.
C-  53.  NJET40   - Number of jets w/ET.gt.40.
C-  54.  MJ12     - Jet1-jet2 inv. mass.
C-  55.  MJ13     - Jet1-jet3 inv. mass.
C-  56.  MJ23     - Jet2-jet3 inv. mass.
C-  57.  MJBEST   - Nearest mw of mj12, mj13 and mj23.
C-  58.  MJ123    - Jet1-jet2-jet3 inv. mass.
C-  59.  MASS     - Invariant mass off all particles.
C-  60.  X1       - X1.
C-  61.  X2       - X2.
C-  62.  XF       - Feynman x.
C-  63.  MT4      - Best top mass for 4-jet events.
C-  64.  DMT4     - Delta-top-mass for 4-jet events.
C-  65.  S        - Sphericity
C-  66.  Y        - Variable orthogonal to sphericity.
C-  67.  A        - Aplanarity
C-  68.  SJ       - Jet sphericity
C-  69.  YJ       - Jet Y
C-  70.  AJ       - Jet aplanarity
C-  71.  MGG      - Gamma-gamma mass
C-  72.  MGJ1     - Mass (gamma-jet-1)
C-  73.  MGJ2     - Mass (gamma-jet-2)
C-  74.  MGJ3     - Mass (gamma-jet-3)
C-  75.  MBP3     - Best bprime mass for photon + 3 jet events.
C-  76.  DMBP3    - Delta-bprime-mass for photon + 3 jet events
C-
C-  Vertices
C-  ========
C-
C-  1.   ZVERT    - Z of vertex.
C-
C-  Electrons (PELC)
C-  ================
C-
C-  1.   EETUn    - Uncorrected ET.
C-  2.   EETn     - Corrected ET.
C-  3.   EETAn    - Eta.
C-  4.   EPHIn    - Phi.
C-  5.   ECETAn   - Cal. eta.
C-  6.   ECHIn    - Truncated H-matrix chisquare.
C-  7.   EDRn     - Delta R to nearest jet.
C-  8.   EB2Bn    - Back-to-back parameter (pi-phi(e)-phi(jet1)).
C-  9.   EFRACn   - EM fraction.
C-  10.  EISOn    - Isolation parameter.
C-  11.  ECELLSn  - Number of cells.
C-  12.  ETSIGn   - Track match significance.
C-  13.  ECDCMIPn - Mips in CDC.
C-  14.  EFDCMIPn - Mips in CDC.
C-  15.  ETRDACCn - TRD acceptance flag.
C-  16.  ETRDEFFn - TRD efficiency.
C-
C-  Photons (PPHO)
C-  ==============
C-
C-  1.   PETUn    - Uncorrected ET.
C-  2.   PETn     - Corrected ET.
C-  3.   PETAn    - Eta.
C-  4.   PPHIn    - Phi.
C-  5.   PCETAn   - Cal. eta.
C-  6.   PCHIn    - Truncated H-matrix chisquare.
C-  7.   PDRn     - Delta R to nearest jet.
C-  8.   PB2Bn    - Back-to-back parameter (pi-phi(ph)-phi(jet1)).
C-  9.  PFRACn   - EM fraction.
C-  10.  PISOn    - Isolation parameter.
C-  11.  PCELLSn  - Number of cells.
C-
C-  Jets (JETS)
C-  ===========
C-
C-  1.   JALGn    - Jet algorithm (1=0.7, 2=0.5, 3=0.3, 4=NN).
C-  2.   JETUn    - Uncorrected ET.
C-  3.   JETn     - Corrected ET.
C-  4.   JETAn    - Eta.
C-  5.   JPHIn    - Phi.
C-  6.   JRADn    - RMS radius in R-space.
C-  7.   JEMFn    - EM fraction.
C-  8.   JDRBn    - Delta-R to nearest b quark (ISAQ).
C-  9.   JDRCn    - Delta-R to nearest c quark (ISAQ).
C-  10.  JDRTn    - Delta-R to nearest tau (ISAQ).
C-
C-  High-pt Muons (PMUO)
C-  ====================
C-
C-  1.   MPTn     - PT.
C-  2.   METAn    - Eta.
C-  3.   MPHIn    - Phi.
C-  4.   MIFW4n   - IFW4.
C-  5.   MBDLn    - Integral B.DL.
C-  6.   MECAL2n  - Calorimeter energy (0.2 cone).
C-  7.   MECAL4n  - Calorimeter energy (0.4 cone).
C-  8.   MECAL6n  - Calorimeter energy (0.6 cone).
C-  9.   MFT0n    - Floating t0.
C-  10.  MIPBn    - Bend view "3-D" impact parameter.
C-  11.  MIPNn    - Non-bend view impact parameter.
C-  12.  MDRn     - Delta R to nearest jet.
C-
C-  Tagging Muons (PMUO)
C-  ====================
C-
C-  1.   MTPTn    - PT.
C-  2.   MTETAn   - Eta.
C-  3.   MTPHIn   - Phi.
C-  4.   MTIFW4n  - IFW4.
C-  5.   MTBDLn   - Integral B.DL.
C-  6.   MTECAL2n - Calorimeter energy (0.2 cone).
C-  7.   MTECAL4n - Calorimeter energy (0.4 cone).
C-  8.   MTECAL6n - Calorimeter energy (0.6 cone).
C-  9.   MTFT0n   - Floating t0.
C-  10.  MTIPBn   - Bend view "3-D" impact parameter.
C-  11.  MTIPNn   - Non-bend view impact parameter.
C-  12.  MTDRn    - Delta R to nearest jet.
C-  13.  MTPTRn   - PT relative to nearest jet.
C-  14.  MTJETn   - ET of tagged jet.
C-  15.  MTJETAn  - Eta of tagged jet.
C-  16.  MTJPHIn  - Phi of tagged jet.
C-  17.  MTTAGn   - Good tag flag.
C-
C-  W bosons
C-  ========
C-
C-  1.   WMT2n    - W transverse mass from MET(2)
C-  2.   WMT3n    - W transverse mass from MET(3)
C-  3.   WPT2n    - W pt from MET(2)
C-  4.   WPT3n    - W pt from MET(3)
C-
C-  Z bosons
C-  ========
C-
C-  1.   ZMASS    - Mass of two leading electrons.
C-  3.   ZPT      - Pt of two leading electrons.
C-
C----------------------------------------------------------------------
      implicit none
      logical top_btag, top_btag_end
      include 'd0$inc:zebcom.inc'
      include 'd0$inc:quest.inc'
      include 'd0$links:izhmte.link'
      include 'd0$links:izvert.link'
      include 'd0$links:izisp1.link'
C-
C- Ntuple variables and definition.
C-
      integer event_ntuple_size        ! Total number of words in ntuple
      integer max_ntuple_size
      parameter(max_ntuple_size=512)
      character*12 event_tag(max_ntuple_size)
      common /top_btag_cwn/ event_data(max_ntuple_size)
      real event_data
      integer ievent_data(max_ntuple_size)
      equivalence (event_data(1), ievent_data(1))
      character*1 num
      character*8 tags(max_ntuple_size)
      integer ntoff, ioff, joff
C- Non-repeated (global) variables
      integer nglob_var                ! Number of global event quantities
C- Repeated variables
      integer num_vert, nvert_var, vert_offset        ! Vertices
      integer num_elec, nelec_var, elec_offset        ! Electrons
      integer num_phot, nphot_var, phot_offset        ! Photons
      integer num_jet, njet_var, jet_offset           ! Jets
      integer num_muon, nmuon_var, muon_offset        ! High-pt muons
      integer num_mutag, nmutag_var, mutag_offset     ! Soft muons
      integer num_w, nw_var, w_offset                 ! Electron W's
      integer num_z, nz_var, z_offset                 ! Electron Z's
C- Ntuple control
      character*80 ntuple_file
      character*16 ntuple_dir, ntuple_path
      character*8 rz_top_dir, rz_path, chblok
      character*256 chform
      integer max_rec, lrecl, nwbuff, icycle, num_col, col_width
      logical column_wise
      logical use_isp1            ! Get e,mu from isp1 (rather than isal)
C-
C- Protected link area.
C-
      integer max_jet_link, max_elec_link, max_phot_link, 
     &  max_muon_link, max_mutag_link
      parameter (max_jet_link = 20)
      parameter (max_elec_link = 10)
      parameter (max_phot_link = 10)
      parameter (max_muon_link = 10)
      parameter (max_mutag_link = 10)
      common /top_btag_link/ lfirst, 
     &  lisae, ljets, ljets_good(max_jet_link), lvcor,
     &  lpelc, lpelc2, lpelc_good(max_elec_link), 
     &  lppho, lppho_good(max_phot_link), 
     &  lhmte, lpmuo, lpmuo_soft(max_mutag_link), 
     &  lpmuo_isol(max_muon_link), lmuot, ljets_dr, 
     &  lpnut2, lpnut3, lpnut4, lvert, lverh, lisaq, lisal, lisaj, 
     &  lglob, lisv1, lisp1,
     &  llast
      integer lfirst, 
     &  lisae, ljets, ljets_good, lvcor,
     &  lpelc, lpelc2, lpelc_good, 
     &  lppho, lppho_good, 
     &  lhmte, lpmuo, lpmuo_soft, lpmuo_isol, lmuot, ljets_dr, 
     &  lpnut2, lpnut3, lpnut4, lvert, lverh, lisaq, lisal, lisaj, 
     &  lglob, lisv1, lisp1,
     &  llast
C-
C- Topological pre-selection variables.
C-
      integer num_elec_min, num_phot_min, num_jet_min, num_mutag_min,
     &  num_muon_min
      real met2_min, met3_min
      integer num_em_min
C-
C- Final selection variables.
C-
      logical do_final_selection
      real jet1, jet2, jet3, jet4
      integer num_elec_min_final, num_phot_min_final, 
     &  num_muon_min_final, num_mutag_min_final
      real jet1_etmin_final, jet2_etmin_final, jet3_etmin_final, 
     &  jet4_etmin_final
      real met2_min_final, met3_min_final
C-
C- Statistics
C-
      integer num_input, num_ntuple, num_output
C-
C- Analysis variables
C-
      logical mcdata
      logical clncal
      real temp
      real cm_energy
      real s                           ! C.M. energy squared
      integer run, event
      real weight                      ! Isajet event weight
      real uwght                       ! User supplied weight
      real dwght                       ! Showerlibrary decay weight
      real e, et, px, py, pz, p, pt, eta, phi, etu, p4(4), p2(2)
      real etmax
      real dpx, dpy
      real sum_et, sum_e
      real mt4, dmt4
      real mbp3, dmbp3
C-
C- Trigger
C-
      integer ntrig_max
      parameter (ntrig_max=20)
      logical found
      integer ntrig, trig, l2num, trig_ntuple_bit(ntrig_max)
      character*32 trig_ntuple_name(ntrig_max)
C- Isajet
      integer pid, qid1, qid2          ! Isajet particle id.
      integer nt, nb, nc, ns, nd, nu, ng, ne, nmu, ntau
      integer ngi, nui, ndi, nsi, nci, nbi
C- Vertices
      integer nvert, mvert, max_vert   ! Number of vertices.
      parameter(max_vert=3)
      real vert_info(3,max_vert)
      real zvert                       ! Z of vertex.
      real zmcvtx, dzmcvtx             ! Z of MC vertex.
      logical use_mc_vertex
C- Electrons & Photons
      real phi_jet1
      integer nelec, nphot             ! Number of good
      integer melec, mphot
      real caleta                      ! Calorimeter eta
      real chi                         ! H-matrix chisquare
      real b2b                         ! Back-to-back parameter
      real emfrac, iso, trksig         ! Cleanem quantities
      real cdcmip, fdcmip, trdacc, trdeff
      integer ncells                   ! Number of cells in cluster
      real cquan(30),tquan(30)         ! Cleanem quantities
      logical ok
      integer ncvar, ntvar, istat
C- Jets
      integer nstruc, nl
      integer jet_alg                  ! Requested jet algorithm
      integer jet_sys                  ! Systematics flag
      integer njet, mjet               ! Number of good jets
      integer njet10, njet15, njet20, njet25, njet30, njet35, njet40
      integer algorithm                ! Actual algorithm
      real pjet(4, max_jet_link)       ! 4-vectors for 2-body inv. mass.
      real radius
      real jet_correction_factor
      real drb, drc, drtau
      integer ldummy
C- Muons
      integer nmutag, mmutag, nmuon, mmuon
      integer ifw4
      real jpx, jpy, jpz, jp, je, jet, jeta, jphi
      real eloss
      real ptrel, ecal2, ecal4, ecal6, dr, bdl, ft0, impactb, impactn
      real good_tag
C- Vector bosons
      logical w_use_met3
      logical do_electron_w_analysis, do_electron_z_analysis
      logical do_electron_w_reconstruction, do_electron_z_reconstruction
      real pw4(4), pzw2
      real pxw, pyw, pzw, ew, pxz, pyz, pzz, ez
      real wmt2, wmt3, wpt2, wpt3, zmass, zpt ! Mass and pt variables
      real mw, mz
C- Scalar ET
      real set, setc, sete, setmr
C- Hot ET
      real hot_e, hot_et
C- Missing ET
C      integer version, pass            ! For MISS_ET_CORR call.
C      real met_vec(3)
C      real cal_escale(3)               ! For MISS_ET_CORR call.
      real met2u, mphi2u, met2, mphi2, met3, mphi3, metx3, mety3
      real metx, mety
C- HT
      real ht, ht_jeteta_max, ht_jetpt_min
C- Event shape
      integer max_part, npart
      parameter(max_part=20)
      real pv(4,max_part)              ! 4-vectors for spher., aplan.
      real ptot(4)
      real mj12, mj13, mj23, mjbest
      real s0, y0, a0
      real s1, y1, a1
      real eigval(3), eigvec(3,3)
C- Mass, xf, etc.
      real pxtot, pytot, pztot, etot, mass, xf, x1, x2, tau
      real mwj1, mwj2, mwj3, mwj4, mj123, mj124, mj134, mj234
      real mgg, mgj1, mgj2, mgj3, mg2j1, mg2j2
C- Functions
      real rndm
      real proxim
      integer ssunit
      integer get_caph_alg
      integer runno, evonum, trulen
      integer gzisae, gzjets, gzpnut, gzisaq, gzisal, gzglob, gzisaj,
     &  gzisv1
      logical clean_cal_junk
C- Miscellanious variables
      integer lun
      integer i, j, k, l, n
      integer ival, lval, type
      character*32 cval
      logical done
      real pi
      integer unit, ier
      logical first
C-
C- Statement functions
C-
      real psqrt
      psqrt(s) = sqrt(amax1(s,0.))
      data first/.true./
C----------------------------------------------------------------------
C
      top_btag = .true.        ! set it to false to skip any additional
                               ! processing of current event
      if(lhead.eq.0)then
        top_btag = .false.
        return
      endif
      mcdata = iq(lhead+1).gt.1000   ! Monte Carlo data?
C-
C- Initialization block
C-
      if(first) then
        first=.false.
        pi = 2.*acos(0.)
C-
C- Zero counters
C-
        num_input = 0
        num_ntuple = 0
        num_output = 0
C-
C- Initialize link area.
C-
        call mzlink(ixcom, '/top_btag_link/', lfirst, lfirst, llast)
C-
C- Read RCP parameters from TOP_BTAG_RCP.
C-
        call inrcp('top_btag_rcp',ier)
        if(ier.ne.0)call errmsg('TOP_BTAG','TOP_BTAG','INRCP failed',
     &    'F')
        call ezpick('top_btag_rcp')
        call ezerr(ier)
C-
C- User weight (contains M.C. luminosity information).
C-
        if(ier.eq.0)call ezget('user_weight', uwght, ier)
        if(ier.ne.0)then
          uwght = 1.
          ier = 0
        endif
C-
C- Ntuple control parameters
C-
        if(ier.eq.0)call ezgets('NTUPLE_FILE', 1, ntuple_file, 
     &    l, ier)
        if(ier.eq.0)call ezgets('NTUPLE_DIR', 1, ntuple_dir, 
     &    l, ier)
        if(ier.eq.0)call ezget('ntuple_max_records', 
     &    max_rec, ier)
        if(ier.eq.0)call ezget('ntuple_record_size', 
     &    lrecl, ier)
        if(ier.eq.0)call ezget('ntuple_buffer_size', 
     &    nwbuff, ier)
        if(ier.eq.0)call ezget('column_wise_ntuple', 
     &    column_wise, ier)
        if(ier.eq.0)call ezget('ntuple_num_columns', 
     &    num_col, ier)
        if(ier.eq.0)call ezget('use_isp1', 
     &    use_isp1, ier)
C-
C- C.M. energy
C-
        if(ier.eq.0)call ezget('cm_energy', cm_energy, ier)
        s = cm_energy**2
C-
C- Vector boson masses
C-
        if(ier.eq.0)call ezget('MASS_W', mw, ier)
        if(ier.eq.0)call ezget('MASS_Z', mz, ier)
C-
C- Vertex parameters
C-
        if(ier.eq.0)call ezget('use_mc_vertex', use_mc_vertex, ier)
C-
C- Jets parameters
C-
        if(ier.eq.0)call ezget('jet_alg', jet_alg, ier)
        if(ier.eq.0)call ezget('jet_sys', jet_sys, ier)
C-
C- Vector boson parameters
C-
        if(ier.eq.0)call ezget('w_use_met3', w_use_met3, ier)
        if(ier.eq.0)call ezget('do_electron_w_analysis',
     &    do_electron_w_analysis, ier)
        if(ier.eq.0)call ezget('do_electron_w_reconstruction',
     &    do_electron_w_reconstruction, ier)
        if(ier.eq.0)call ezget('do_electron_z_analysis',
     &    do_electron_z_analysis, ier)
        if(ier.eq.0)call ezget('do_electron_z_reconstruction',
     &    do_electron_z_reconstruction, ier)
C-
C- Topological preselection cuts (determines ntuple filling).
C-
        if(ier.eq.0)call ezget('num_elec_min',num_elec_min,ier)
        if(ier.eq.0)call ezget('num_phot_min',num_phot_min,ier)
        if(ier.eq.0)call ezget('num_em_min',num_em_min,ier)
        if(ier.eq.0)call ezget('num_jet_min',num_jet_min,ier)
        if(ier.eq.0)call ezget('num_muon_min',num_muon_min,ier)
        if(ier.eq.0)call ezget('num_mutag_min',num_mutag_min,ier)
        if(ier.eq.0)call ezget('met2_min',met2_min,ier)
        if(ier.eq.0)call ezget('met3_min',met3_min,ier)
C-
C- Final selection cuts (determines return value, dst output).
C-
        if(ier.eq.0)call ezget('do_final_selection', 
     &    do_final_selection, ier)
        if(ier.eq.0)call ezget('num_elec_min_final',
     &    num_elec_min_final,ier)
        if(ier.eq.0)call ezget('num_phot_min_final',
     &    num_phot_min_final,ier)
        if(ier.eq.0)call ezget('jet1_etmin_final', jet1_etmin_final,
     &    ier)
        if(ier.eq.0)call ezget('jet2_etmin_final', jet2_etmin_final,
     &    ier)
        if(ier.eq.0)call ezget('jet3_etmin_final', jet3_etmin_final,
     &    ier)
        if(ier.eq.0)call ezget('jet4_etmin_final', jet4_etmin_final,
     &    ier)
        if(ier.eq.0)call ezget('num_muon_min_final',
     &    num_muon_min_final, ier)
        if(ier.eq.0)call ezget('num_mutag_min_final',
     &    num_mutag_min_final, ier)
        if(ier.eq.0)call ezget('met2_min_final', met2_min_final, ier)
        if(ier.eq.0)call ezget('met3_min_final', met3_min_final, ier)
C-
C- HT parameters
C-
        if(ier.eq.0)call ezget('ht_jetpt_min',ht_jetpt_min,ier)
        if (ier.eq.0)call ezget('ht_jeteta_max',ht_jeteta_max,ier)
C-
C- Ntuple definition parameters (tags, etc.)
C-
C- Trigger bitmask
        n = 1
        ntrig = 0
        done = .false.
        do while (.not.done)
          ntrig = ntrig + 1
          call ezget_next_value_type('TRIG_BITMASK', ival, cval, type, 
     &      lval, ier, n)
          if(ier.ne.0.or.type.ne.1)call errmsg('TOP_BTAG', 'TOP_BTAG',
     &      'Error getting trigger bitmask parameters', 'F')
          trig_ntuple_bit(ntrig) = ival
          call ezget_next_value_type('TRIG_BITMASK', ival, cval, type, 
     &      lval, ier, n)
          if(ier.ne.0.and.ier.ne.1.or.type.lt.10)
     &      call errmsg('TOP_BTAG', 'TOP_BTAG',
     &      'Error getting trigger bitmask parameters', 'F')
          trig_ntuple_name(ntrig) = cval(1:lval)
          if(ier.eq.1)then
            ier = 0
            done = .true.
          endif
        enddo
C- Global variables.
        if(ier.eq.0)call ez_get_chars('GLOB_TAG', nglob_var, 
     &    event_tag, ier)
        n = nglob_var
C- Vertices
        if(ier.eq.0)call ezget('NUM_VERT', num_vert, ier)
        if(ier.eq.0)call ez_get_chars('VERT_TAG', nvert_var, tags, 
     &    ier)
        vert_offset = n
        do j = 1, num_vert
          num = char(ichar('0')+j)
          do i = 1, nvert_var
            n = n + 1
            event_tag(n) = tags(i)(1:trulen(tags(i)))//num
          enddo
        enddo
C- Electrons
        if(ier.eq.0)call ezget('NUM_ELEC', num_elec, ier)
        if(ier.eq.0)call ez_get_chars('ELEC_TAG', nelec_var, tags, 
     &    ier)
        elec_offset = n
        do j = 1, num_elec
          num = char(ichar('0')+j)
          do i = 1, nelec_var
            n = n + 1
            event_tag(n) = tags(i)(1:trulen(tags(i)))//num
          enddo
        enddo
C- Photons
        if(ier.eq.0)call ezget('NUM_PHOT', num_phot, ier)
        if(ier.eq.0)call ez_get_chars('PHOT_TAG', nphot_var, tags, 
     &    ier)
        phot_offset = n
        do j = 1, num_phot
          num = char(ichar('0')+j)
          do i = 1, nphot_var
            n = n + 1
            event_tag(n) = tags(i)(1:trulen(tags(i)))//num
          enddo
        enddo
C- Jets
        if(ier.eq.0)call ezget('NUM_JET', num_jet, ier)
        if(ier.eq.0)call ez_get_chars('JET_TAG', njet_var, tags, 
     &    ier)
        jet_offset = n
        do j = 1, num_jet
          num = char(ichar('0')+j)
          do i = 1, njet_var
            n = n + 1
            event_tag(n) = tags(i)(1:trulen(tags(i)))//num
          enddo
        enddo
C- High-pt muons
        if(ier.eq.0)call ezget('NUM_MUON', num_muon, ier)
        if(ier.eq.0)call ez_get_chars('MUON_TAG', nmuon_var, tags, 
     &    ier)
        muon_offset = n
        do j = 1, num_muon
          num = char(ichar('0')+j)
          do i = 1, nmuon_var
            n = n + 1
            event_tag(n) = tags(i)(1:trulen(tags(i)))//num
          enddo
        enddo
C- Soft muons
        if(ier.eq.0)call ezget('NUM_MUTAG', num_mutag, ier)
        if(ier.eq.0)call ez_get_chars('MUTAG_TAG', nmutag_var, tags, 
     &    ier)
        mutag_offset = n
        do j = 1, num_mutag
          num = char(ichar('0')+j)
          do i = 1, nmutag_var
            n = n + 1
            event_tag(n) = tags(i)(1:trulen(tags(i)))//num
          enddo
        enddo
C- Electorn W's
        if(ier.eq.0)call ezget('NUM_W', num_w, ier)
        if(ier.eq.0)call ez_get_chars('W_TAG', nw_var, tags, 
     &    ier)
        w_offset = n
        do j = 1, num_w
          num = char(ichar('0')+j)
          do i = 1, nw_var
            n = n + 1
            event_tag(n) = tags(i)(1:trulen(tags(i)))//num
          enddo
        enddo
C- Electron Z's
        if(ier.eq.0)call ezget('NUM_Z', num_z, ier)
        if(ier.eq.0)call ez_get_chars('Z_TAG', nz_var, tags, 
     &    ier)
        z_offset = n
        do j = 1, num_z
          num = char(ichar('0')+j)
          do i = 1, nz_var
            n = n + 1
            event_tag(n) = tags(i)(1:trulen(tags(i)))//num
          enddo
        enddo
        event_ntuple_size = n
        if (ier.ne.0)call errmsg('TOP_BTAG', 'TOP_BTAG',
     &    'Error getting RCP parameters', 'F')
        call ezrset
C-
C- Open RZ file for ntuple.
C-
        call gtunit(777,unit,ier)
        write(rz_top_dir, '(''LUN'',I3.3)')unit
        rz_path = '//'//rz_top_dir
        iquest(10) = max_rec
        call hropen(unit, rz_top_dir, ntuple_file, 'NQ', lrecl, ier)
        if(ier.ne.0)call errmsg('TOP_BTAG', 'TOP_BTAG', 
     &    'HROPEN failed for ntuple file', 'F')
C-
C- Make directory for memory resident part of ntuple and go there.
C-
        call hcdir('//PAWC', ' ')
        ntuple_path = '//PAWC/'//ntuple_dir
        call hmdir(ntuple_dir, ' ')
        call hcdir(ntuple_path, ' ')
        if(iquest(1).ne.0)call errmsg('TOP_BTAG', 'TOP_BTAG', 
     &    'HMDIR or HCDIR failed for ntuple directory', 'F')
C-
C- Book ntuple.
C-
        if(column_wise)then
C- Book column-wise ntuple
          call hcdir(rz_path, ' ')
          call hbset('BSIZE', nwbuff, ier)
          call hbnt(1, 'Event', ' ')
          if(iquest(1).ne.0)call errmsg('TOP_BTAG', 'TOP_BTAG', 
     &      'HBNT failed', 'F')
C- Define columns
          n = event_ntuple_size
          k = 0
          do i = 1, num_col
            write(chblok,'(''COL'',i3.3)')i
            col_width = n/(num_col - i + 1)
            chform = ' '
            do j = k + 1, k + col_width
              if(chform.ne.' ')chform = chform(1:trulen(chform))//','
              chform = chform(1:trulen(chform))//
     &                 event_tag(j)(1:trulen(event_tag(j)))
              if(index(event_tag(j),':').eq.0)
     &          chform = chform(1:trulen(chform))//':R'
            enddo
            if(trulen(chform).eq.len(chform))
     &        call errmsg('TOP_BTAG', 'TOP_BTAG', 
     &        'Column too big', 'F')
            call hbname(1, chblok, event_data(k+1), chform)
            n = n - col_width
            k = k + col_width
          enddo
        else
C- Book row-wise ntuple
          call hbookn(1, 'Event', event_ntuple_size, rz_path, nwbuff, 
     &      event_tag)
          if(iquest(1).ne.0)call errmsg('TOP_BTAG', 'TOP_BTAG', 
     &      'HBOOKN failed', 'F')
        endif
C-
C- End of Initialization.
C-
      endif
C-
C- Count input events
C-
      num_input = num_input + 1
C-
C- Set path to ntuple dir.
C-
      call hcdir(ntuple_path, ' ')
      if(iquest(1).ne.0)call errmsg('TOP_BTAG', 'TOP_BTAG', 
     &  'HCDIR failed after ntuple was successfully booked', 'F')
C-
C- Get links of good objects from PARTICLE_SELECT.
C-
      call set_caph_alg(jet_alg)
C-
C- Electron id.
C-
      if(mcdata)then
        call gtslink('MCE_TGHT', max_elec_link, nelec, lpelc_good)
      else
        call gtslink('ELE_TGHT', max_elec_link, nelec, lpelc_good)
      endif
      melec = min(nelec, num_elec)
C-
C- Photon id.
C-
      if(mcdata)then
        call gtslink('MCG_TGHT', max_phot_link, nphot, lppho_good)
      else
        call gtslink('GAM_TGHT', max_phot_link, nphot, lppho_good)
      endif
      mphot = min(nphot, num_phot)
C-
C- High-pt muon id.
C-
      call gtslink('ISOLMUON', max_muon_link, nmuon, lpmuo_isol)
      mmuon = min(nmuon, num_muon)
C-
C- Soft muon id.
C-
      call gtslink('BTAGMUON', max_mutag_link, nmutag, lpmuo_soft)
      mmutag = min(nmutag, num_mutag)
C-
C- Jet id.
C-
      call gtslink('TOP_JETS', max_jet_link, njet, ljets_good)
      mjet = min(njet, num_jet)
C-
C- End of particle id.
C-
      call reset_caph
C-
C- Loop over vertices.  Get quantities for ntuple.  Leave the primary vertex
C- position in zvert.
C-
      nvert = 0
      zvert = 0.
      call vzero(event_data(vert_offset+1), num_vert * nvert_var)
      ntoff = vert_offset
      call vertex_info(max_vert, nvert, vert_info, ok)
      if(.not.ok)then
        nvert = 0
        zvert = 0.
      endif
      mvert = min(nvert,num_vert)
      do i = 1,mvert
        event_data(ntoff+1) = vert_info(1,i)
        ntoff = ntoff + nvert_var
      enddo
C- Get MC vertex (if appropriate)
      if(mcdata)then
        call zvertx(zmcvtx, dzmcvtx)
      else
        zmcvtx = 0.
      endif
C- Get official Z vertex
      if(mcdata.and.use_mc_vertex)then
        zvert = zmcvtx
      elseif(nvert.gt.0)then
        zvert = event_data(vert_offset+1)
      else
        zvert = 0.
      endif
C-
C- Missing ET analysis.
C- First get uncorrected cal missing ET.
C-
      lpnut2 = gzpnut(2)
      metx = q(lpnut2 + 3)
      mety = q(lpnut2 + 4)
      met2u = sqrt(metx**2 + mety**2)
      mphi2u = proxim(atan2(mety, metx), pi)
C- Get cal corrected missing ET from pnut(4)
      lpnut4 = gzpnut(4)
      if(lpnut4.eq.0)then
        call errmsg('No pnut4', 'TOP_BTAG', 
     &    'No PNUT4 bank found -- using PNUT2 instead', 'W')
        lpnut4 = lpnut2
      endif
      metx = q(lpnut4 + 3)
      mety = q(lpnut4 + 4)
      met2 = sqrt(metx**2 + mety**2)
      mphi2 = proxim(atan2(mety, metx), pi)
C- Now correct for muons.
      metx3 = metx
      mety3 = mety
      do i = 1,nmuon
        lpmuo = lpmuo_isol(i)
        px = q(lpmuo+10)
        py = q(lpmuo+11)
        p = q(lpmuo+13)
        eloss = q(lpmuo+33)             ! Expected loss
        metx3 = metx3 - px*(1.-eloss/p)
        mety3 = mety3 - py*(1.-eloss/p)
      enddo
      do i = 1,nmutag
        lpmuo = lpmuo_soft(i)
        px = q(lpmuo+10)
        py = q(lpmuo+11)
        p = q(lpmuo+13)
        eloss = q(lpmuo+33)             ! Expected loss
        metx3 = metx3 - px*(1.-eloss/p)
        mety3 = mety3 - py*(1.-eloss/p)
      enddo
      met3 = sqrt(metx3**2 + mety3**2)
      mphi3 = proxim(atan2(mety3, metx3), pi)
C-
C- Ntuple pre-selection cuts.
C-
      if(    nelec.lt.num_elec_min
     &  .or. nphot.lt.num_phot_min
     &  .or. nphot+nelec .lt. num_em_min
     &  .or. njet.lt.num_jet_min
     &  .or. nmuon.lt.num_muon_min
     &  .or. nmutag.lt.num_mutag_min
     &  .or. met2.lt.met2_min
     &  .or. met3.lt.met3_min)then
        top_btag = .false.
        go to 999
      endif
C-
C- If we get here we want this event to be added to the ntuple.
C-
C- Get easy global event quantities.
C-
      run = runno()
      event = evonum()
      lisae = gzisae()
      if(lisae.ne.0)then
        weight = q(lisae+12)
      else
        weight = 1.
      endif
      call gtwght(dwght)
      lglob = gzglob()
      setc = q(lglob+7)
      sete = q(lglob+6)
      set = setc + sete
      setmr = q(lglob+14)
      call calc_hot_et(hot_e, hot_et)
      clncal = clean_cal_junk()
C-
C- Construct trigger bitmask
C-
      trig = 0
      do i = 1,ntrig
        call get_l2_bit_number(trig_ntuple_name(i), l2num, found)
        if(found)trig = ibset(trig, trig_ntuple_bit(i))
      enddo
C-
C- Loop over Isajet banks.  Get quantities for ntuple.
C-
      nt = 0
      nb = 0
      nc = 0
      ns = 0
      nd = 0
      nu = 0
      ng = 0
      ne = 0
      nmu = 0
      ntau = 0
      ngi = 0
      nui = 0
      ndi = 0
      nsi = 0
      nci = 0
      nbi = 0
      lisaj = gzisaj()
      do while(lisaj.ne.0)
        pid = iq(lisaj+1)
        if(    abs(pid).eq.6    .or. abs(pid).eq.1600
     &    .or. abs(pid).eq.2600 .or. abs(pid).eq.3600
     &    .or. abs(pid).eq.4600 .or. abs(pid).eq.5600
     &    .or. abs(pid).eq.6700 .or. abs(pid).eq.6800)nt = nt + 1
        if(abs(pid).eq.6600)nt = nt + 2
        lisaj = lq(lisaj)
      enddo
      lisaq = gzisaq()
      do while(lisaq.ne.0)
        pid = iq(lisaq+1)
        if(abs(pid).ge.1 .and. abs(pid).le.8)then
          qid1 = pid
          qid2 = 0
        elseif(abs(pid).ge.1000 .and. abs(pid).le.8800
     &         .and.mod(pid,100).eq.0)then
          qid1 = mod(pid/100,10)
          qid2 = pid/1000
        else
          qid1 = 0
          qid2 = 0
        endif
        if(lq(lisaq-1).ne.0)then
          if(abs(qid1).eq.5)nb = nb + 1
          if(abs(qid2).eq.5)nb = nb + 1
          if(abs(qid1).eq.4)nc = nc + 1
          if(abs(qid2).eq.4)nc = nc + 1
          if(abs(qid1).eq.3)ns = ns + 1
          if(abs(qid2).eq.3)ns = ns + 1
          if(abs(qid1).eq.2)nd = nd + 1
          if(abs(qid2).eq.2)nd = nd + 1
          if(abs(qid1).eq.1)nu = nu + 1
          if(abs(qid2).eq.1)nu = nu + 1
          if(pid.eq.9)ng = ng + 1
          if(abs(pid).eq.16)ntau = ntau + 1
        else
          if(abs(qid1).eq.5)nbi = nbi + 1
          if(abs(qid2).eq.5)nbi = nbi + 1
          if(abs(qid1).eq.4)nci = nci + 1
          if(abs(qid2).eq.4)nci = nci + 1
          if(abs(qid1).eq.3)nsi = nsi + 1
          if(abs(qid2).eq.3)nsi = nsi + 1
          if(abs(qid1).eq.2)ndi = ndi + 1
          if(abs(qid2).eq.2)ndi = ndi + 1
          if(abs(qid1).eq.1)nui = nui + 1
          if(abs(qid2).eq.1)nui = nui + 1
          if(pid.eq.9)ngi = ngi + 1
        endif
        lisaq = lq(lisaq)
      enddo
      if(use_isp1)then
        lisv1 = gzisv1()
        do while(lisv1.ne.0)
          lisp1 = lq(lisv1-izisp1)
          do while(lisp1.ne.0)
            pid = iq(lisp1+1)
            if(abs(pid).eq.12)ne = ne + 1
            if(abs(pid).eq.14)nmu = nmu + 1
            lisp1 = lq(lisp1)
          enddo
          lisv1 = lq(lisv1)
        enddo
      else
        lisal = gzisal()
        do while(lisal.ne.0)
          pid = iq(lisal+1)
          if(abs(pid).eq.12)ne = ne + 1
          if(abs(pid).eq.14)nmu = nmu + 1
          lisal = lq(lisal)
        enddo
      endif
C-
C- Get phi of leading jet for back-to-back calculations.
C-
      phi_jet1 = 1000.
      etmax = 0.
      do i=1,mjet
        ljets = ljets_good(i)
        et = q(ljets+6)
        if(et.gt.etmax)then
          etmax = et
          phi_jet1 = q(ljets+8)
        endif
      enddo
C-
C- Here we loop over good particles again and get quantities for ntuple and
C- do other analysis.
C-
      npart = 0
      ht = 0.
      sum_et = 0.
      sum_e = 0.
C-
C- Loop over electrons.  Get quantities for ntuple.
C-
      call vzero(event_data(elec_offset+1), num_elec * nelec_var)
      ntoff = elec_offset
      do i = 1,melec
        lpelc = lpelc_good(i)
        et = q(lpelc+7)
        px = q(lpelc+3)
        py = q(lpelc+4)
        pz = q(lpelc+5)
        e  = q(lpelc+6)
        lvcor = lq(lpelc-4)
        if(lvcor.ne.0)then
          dpx = q(lvcor+3)
          dpy = q(lvcor+4)
        else
          dpx = 0.
          dpy = 0.
        endif
        etu = sqrt((px-dpx)**2 + (py-dpy)**2)
C- Call cleanem again and get additional quantities for ntuple
        call cleanem(lpelc, 1, ok, istat)
        call cleanem_cquans(ncvar, cquan)
        call cleanem_tquans(ntvar, tquan)
        eta = cquan(17)
        phi = cquan(18)
        caleta = cquan(5)
        chi = cquan(4)
        emfrac = cquan(9)
        iso = cquan(13)
        ncells = cquan(21)
        trksig = tquan(12)
        cdcmip = tquan(13)
        fdcmip = tquan(14)
        trdacc = tquan(22)
        trdeff = tquan(23)
        call find_nearest_object(eta, phi, njet, ljets_good, 9, 8, 
     &    ljets_dr, dr)
        if(phi_jet1.lt.1000.)then
          b2b = proxim(pi-phi+phi_jet1, 0.)
        else
          b2b = pi
        endif
        event_data(ntoff+1) = etu
        event_data(ntoff+2) = et
        event_data(ntoff+3) = eta
        event_data(ntoff+4) = phi
        event_data(ntoff+5) = caleta
        event_data(ntoff+6) = chi
        event_data(ntoff+7) = dr
        event_data(ntoff+8) = b2b
        event_data(ntoff+9) = emfrac
        event_data(ntoff+10) = iso
        event_data(ntoff+11) = ncells
        event_data(ntoff+12) = trksig
        event_data(ntoff+13) = cdcmip
        event_data(ntoff+14) = fdcmip
        event_data(ntoff+15) = trdacc
        event_data(ntoff+16) = trdeff
        ntoff = ntoff + nelec_var
C- Accumulate 4-vector
        npart = npart + 1
        if(npart.le.max_part)then
          pv(1, npart) = px
          pv(2, npart) = py
          pv(3, npart) = pz
          pv(4, npart) = e
        endif
      enddo
C-
C- Loop over photons.  Get quantities for ntuple.
C-
      call vzero(event_data(phot_offset+1), num_phot * nphot_var)
      ntoff = phot_offset
      do i = 1,mphot
        lppho = lppho_good(i)
        et = q(lppho+7)
        px = q(lppho+3)
        py = q(lppho+4)
        pz = q(lppho+5)
        e  = q(lppho+6)
        lvcor = lq(lppho-4)
        if(lvcor.ne.0)then
          dpx = q(lvcor+3)
          dpy = q(lvcor+4)
          dpy = q(lvcor+5)
        else
          dpx = 0.
          dpy = 0.
          dpy = 0.
        endif
        etu = sqrt((px-dpx)**2 + (py-dpy)**2)
C- Call cleanem again and get additional quantities for ntuple
        call cleanem(lppho, 0, ok, istat)
        call cleanem_cquans(ncvar, cquan)
        eta = cquan(17)
        phi = cquan(18)
        caleta = cquan(5)
        chi = cquan(4)
        emfrac = cquan(9)
        iso = cquan(13)
        ncells = cquan(21)
        call find_nearest_object(eta, phi, njet, ljets_good, 9, 8, 
     &    ljets_dr, dr)
        if(phi_jet1.lt.1000.)then
          b2b = proxim(pi-phi+phi_jet1, 0.)
        else
          b2b = pi
        endif
        event_data(ntoff+1) = etu
        event_data(ntoff+2) = et
        event_data(ntoff+3) = eta
        event_data(ntoff+4) = phi
        event_data(ntoff+5) = caleta
        event_data(ntoff+6) = chi
        event_data(ntoff+7) = dr
        event_data(ntoff+8) = b2b
        event_data(ntoff+9) = emfrac
        event_data(ntoff+10) = iso
        event_data(ntoff+11) = ncells
        ntoff = ntoff + nphot_var
C- Accumulate 4-vector
        npart = npart + 1
        if(npart.le.max_part)then
          pv(1, npart) = px
          pv(2, npart) = py
          pv(3, npart) = pz
          pv(4, npart) = e
        endif
      enddo
C-
C- Loop over jets.  Get jet quantities for ntuple.  Calculate HT and sums
C- for Serban's variable.
C-
      call vzero(event_data(jet_offset+1), num_jet * njet_var)
      ntoff = jet_offset
      njet10 = 0
      njet15 = 0
      njet20 = 0
      njet25 = 0
      njet30 = 0
      njet35 = 0
      njet40 = 0
      do i = 1,mjet
        ljets = ljets_good(i)
        algorithm = get_caph_alg(ljets)
C- Kinematic quantities.
C        etu = q(ljets+6)
C        call jet_corr(ljets, jet_sys, e, et, eta, phi, px, py, pz)
C- First get uncorrected energy.
        if(iq(ljets-1).ge.29 .and. btest(iq(ljets+26),0))then
          jet_correction_factor = q(ljets+29)
        else
          jet_correction_factor = 1.
        endif
        if(jet_correction_factor.eq.1.)then
          call errmsg('No jet correction', 'TOP_BTAG',
     &      'Jet energy scale factor = 1.0', 'W')
        endif
        e = q(ljets+5)
        et = q(ljets+6)
        etu = et/jet_correction_factor
        eta = q(ljets+9)
        phi = q(ljets+8)
        px = q(ljets+2)
        py = q(ljets+3)
        pz = q(ljets+4)
        radius = sqrt(q(ljets+12)**2 + q(ljets+13)**2)
        emfrac = q(ljets+14)
        call find_nearest_b(eta, phi, ldummy, drb)
        call find_nearest_c(eta, phi, ldummy, drc)
        call find_nearest_tau(eta, phi, ldummy, drtau)
        if(et.gt.10.)njet10 = njet10 + 1
        if(et.gt.15.)njet15 = njet15 + 1
        if(et.gt.20.)njet20 = njet20 + 1
        if(et.gt.25.)njet25 = njet25 + 1
        if(et.gt.30.)njet30 = njet30 + 1
        if(et.gt.35.)njet35 = njet35 + 1
        if(et.gt.40.)njet40 = njet40 + 1
        event_data(ntoff+1) = algorithm
        event_data(ntoff+2) = etu
        event_data(ntoff+3) = et
        event_data(ntoff+4) = eta
        event_data(ntoff+5) = phi
        event_data(ntoff+6) = radius
        event_data(ntoff+7) = emfrac
        event_data(ntoff+8) = drb
        event_data(ntoff+9) = drc
        event_data(ntoff+10) = drtau
        ntoff = ntoff + njet_var
C- Calculate HT
        if(et.ge.ht_jetpt_min .and. abs(eta).le.ht_jeteta_max)
     &    ht = ht + et
C- Accumulate 4-vectors
        npart = npart + 1
        if(npart.le.max_part)then
          pv(1, npart) = px
          pv(2, npart) = py
          pv(3, npart) = pz
          pv(4, npart) = e
        endif
C- Accumulate jets
        pjet(1, i) = px
        pjet(2, i) = py
        pjet(3, i) = pz
        pjet(4, i) = e
      enddo
C-
C- Sort jet data on word three (corrected ET).
C-
      do i=1,mjet-1
        ioff = jet_offset + njet_var*(i-1)
        do j=i+1,mjet
          joff = jet_offset + njet_var*(j-1)
          if(event_data(ioff+3).lt.event_data(joff+3))then
            do k = 1,njet_var
              temp = event_data(ioff+k)
              event_data(ioff+k) = event_data(joff+k)
              event_data(joff+k) = temp
            enddo
            do k = 1,4
              temp = pjet(k,i)
              pjet(k,i) = pjet(k,j)
              pjet(k,j) = temp
            enddo
          endif
        enddo
      enddo
C-
C- Calculate centrality
C-
      do i=1,min(mjet,4)
        ioff = jet_offset + njet_var*(i-1)
        sum_et = sum_et + event_data(ioff+3)
        sum_e = sum_e + pjet(4,i)
      enddo
C-
C- Loop over high-pt muons.  Get quantities for ntuple.
C-
      call vzero(event_data(muon_offset+1), num_muon * nmuon_var)
      ntoff = muon_offset
      do i = 1,mmuon
        lpmuo = lpmuo_isol(i)
        lmuot = lq(lpmuo-2)
        px = q(lpmuo+10)
        py = q(lpmuo+11)
        pz = q(lpmuo+12)
        p = q(lpmuo+13)
        pt = q(lpmuo+14)
        eta = q(lpmuo+16)
        phi = q(lpmuo+17)
        ifw4 = iq(lpmuo+9)
        bdl = q(lmuot+22)
        ecal2 = q(lpmuo+34)
        ecal4 = q(lpmuo+35)
        ecal6 = q(lpmuo+36)
        ft0 = q(lpmuo+24)
        impactb = q(lpmuo+41)
        impactn = q(lpmuo+57)
C- Get information about nearest jet (if any).
        call find_nearest_object(eta, phi, njet, ljets_good, 9, 8, 
     &    ljets_dr, dr)
        event_data(ntoff+1) = pt
        event_data(ntoff+2) = eta
        event_data(ntoff+3) = phi
        event_data(ntoff+4) = ifw4
        event_data(ntoff+5) = bdl
        event_data(ntoff+6) = ecal2
        event_data(ntoff+7) = ecal4
        event_data(ntoff+8) = ecal6
        event_data(ntoff+9) = ft0
        event_data(ntoff+10) = impactb
        event_data(ntoff+11) = impactn
        event_data(ntoff+12) = dr
        ntoff = ntoff + nmuon_var
      enddo
C-
C- Loop over soft muons.  Get quantities for ntuple.
C-
      call vzero(event_data(mutag_offset+1), num_mutag * nmutag_var)
      ntoff = mutag_offset
      do i = 1,mmutag
        lpmuo = lpmuo_soft(i)
        lmuot = lq(lpmuo-2)
        px = q(lpmuo+10)
        py = q(lpmuo+11)
        pz = q(lpmuo+12)
        p = q(lpmuo+13)
        pt = q(lpmuo+14)
        eta = q(lpmuo+16)
        phi = q(lpmuo+17)
        ifw4 = iq(lpmuo+9)
        bdl = q(lmuot+22)
        ecal2 = q(lpmuo+34)
        ecal4 = q(lpmuo+35)
        ecal6 = q(lpmuo+36)
        ft0 = q(lpmuo+24)
        impactb = q(lpmuo+41)
        impactn = q(lpmuo+57)
C- Get information about tagged jet (if any).
        call find_nearest_object(eta, phi, njet, ljets_good, 9, 8, 
     &    ljets_dr, dr)
        if(dr.lt.10.)then
C          call jet_corr(ljets_dr, jet_sys, je, jet, jeta, jphi, 
C     &      jpx, jpy, jpz)
          jpx = q(ljets_dr+2)
          jpy = q(ljets_dr+3)
          jpz = q(ljets_dr+4)
          jet = q(ljets_dr+6)
          jeta = q(ljets_dr+9)
          jphi = q(ljets_dr+8)
          jp = sqrt(jpx**2 + jpy**2 + jpz**2)
          ptrel = sqrt((px*jpy - py*jpx)**2 +
     &                 (py*jpz - pz*jpy)**2 +
     &                 (pz*jpx - px*jpz)**2)/jp
        else
          ptrel = 9999.
          jet = 0.
          jeta = 0.
          jphi = 0.
        endif
C- Good muon tag?
        good_tag = 0.
        if(dr.lt.1.)good_tag = 1.
        event_data(ntoff+1) = pt
        event_data(ntoff+2) = eta
        event_data(ntoff+3) = phi
        event_data(ntoff+4) = ifw4
        event_data(ntoff+5) = bdl
        event_data(ntoff+6) = ecal2
        event_data(ntoff+7) = ecal4
        event_data(ntoff+8) = ecal6
        event_data(ntoff+9) = ft0
        event_data(ntoff+10) = impactb
        event_data(ntoff+11) = impactn
        event_data(ntoff+12) = dr
        event_data(ntoff+13) = ptrel
        event_data(ntoff+14) = jet
        event_data(ntoff+15) = jeta
        event_data(ntoff+16) = jphi
        event_data(ntoff+17) = good_tag
        ntoff = ntoff + nmutag_var
      enddo
C-
C- W boson analysis
C- Loop over electrons.  Get quantities for ntuple.
C-
      if(do_electron_w_analysis)then
        call vzero(event_data(w_offset+1), num_w * nw_var)
        ntoff = w_offset
        do i = 1,min(nelec, num_w)
          lpelc = lpelc_good(i)
          px = q(lpelc+3)
          py = q(lpelc+4)
          pz = q(lpelc+5)
          e  = q(lpelc+6)
          et = q(lpelc+7)
          phi = q(lpelc+10)
          wmt2 = sqrt(2*et*met2*(1.-cos(phi-mphi2)))
          wmt3 = sqrt(2*et*met3*(1.-cos(phi-mphi3)))
          wpt2 = sqrt((px+metx)**2 + (py+mety)**2)
          wpt3 = sqrt((px+metx3)**2 + (py+mety3)**2)
          event_data(ntoff+1) = wmt2
          event_data(ntoff+2) = wmt3
          event_data(ntoff+3) = wpt2
          event_data(ntoff+4) = wpt3
          ntoff = ntoff + nw_var
          if(do_electron_w_reconstruction)then
            p4(1) = px
            p4(2) = py
            p4(3) = pz
            p4(4) = e
            if(w_use_met3)then
              p2(1) = metx3
              p2(2) = mety3
            else
              p2(1) = metx
              p2(2) = mety
            endif
            call find_wlnu(mw, p4, p2, pw4, pzw2, ok)
            if(i.eq.1)then
              pxw = pw4(1)
              pyw = pw4(2)
              pzw = pw4(3)
              ew = pw4(4)
            endif
C- Replace electron 4-vector with W 4-vector in list.
            if(i.le.max_part)then
              pv(1, i) = pw4(1)
              pv(2, i) = pw4(2)
              pv(3, i) = pw4(3)
              pv(4, i) = pw4(4)
            endif
          endif
        enddo
      endif
C-
C- Z boson analysis.  Use only two leading electrons.
C-
      if(do_electron_z_analysis)then
        call vzero(event_data(z_offset+1), num_z * nz_var)
        ntoff = z_offset
        if(nelec.ge.2)then
          lpelc = lpelc_good(1)
          lpelc2 = lpelc_good(2)
          pxz = q(lpelc+3) + q(lpelc2+3)
          pyz = q(lpelc+4) + q(lpelc2+4)
          pzz = q(lpelc+5) + q(lpelc2+5)
          ez  = q(lpelc+6) + q(lpelc2+6)
          zmass = sqrt(ez**2 - pxz**2 - pyz**2 - pzz**2)
          zpt = sqrt(pxz**2 + pyz**2)
          event_data(ntoff+1) = zmass
          event_data(ntoff+2) = zpt
          ntoff = ntoff + nz_var
          if(do_electron_z_reconstruction)then
C- Replace first electron 4-vector with Z 4-vector in list.
            if(max_part.ge.1)then
              pv(1, 1) = pxz
              pv(2, 1) = pyz
              pv(3, 1) = pzz
              pv(4, 1) = ez
            endif
C- Zero second electron 4-vector.
            if(max_part.ge.1)then
              pv(1, 2) = 0.
              pv(2, 2) = 0.
              pv(3, 2) = 0.
              pv(4, 2) = 0.
            endif
          endif
        endif
      endif
C-
C- Event shape
C-
      call sphericity0(npart, pv, 0, s0, y0, a0, eigval, eigvec, 
     &  ptot, mass)
      if(npart.le.2)a0 = 0.
      call sphericity0(mjet, pjet, 0, s1, y1, a1, eigval, eigvec, 
     &  ptot, mass)
      if(npart.le.2)a1 = 0.
C-
C- 2-jet invariant masses
C-
      mj12 = 0.
      mj13 = 0.
      mj23 = 0.
      if(njet.ge.2)then
        mj12 = psqrt((pjet(4,1)+pjet(4,2))**2
     &             - (pjet(1,1)+pjet(1,2))**2
     &             - (pjet(2,1)+pjet(2,2))**2
     &             - (pjet(3,1)+pjet(3,2))**2)
      endif
      if(njet.ge.3)then
        mj13 = psqrt((pjet(4,1)+pjet(4,3))**2
     &             - (pjet(1,1)+pjet(1,3))**2
     &             - (pjet(2,1)+pjet(2,3))**2
     &             - (pjet(3,1)+pjet(3,3))**2)
        mj23 = psqrt((pjet(4,2)+pjet(4,3))**2
     &             - (pjet(1,2)+pjet(1,3))**2
     &             - (pjet(2,2)+pjet(2,3))**2
     &             - (pjet(3,2)+pjet(3,3))**2)
      endif
      mjbest = mj12
      if(abs(mj13-mw).lt.abs(mjbest-mw))mjbest = mj13
      if(abs(mj23-mw).lt.abs(mjbest-mw))mjbest = mj23
C-
C- Gamma-jet invariant masses
C-
      mgj1 = 0.
      mgj2 = 0.
      mgj3 = 0.
      if(nphot+nelec.ge.1)then
        n = 1
        if(njet.ge.1)mgj1 = 
     &    psqrt((pv(4,n)+pjet(4,1))**2
     &        - (pv(1,n)+pjet(1,1))**2
     &        - (pv(2,n)+pjet(2,1))**2
     &        - (pv(3,n)+pjet(3,1))**2)
        if(njet.ge.2)mgj2 = 
     &    psqrt((pv(4,n)+pjet(4,2))**2
     &        - (pv(1,n)+pjet(1,2))**2
     &        - (pv(2,n)+pjet(2,2))**2
     &        - (pv(3,n)+pjet(3,2))**2)
        if(njet.ge.3)mgj3 = 
     &    psqrt((pv(4,n)+pjet(4,3))**2
     &        - (pv(1,n)+pjet(1,3))**2
     &        - (pv(2,n)+pjet(2,3))**2
     &        - (pv(3,n)+pjet(3,3))**2)
      endif
      mg2j1 = 0.
      mg2j2 = 0.
      if(nphot+nelec.ge.2)then
        n = 2
        if(njet.ge.1)mg2j1 = 
     &    psqrt((pv(4,n)+pjet(4,1))**2
     &        - (pv(1,n)+pjet(1,1))**2
     &        - (pv(2,n)+pjet(2,1))**2
     &        - (pv(3,n)+pjet(3,1))**2)
        if(njet.ge.2)mg2j2 = 
     &    psqrt((pv(4,n)+pjet(4,2))**2
     &        - (pv(1,n)+pjet(1,2))**2
     &        - (pv(2,n)+pjet(2,2))**2
     &        - (pv(3,n)+pjet(3,2))**2)
      endif
C-
C- Gamma-gamma invariant mass
C-
      mgg = 0.
      if(nphot+nelec.ge.2)then
        n = 1
        mgg = psqrt((pv(4,n)+pv(4,n+1))**2
     &            - (pv(1,n)+pv(1,n+1))**2
     &            - (pv(2,n)+pv(2,n+1))**2
     &            - (pv(3,n)+pv(3,n+1))**2)
      endif
C-
C- Total mass, x1, x2, xf.
C-
      pxtot = 0.
      pytot = 0.
      pztot = 0.
      etot = 0.
      do i = 1,npart
        pxtot = pxtot + pv(1,i)
        pytot = pytot + pv(2,i)
        pztot = pztot + pv(3,i)
        etot = etot + pv(4,i)
      enddo
      mass = psqrt(etot**2 - pxtot**2 - pytot**2 - pztot**2)
      tau = mass**2/s
      xf = 2.*pztot/sqrt(s)
      x1 = 0.5*( xf + sqrt(xf**2 + 4.*tau))
      x2 = 0.5*(-xf + sqrt(xf**2 + 4.*tau))
C-
C- W + 4-jet mass and delta-mass analysis.
C-
      if(njet.ge.3)then
        mj123 = psqrt( (pjet(4,1)+pjet(4,2)+pjet(4,3))**2
     &                -(pjet(1,1)+pjet(1,2)+pjet(1,3))**2
     &                -(pjet(2,1)+pjet(2,2)+pjet(2,3))**2
     &                -(pjet(3,1)+pjet(3,2)+pjet(3,3))**2)
      else
        mj123 = 0.
      endif
      if(nelec.eq.1 .and. njet.ge.4)then
        mwj1 = psqrt( (ew+pjet(4,1))**2 - (pxw+pjet(1,1))**2
     &             - (pyw+pjet(2,1))**2 - (pzw+pjet(3,1))**2)
        mwj2 = psqrt( (ew+pjet(4,2))**2 - (pxw+pjet(1,2))**2
     &             - (pyw+pjet(2,2))**2 - (pzw+pjet(3,2))**2)
        mwj3 = psqrt( (ew+pjet(4,3))**2 - (pxw+pjet(1,3))**2
     &             - (pyw+pjet(2,3))**2 - (pzw+pjet(3,3))**2)
        mwj4 = psqrt( (ew+pjet(4,4))**2 - (pxw+pjet(1,4))**2
     &             - (pyw+pjet(2,4))**2 - (pzw+pjet(3,4))**2)
        mj124 = psqrt( (pjet(4,1)+pjet(4,2)+pjet(4,4))**2
     &                -(pjet(1,1)+pjet(1,2)+pjet(1,4))**2
     &                -(pjet(2,1)+pjet(2,2)+pjet(2,4))**2
     &                -(pjet(3,1)+pjet(3,2)+pjet(3,4))**2)
        mj134 = psqrt( (pjet(4,1)+pjet(4,3)+pjet(4,4))**2
     &                -(pjet(1,1)+pjet(1,3)+pjet(1,4))**2
     &                -(pjet(2,1)+pjet(2,3)+pjet(2,4))**2
     &                -(pjet(3,1)+pjet(3,3)+pjet(3,4))**2)
        mj234 = psqrt( (pjet(4,2)+pjet(4,3)+pjet(4,4))**2
     &                -(pjet(1,2)+pjet(1,3)+pjet(1,4))**2
     &                -(pjet(2,2)+pjet(2,3)+pjet(2,4))**2
     &                -(pjet(3,2)+pjet(3,3)+pjet(3,4))**2)
        dmt4 = 1.e6
        if(abs(mwj1-mj234).lt.abs(dmt4))then
          mt4 = 0.5*(mwj1+mj234)
          dmt4 = mwj1-mj234
        endif
        if(abs(mwj2-mj134).lt.abs(dmt4))then
          mt4 = 0.5*(mwj2+mj134)
          dmt4 = mwj2-mj134
        endif
        if(abs(mwj3-mj124).lt.abs(dmt4))then
          mt4 = 0.5*(mwj3+mj124)
          dmt4 = mwj3-mj124
        endif
        if(abs(mwj4-mj123).lt.abs(dmt4))then
          mt4 = 0.5*(mwj4+mj123)
          dmt4 = mwj4-mj123
        endif
      else
        mt4 = 0.
        dmt4 = 1.e6
      endif
C-
C- B-prime invarient mass analysis.
C-
      mbp3 = 0.
      dmbp3 = 1.e6
      if(nphot+nelec.eq.1 .and. njet.ge.3)then
        mbp3 = 0.
        dmbp3 = 1.e6
        if(abs(mgj1-mj23).lt.abs(dmbp3))then
          mbp3 = 0.5*(mgj1+mj23)
          dmbp3 = mgj1-mj23
        endif
        if(abs(mgj2-mj13).lt.abs(dmbp3))then
          mbp3 = 0.5*(mgj2+mj13)
          dmbp3 = mgj2-mj13
        endif
        if(abs(mgj3-mj12).lt.abs(dmbp3))then
          mbp3 = 0.5*(mgj3+mj12)
          dmbp3 = mgj3-mj12
        endif
      endif
      if(nphot+nelec.eq.2 .and. njet.ge.2)then
        mbp3 = 0.
        dmbp3 = 1.e6
        if(abs(mgj1-mg2j2).lt.abs(dmbp3))then
          mbp3 = 0.5*(mgj1+mg2j2)
          dmbp3 = mgj1-mg2j2
        endif
        if(abs(mgj2-mg2j1).lt.abs(dmbp3))then
          mbp3 = 0.5*(mgj2+mg2j1)
          dmbp3 = mgj2-mg2j1
        endif
      endif
C-
C- Fill global variable vector
C-
      event_data(1) = run
      event_data(2) = event
      event_data(3) = weight
      event_data(4) = uwght
      event_data(5) = dwght
      event_data(6) = nelec
      event_data(7) = nphot
      event_data(8) = njet
      event_data(9) = nmuon
      event_data(10) = nmutag
      event_data(11) = met2u
      event_data(12) = mphi2u
      event_data(13) = met2
      event_data(14) = mphi2
      event_data(15) = met3
      event_data(16) = mphi3
      event_data(17) = ht
      if(sum_e.gt.0.)then
        event_data(18) = sum_et/sum_e
      else
        event_data(18) = 0.
      endif
      if(clncal)then
        event_data(19) = 1.
      else
        event_data(19) = 0.
      endif
      ievent_data(20) = trig
      event_data(21) = nvert
      event_data(22) = zvert
      if(mcdata)then
        event_data(23) = 1.
      else
        event_data(23) = 0.
      endif
      event_data(24) = zmcvtx
      event_data(25) = nt
      event_data(26) = nb
      event_data(27) = nc
      event_data(28) = ns
      event_data(29) = nd
      event_data(30) = nu
      event_data(31) = ng
      event_data(32) = ne
      event_data(33) = nmu
      event_data(34) = ntau
      event_data(35) = ngi
      event_data(36) = nui
      event_data(37) = ndi
      event_data(38) = nsi
      event_data(39) = nci
      event_data(40) = nbi
      event_data(41) = set
      event_data(42) = setc
      event_data(43) = sete
      event_data(44) = setmr
      event_data(45) = hot_e
      event_data(46) = hot_et
      event_data(47) = njet10
      event_data(48) = njet15
      event_data(49) = njet20
      event_data(50) = njet25
      event_data(51) = njet30
      event_data(52) = njet35
      event_data(53) = njet40
      event_data(54) = mj12
      event_data(55) = mj13
      event_data(56) = mj23
      event_data(57) = mjbest
      event_data(58) = mj123
      event_data(59) = mass
      event_data(60) = xf
      event_data(61) = x1
      event_data(62) = x2
      event_data(63) = mt4
      event_data(64) = dmt4
      event_data(65) = s0
      event_data(66) = y0
      event_data(67) = a0
      event_data(68) = s1
      event_data(69) = y1
      event_data(70) = a1
      event_data(71) = mgg
      event_data(72) = mgj1
      event_data(73) = mgj2
      event_data(74) = mgj3
      event_data(75) = mbp3
      event_data(76) = dmbp3
      if(column_wise)then
        call hfnt(1)
      else
        call hfn(1, event_data)
      endif
      if(iquest(1).ne.0)call errmsg('TOP_BTAG', 'TOP_BTAG', 
     &  'Error filling ntuple', 'F')
      num_ntuple = num_ntuple + 1
C-
C- Final selection cuts
C-
      if(do_final_selection)then
        if(    nelec.lt.num_elec_min_final
     &    .or. nphot.lt.num_phot_min_final
     &    .or. nmuon.lt.num_muon_min_final
     &    .or. nmutag.lt.num_mutag_min_final
     &    .or. met2.lt.met2_min_final
     &    .or. met3.lt.met3_min_final)then
          top_btag = .false.
        endif
        jet1 = 0.
        jet2 = 0.
        jet3 = 0.
        jet4 = 0.
        if(njet.ge.1)jet1 = event_data(jet_offset + 3)
        if(njet.ge.2)jet2 = event_data(jet_offset + njet_var + 3)
        if(njet.ge.3)jet3 = event_data(jet_offset + 2*njet_var + 3)
        if(njet.ge.4)jet4 = event_data(jet_offset + 3*njet_var + 3)
        if(jet1.lt.jet1_etmin_final .or.
     &     jet2.lt.jet2_etmin_final .or. 
     &     jet3.lt.jet3_etmin_final .or. 
     &     jet4.lt.jet4_etmin_final)then
          top_btag = .false.
        endif
      endif
      if(top_btag)num_output = num_output + 1
      goto 999

      ENTRY TOP_BTAG_END()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of run processing.
C-
C-   Returned value  : true
C-
C-   Created  14-May-1993   Herbert Greenlee
C-
C----------------------------------------------------------------------
      top_btag_end=.true.
C-
C- Flush and then delete ntuple.
C-
      call hcdir(ntuple_path, ' ')
      if(iquest(1).eq.0)call hcdir(rz_path, ' ')
      if(iquest(1).eq.0)call hrout(1, icycle, ' ')
      if(iquest(1).eq.0)call hrend(rz_top_dir)
      if(iquest(1).eq.0.and..not.column_wise)call hdelet(1)
      if(iquest(1).ne.0)call errmsg('TOP_BTAG', 'TOP_BTAG',
     &  'Error closing ntuple', 'F')
C-
C- Print statistics
C-
      lun = ssunit()
      print 500, num_input, num_ntuple, num_output
      write(lun,500)num_input, num_ntuple, num_output
 500  format(/' TOP_BTAG package statistics:'/
     &  /1x,i8,' Events processed'
     &  /1x,i8,' Events stored in Ntuple'
     &  /1x,i8,' Events selected'/)
      go to 999

 999  return
      end
