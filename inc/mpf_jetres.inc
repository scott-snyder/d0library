C--------------------------------------------------------------------------------
C-                        MPF_JETRES.INC (version 4.2)
C-
C-  Include file for Missing Et Projection Fraction (MPF) analysis which controls
C-      ntuple parameters and counting variables.  In all cases except PTAU's,
C-      the objects are ordered in Et.  For PTAU entries, the PTAU's are Et
C-      ordered and then are followed by 0.5 cone jets which do not overlap the
C-      PTAU's.
C-
C-  GLOBAL EVENT QUANTITIES --
C-           runnum         : run #
C-           eventnum       : event #
C-           met            : uncorrected missing Et (pnut)
C-           mex            : "                 " Etx   "
C-           mey            : "                 " Ety   "
C-           z_vert         : z coordinate of the primary vertex
C-           isa_zv         : vertex of mc generator
C-           filter         : bit pattern designating user defined filters fired
C-           inv_mass       : invariant mass of di-emcluster
C-           mpf            : missing Et projection fraction for event
C-           mu_pt1         : leading muon pt
C-           mi_flag        : multiple-interaction flag
C-           ncaid          : # CAID banks in event
C-           scalr_et       : scalar Et
C-           ublank         : main-ring status word
C-           met_phi        : phi of Missing Et
C-           reco_vrs       : reco version plus pass release
C-           lumin          : instantaneous luminosity at event
C-           mc_etthresh    : ptmin for MC generator
C-           mpf_z          : mpf in di-emcluster events
C-           offset         : noise + underlying event
C-           et_prtn        : Et of parton event generated at
C-           kt             : event kt boost
C-           kt_phi         : phi of event kt
C-           soft_et        : Et of soft recoil in event (not including jets)
C-           soft_ex        : Ex "  "
C-           soft_ey        : Ey "  "
C-           soft_phi       : phi "  "
C-           mex_icd        : uncorrected ICD missing Etx
C-           mey_icd        : uncorrected ICD missing Ety
C-           ht_em          : sum of em object et
C-           z_e            : energy of Z boson
C-           z_et           : Et of Z boson
C-           z_eta          : eta of Z boson
C-           Z_phi          : phi of Z boson
C-           drell_yan      : .TRUE. if 2nd EM cluster significant compared to
C-                            leading cluster, .FALSE. if 'single' cluster evt.
C-
C-  RECONSTRUCTED PHOTON QUANTITIES -- (k) indicates order in Et
C-           n_emc          : # of reconstructed em clusters (PELC/PPHO)
C-                            passing quality cuts in MPF_JET_RESPONSE.RCP
C-           emc_et(k)      : Et of EM clusters
C-           emc_epr(k)     : Et of parton corresponding to EM cluster
C-           emc_e(k)       : E
C-           emc_ex(k)      : Ex
C-           emc_ey(k)      : Ey
C-           emc_eta(k)     : physics eta
C-           emc_deta(k)    : detector eta
C-           emc_phi(k)     : phi
C-
C-           emc_emf(k)     : em fraction of photon
C-           emc_fiso(k)    : (total E(0.4)-EM E(0.2))/EM E(0.2)
C-           emc_ecor(k)    : tot E in R <= 0.2
C-           emc_emcor(k)   : em E in R <= 0.2
C-           emc_eiso(k)    : tot E in R <= 0.4
C-           emc_emiso(k)   : em E in R <= 0.4
C-           emc_eti7(k)    : E in physics isol cone (R = 0.7)
C-
C-           emc_stat(k)    : CLEANEM status word
C-           emc_chsq(k)    : H-matrix chi-squared
C-           emc_dedx(k)    : dE/dx of CDC/FDC track matching em cluster
C-           emc_trkm(k)    : trackmatch significance of track matching em clus.
C-           emc_ncen(k)    : em E in cluster w/o central tower
C-           emc_clus(k)    : .TRUE. if PELC, .FALSE. if PPHO
C-           emc_treff(k)   : TRD eff. from trunc. sum (-1 if acc.eq.0)
C-
C-  RECONSTRUCTED JET QUANTITIES -- jets which do not correspond to selected
C-      good EM objects in event.  (i) indicates order in Et, k indicates
C-      algorithm used (1=0.7, 2=0.5, 3=0.3, 4=NN, 5=1.0, and 6=PTAU)
C-
C-           njet(k)        : # reconstructed jets that are not good em objects
C-           eprime(k)      : photon Et*cosh(jet eta)
C-           del_phi(k)     : delta phi of leading jet and photon
C-           mpf_jet(k)     : met-dot-n_hat_jet/et_jet (old mpf definition)
C-           ht(k)          : Ht sum
C-
C-           jt_et'i'(k)     : Et
C-           jt_ep'i'(k)     : Et of parton corresponding to leading jet
C-           jt_e'i'(k)      : E
C-           jt_ex'i'(k)     : Ex
C-           jt_ey'i'(k)     : Ey
C-           jt_eta'i'(k)    : physics eta
C-           jt_deta'i'(k)   : detector eta
C-           jt_phi'i'(k)    : phi
C-
C-           jt_emf'i'(k)    : EM fraction
C-           jt_msp'i'(k)    : merge/split flag
C-           jt_rms'i'(k)    : RMS in eta-phi
C-           jt_ntr'i'(k)    : # of tracks in jet
C-           jt_ncl'i'(k)    : # cells above threshold
C-           jt_icd'i'(k)    : ICD fraction
C-           jt_chf'i'(k)    : coarse hadronic fraction
C-           jt_mxc'i'(k)    : maximum cell ratio
C-
C-  Created   MAR-04-1994   Bob Kehoe
C-  Updated   May-25-1994   Bob Kehoe -- add several words to ntuple
C-  Updated   Feb-20-1995   Bob Kehoe -- mi_flag, ncaid, scalar et,
C-                                       microblank, and luminosity added
C-  Updated   Apr-20-1995   Chip Stewart, Bob Kehoe   (CWNized)
C-  Updated   Oct-18-1995   Bob Kehoe -- change to mpf_jetres.inc for IBM's
C----------------------------------------------------------------------

      INTEGER nbin_et,limit,num_algo,nmax_e,nt_id,iuser
      real epsilon
      PARAMETER (nbin_et = 100)
      PARAMETER (limit = 20)
      PARAMETER (num_algo = 6)
      PARAMETER (nmax_e = 450)
      PARAMETER (nt_id = 3)
      PARAMETER (iuser = 967)
      PARAMETER (epsilon = 0.000001)
      COMMON /mpf_jetres/ nevt,npass,evsize,tdir,
     &  ntup_file,filedir,pawcdir,unit,title
      INTEGER nevt,npass,evsize,unit
      CHARACTER*80 tdir,ntup_file,pawcdir,filedir,title
C-
      COMMON/mpf_event/ runnum,eventnum,met,mex,mey,z_vert,
     &  isa_zv,filter,inv_mass,mpf,mu_pt1,mi_flag,ncaid,
     &  scalr_et,ublank,met_phi,reco_vrs,lumin,mc_etthresh,
     &  mpf_z,offset
      INTEGER runnum,eventnum,mi_flag,ncaid,ublank,filter
      REAL met,mex,mey,z_vert,isa_zv,inv_mass,mpf,mu_pt1
      REAL scalr_et,met_phi,reco_vrs,lumin,mc_etthresh,mpf_z
      REAL offset
      CHARACTER*255 mpf_event_chform
      parameter (mpf_event_chform = 'runnum:I,eventnum:I,'//
     &   'met:R,mex:R,mey:R,z_vert:r,isa_zv:r,filter:i,'//
     &   'inv_mass:R,mpf:R,mu_pt1:R,mi_flag:i,ncaid:i,'//
     &   'scalr_et:r,ublank:I,met_phi:R,reco_vrs:r,lumin:R,'//
     &   'mc_etthresh:r,mpf_z:r,offset:r')
c***
      COMMON/mpf_global/ et_prtn,kt,kt_phi,soft_et,soft_ex,
     &  soft_ey,soft_phi,mex_icd,mey_icd,ht_em,
     &  z_e,z_et,z_eta,z_phi,drell_yan
      REAL et_prtn,kt,kt_phi,soft_et,soft_ex,soft_ey
      REAL soft_phi,mex_icd,mey_icd,ht_em
      REAL z_e,z_et,z_eta,z_phi
      CHARACTER*255 mpf_global_chform
      LOGICAL drell_yan
      parameter (mpf_global_chform = 'et_prtn:r,kt:R,'//
     &    'kt_phi:R,soft_et:r,soft_ex:r,soft_ey:r,'//
     &    'soft_phi:r,mex_icd:R,mey_icd:R,ht_em:r,'//
     &    'z_e:r,z_et:r,z_eta:r,z_phi:r,drell_yan:L')
c***
      COMMON/mpf_emvector/n_emc,
     &  emc_et(2),emc_epr(2),emc_e(2),emc_ex(2),
     &  emc_ey(2),emc_eta(2),emc_deta(2),emc_phi(2)
      INTEGER n_emc
      REAL emc_et,emc_epr,emc_e,emc_ex
      REAL emc_ey,emc_eta,emc_deta,emc_phi
      CHARACTER*255 mpf_emvector_chform
      parameter (mpf_emvector_chform = 'n_emc[0,2]:i,'//
     &    'emc_et(n_emc):r,emc_epr(n_emc):r,'//
     &    'emc_e(n_emc):r,emc_ex(n_emc):r,'//
     &    'emc_ey(n_emc):r,emc_eta(n_emc):r,'//
     &    'emc_deta(n_emc):r,emc_phi(n_emc):r')
c***
      common/mpf_emiso/nele2,emc_emf(2),emc_fiso(2),
     &  emc_ecor(2),emc_emcor(2),emc_eiso(2),emc_emiso(2),
     &  emc_eti7(2)
      integer nele2
      real emc_emf,emc_fiso,emc_ecor
      real emc_emcor,emc_eiso,emc_emiso,emc_eti7
      character*255 mpf_emiso_chform
      parameter (mpf_emiso_chform = 'nele2[0,2]:i,'//
     &    'emc_emf(nele2):r,emc_fiso(nele2):r,'//
     &    'emc_ecor(nele2):r,emc_emcor(nele2):r,'//
     &    'emc_eiso(nele2):r,emc_emiso(nele2):r,'//
     &    'emc_eti7(nele2):r')
c***
      common/mpf_emquality/nele3,emc_stat(2),emc_chsq(2),
     &  emc_dedx(2),emc_trkm(2),emc_ncen(2),
     &  emc_clus(2),emc_treff(2)
      integer nele3,emc_stat
      real emc_chsq,emc_dedx,emc_trkm,emc_ncen,emc_treff
      character*255 mpf_emquality_chform
      logical emc_clus
      parameter (mpf_emquality_chform = 'nele3[0,2]:i,'//
     &    'emc_stat(nele3):i,emc_chsq(nele3):r,'//
     &    'emc_dedx(nele3):r,emc_trkm(nele3):r,'//
     &    'emc_ncen(nele3):r,emc_clus(nele3):L,'//
     &    'emc_treff(nele3):r')
c***
      COMMON /mpf_jetalgo/algo,eprime(6),del_phi(6),
     &  mpf_jet(6),ht(6)
      INTEGER algo
      REAL eprime,del_phi,mpf_jet,ht
      CHARACTER*255 mpf_jetalgo_chform
      parameter (mpf_jetalgo_chform = 'algo[0,6]:i,'//
     &    'eprime(algo):r,del_phi(algo):r,'//
     &    'mpf_jet(algo):r,ht(algo):r')
c***
      COMMON /mpf_jet7/nj7,
     &  jt_et7(5),jt_epr7(5),jt_e7(5),jt_ex7(5),jt_ey7(5),
     &  jt_eta7(5),jt_deta7(5),jt_phi7(5),
     &  jt_emf7(5),jt_msp7(5),jt_rms7(5),jt_ntr7(5),
     &  jt_ncl7(5),jt_icd7(5),jt_chf7(5),jt_mxc7(5)
      INTEGER nj7,jt_msp7,jt_ntr7,jt_ncl7
      REAL jt_et7,jt_epr7,jt_e7,jt_ex7,jt_ey7,jt_eta7
      REAL jt_deta7,jt_phi7
      REAL jt_emf7,jt_rms7,jt_icd7,jt_chf7,jt_mxc7
      CHARACTER*255 mpf_jetinfo7
      parameter (mpf_jetinfo7 = 'nj7[0,5]:i,jt_et7(nj7):r,'//
     &    'jt_epr7(nj7):r,jt_e7(nj7):r,jt_ex7(nj7):r,'//
     &    'jt_ey7(nj7):r,jt_eta7(nj7):r,jt_deta7(nj7):r,'//
     &    'jt_phi7(nj7):r,jt_emf7(nj7):r,jt_msp7(nj7):i,'//
     &    'jt_rms7(nj7):r,jt_ntr7(nj7):i,jt_ncl7(nj7):i,'//
     &    'jt_icd7(nj7):r,jt_chf7(nj7):r,jt_mxc7(nj7):r')
c***
      COMMON /mpf_jet5/nj5,
     &  jt_et5(5),jt_epr5(5),jt_e5(5),jt_ex5(5),jt_ey5(5),
     &  jt_eta5(5),jt_deta5(5),jt_phi5(5),
     &  jt_emf5(5),jt_msp5(5),jt_rms5(5),jt_ntr5(5),
     &  jt_ncl5(5),jt_icd5(5),jt_chf5(5),jt_mxc5(5)
      INTEGER nj5,jt_msp5,jt_ntr5,jt_ncl5
      REAL jt_et5,jt_epr5,jt_e5,jt_ex5,jt_ey5,jt_eta5
      REAL jt_deta5,jt_phi5
      REAL jt_emf5,jt_rms5,jt_icd5,jt_chf5,jt_mxc5
      CHARACTER*255 mpf_jetinfo5
      parameter (mpf_jetinfo5 = 'nj5[0,5]:i,jt_et5(nj5):r,'//
     &    'jt_epr5(nj5):r,jt_e5(nj5):r,jt_ex5(nj5):r,'//
     &    'jt_ey5(nj5):r,jt_eta5(nj5):r,jt_deta5(nj5):r,'//
     &    'jt_phi5(nj5):r,jt_emf5(nj5):r,jt_msp5(nj5):i,'//
     &    'jt_rms5(nj5):r,jt_ntr5(nj5):i,jt_ncl5(nj5):i,'//
     &    'jt_icd5(nj5):r,jt_chf5(nj5):r,jt_mxc5(nj5):r')
c***
      COMMON /mpf_jet3/nj3,
     &  jt_et3(5),jt_epr3(5),jt_e3(5),jt_ex3(5),jt_ey3(5),
     &  jt_eta3(5),jt_deta3(5),jt_phi3(5),
     &  jt_emf3(5),jt_msp3(5),jt_rms3(5),jt_ntr3(5),
     &  jt_ncl3(5),jt_icd3(5),jt_chf3(5),jt_mxc3(5)
      INTEGER nj3,jt_msp3,jt_ntr3,jt_ncl3
      REAL jt_et3,jt_epr3,jt_e3,jt_ex3,jt_ey3,jt_eta3
      REAL jt_deta3,jt_phi3
      REAL jt_emf3,jt_rms3,jt_icd3,jt_chf3,jt_mxc3
      CHARACTER*255 mpf_jetinfo3
      parameter (mpf_jetinfo3 = 'nj3[0,5]:i,jt_et3(nj3):r,'//
     &    'jt_epr3(nj3):r,jt_e3(nj3):r,jt_ex3(nj3):r,'//
     &    'jt_ey3(nj3):r,jt_eta3(nj3):r,jt_deta3(nj3):r,'//
     &    'jt_phi3(nj3):r,jt_emf3(nj3):r,jt_msp3(nj3):i,'//
     &    'jt_rms3(nj3):r,jt_ntr3(nj3):i,jt_ncl3(nj3):i,'//
     &    'jt_icd3(nj3):r,jt_chf3(nj3):r,jt_mxc3(nj3):r')
c***
      COMMON /mpf_jetn/njn,
     &  jt_etn(5),jt_eprn(5),jt_en(5),jt_exn(5),jt_eyn(5),
     &  jt_etan(5),jt_detan(5),jt_phin(5),
     &  jt_emfn(5),jt_mspn(5),jt_rmsn(5),jt_ntrn(5),
     &  jt_ncln(5),jt_icdn(5),jt_chfn(5),jt_mxcn(5)
      INTEGER njn,jt_mspn,jt_ntrn,jt_ncln
      REAL jt_etn,jt_eprn,jt_en,jt_exn,jt_eyn,jt_etan
      REAL jt_detan,jt_phin
      REAL jt_emfn,jt_rmsn,jt_icdn,jt_chfn,jt_mxcn
      CHARACTER*255 mpf_jetinfon
      parameter (mpf_jetinfon = 'njn[0,5]:i,jt_etn(njn):r,'//
     &    'jt_eprn(njn):r,jt_en(njn):r,jt_exn(njn):r,'//
     &    'jt_eyn(njn):r,jt_etan(njn):r,jt_detan(njn):r,'//
     &    'jt_phin(njn):r,jt_emfn(njn):r,jt_mspn(njn):i,'//
     &    'jt_rmsn(njn):r,jt_ntrn(njn):i,jt_ncln(njn):i,'//
     &    'jt_icdn(njn):r,jt_chfn(njn):r,jt_mxcn(njn):r')
c***
      COMMON /mpf_jet10/nj1,
     &  jt_et1(5),jt_epr1(5),jt_e1(5),jt_ex1(5),jt_ey1(5),
     &  jt_eta1(5),jt_deta1(5),jt_phi1(5),
     &  jt_emf1(5),jt_msp1(5),jt_rms1(5),jt_ntr1(5),
     &  jt_ncl1(5),jt_icd1(5),jt_chf1(5),jt_mxc1(5)
      INTEGER nj1,jt_msp1,jt_ntr1,jt_ncl1
      REAL jt_et1,jt_epr1,jt_e1,jt_ex1,jt_ey1,jt_eta1
      REAL jt_deta1,jt_phi1
      REAL jt_emf1,jt_rms1,jt_icd1,jt_chf1,jt_mxc1
      CHARACTER*255 mpf_jetinfo10
      parameter (mpf_jetinfo10 = 'nj1[0,5]:i,jt_et1(nj1):r,'//
     &    'jt_epr1(nj1):r,jt_e1(nj1):r,jt_ex1(nj1):r,'//
     &    'jt_ey1(nj1):r,jt_eta1(nj1):r,jt_deta1(nj1):r,'//
     &    'jt_phi1(nj1):r,jt_emf1(nj1):r,jt_msp1(nj1):i,'//
     &    'jt_rms1(nj1):r,jt_ntr1(nj1):i,jt_ncl1(nj1):i,'//
     &    'jt_icd1(nj1):r,jt_chf1(nj1):r,jt_mxc1(nj1):r')
c***
      COMMON /mpf_jett/njt,
     &  jt_ett(5),jt_eprt(5),jt_nrgt(5),jt_ext(5),jt_eyt(5),
     &  jt_etat(5),jt_detat(5),jt_phit(5),
     &  jt_emft(5),jt_mspt(5),jt_rmst(5),jt_ntrt(5),
     &  jt_nclt(5),jt_icdt(5),jt_chft(5),jt_mxct(5)
      INTEGER njt,jt_mspt,jt_ntrt,jt_nclt
      REAL jt_ett,jt_eprt,jt_nrgt,jt_ext,jt_eyt,jt_etat
      REAL jt_detat,jt_phit
      REAL jt_emft,jt_rmst,jt_icdt,jt_chft,jt_mxct
      CHARACTER*255 mpf_jetinfot
      parameter (mpf_jetinfot = 'njt[0,5]:i,jt_ett(njt):r,'//
     &    'jt_eprt(njt):r,jt_nrgt(njt):r,jt_ext(njt):r,'//
     &    'jt_eyt(njt):r,jt_etat(njt):r,jt_detat(njt):r,'//
     &    'jt_phit(njt):r,jt_emft(njt):r,jt_mspt(njt):i,'//
     &    'jt_rmst(njt):r,jt_ntrt(njt):i,jt_nclt(njt):i,'//
     &    'jt_icdt(njt):r,jt_chft(njt):r,jt_mxct(njt):r')
