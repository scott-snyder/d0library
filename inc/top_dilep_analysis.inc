      INTEGER nwant_ele, nwant_phot, nwant_muon, nwant_jet
      INTEGER ntot_ele,  ntot_phot, ntot_muon, ntot_jet
      INTEGER nelec,nphot, nmuon, njet, max_particle, mvar,nvar
      INTEGER nelec_tght,ntot_eletght,nwant_eletght,nwant_met
      INTEGER ngam_tght,ntot_gamtght,nwant_gamtght,nmuon_tght
      INTEGER nvar_elec,nvar_jet,nvar_muon,nvar_met,nvar_phot
      INTEGER nwant_pmuo, ntot_pmuo, nwant_muon_tght, ntot_muon_tght
      INTEGER nwant_muisa, nvar_muisa, nwant_l2muon, nvar_l2muon
      INTEGER nwant_l2jet, nvar_l2jet, nwant_bbisa, nvar_bbisa
      INTEGER nwant_l1muon, nwant_l1jet
      INTEGER nvar_l1met, nvar_l2met
      PARAMETER( nwant_ele  = 3 )
      PARAMETER( nwant_eletght = 3 )
      PARAMETER( nvar_elec = 53 )
      PARAMETER( nwant_phot = 3 )
      PARAMETER( nwant_gamtght = 3 )
      PARAMETER( nvar_phot = 53 )
      PARAMETER( nwant_muon = 6 )
      PARAMETER( nwant_muon_tght = 6 )
      PARAMETER( nwant_pmuo = 50 )
      PARAMETER( nvar_muon = 86 )          
      PARAMETER( nwant_jet = 20 )
      PARAMETER( nvar_jet = 16 )
      PARAMETER( nwant_met = 7 )           
      PARAMETER( nvar_met = 8 )
      PARAMETER( nwant_muisa = 5 )
      PARAMETER( nvar_muisa = 5 )
      PARAMETER( nwant_bbisa = 5 )
      PARAMETER( nvar_bbisa = 4 )
      PARAMETER( nwant_l1muon = 3 )
      PARAMETER( nwant_l1jet = 3 )
      PARAMETER( nwant_l2muon = 3 )
      PARAMETER( nvar_l2muon = 5 )         
      PARAMETER( nwant_l2jet = 3 )
      PARAMETER( nvar_l2jet = 5 )          
      PARAMETER( nvar_l1met = 2 )
      PARAMETER( nvar_l2met = 2 )
      PARAMETER( max_particle = 54 )
      INTEGER elec_link(nwant_ele),jets_link(nwant_jet)
      INTEGER elec_link_tght(nwant_eletght)
      INTEGER phot_link_tght(nwant_gamtght)
      INTEGER ptr_ele_tght(nwant_eletght)
      INTEGER ptr_gam_tght(nwant_gamtght)
      INTEGER ptr_muon_tght(nwant_muon)
      INTEGER muon_link(nwant_muon)
      INTEGER muon_link_tght(nwant_muon_tght)
      INTEGER pmuo_link(nwant_pmuo)
      INTEGER phot_link(nwant_phot)
      REAL    xrun(50),xglobal(100)
      REAL    xelec(nwant_ele*nvar_elec+1),xphot(nwant_phot*nvar_phot+1)
      REAL    xjet(nwant_jet*nvar_jet+1),xmet(nwant_met*nvar_met+1)
      REAL    xmuon(nwant_muon*nvar_muon+1)
      REAL    xmuisa(nwant_muisa*nvar_muisa+1)
      REAL    xbbisa(nwant_bbisa*nvar_bbisa+1)
      REAL    xl1muon(nwant_l1muon*nvar_l2muon+1)
      REAL    xl1jet(nwant_l1jet*nvar_l2jet+1)
      REAL    xl2muon(nwant_l2muon*nvar_l2muon+1)
      REAL    xl2jet(nwant_l2jet*nvar_l2jet+1)
      REAL    xz_fit(100),xfilt(50)
      INTEGER ixrun(50),ixglobal(100),ixphot(nwant_phot*nvar_phot+1)
      INTEGER ixelec(nwant_ele*nvar_elec+1)
      INTEGER ixjet(nwant_jet*nvar_jet+1),ixmet(nwant_met*nvar_met+1)
      INTEGER ixmuon(nwant_muon*nvar_muon+1)
      INTEGER ixmuisa(nwant_muisa*nvar_muisa+1)
      INTEGER ixbbisa(nwant_bbisa*nvar_bbisa+1)
      INTEGER ixl1muon(nwant_l1muon*nvar_l2muon+1)
      INTEGER ixl1jet(nwant_l1jet*nvar_l2jet+1)
      INTEGER ixl2muon(nwant_l2muon*nvar_l2muon+1)
      INTEGER ixl2jet(nwant_l2jet*nvar_l2jet+1)
      INTEGER ixz_fit(100),ixfilt(50)
      EQUIVALENCE (ixrun(1),xrun(1))
      EQUIVALENCE (ixglobal(1),xglobal(1) )
      EQUIVALENCE (ixelec(1),xelec(1) )
      EQUIVALENCE (ixphot(1),xphot(1) )
      EQUIVALENCE (ixjet(1),xjet(1) )
      EQUIVALENCE (ixmet(1),xmet(1) )
      EQUIVALENCE (ixmuon(1),xmuon(1) )
      EQUIVALENCE (ixmuisa(1),xmuisa(1) )
      EQUIVALENCE (ixbbisa(1),xbbisa(1) )
      EQUIVALENCE (ixl1muon(1),xl1muon(1) )
      EQUIVALENCE (ixl1jet(1),xl1jet(1) )
      EQUIVALENCE (ixl2muon(1),xl2muon(1) )
      EQUIVALENCE (ixl2jet(1),xl2jet(1) )
      EQUIVALENCE (ixz_fit(1),xz_fit(1) )
      EQUIVALENCE (ixfilt(1),xfilt(1) )
      CHARACTER*32 tags(512),trun(50),telec(100),tjets(200),tmet(15)
      CHARACTER*32 tglb(100),tmuon(100),tphot(100),tmuisa(70)
      CHARACTER*32 tl2mu(100),tl2jt(100),xtags(100),tbbisa(70)
      CHARACTER*32 tz_fit(100),tfilt(50)
      REAL  evt_geninfo_tags,met_tags,elec_tags,jets_tags,phot_tags
      REAL  muon_tags, muisa_tags, l2muon_tags, l2jet_tags
      REAL  evt_glbinfo_tags, bbisa_tags, evt_z_fit_tags
      REAL  evt_filtinfo_tags
      REAL    metx,mety,meterx,metery,meterxy,mets,metc,metc_phi,zvert
      REAL    yem(20),htej,ht(4)
      INTEGER nparticle_jets
      LOGICAL mcdata
      COMMON /LOWE_ANALYSIS/ ntot_ele,  ntot_phot, ntot_muon, ntot_jet,
     &  ntot_eletght, ntot_gamtght, nelec, nphot, nmuon, njet,
     &  nelec_tght, ngam_tght, nmuon_tght, ntot_muon_tght,
     &  xrun,xglobal, xelec,xphot, xjet,xmet, xmuon, 
     &  xmuisa, xbbisa, xl1muon, xl1jet, xl2muon, xl2jet, xz_fit,xfilt,
     &  metx,mety,meterx,metery,meterxy,mets,metc,metc_phi,
     &  nparticle_jets,zvert,mcdata,yem,htej,ht
