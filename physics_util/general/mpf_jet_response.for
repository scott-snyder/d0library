      LOGICAL FUNCTION mpf_jet_response()

C----------------------------------------------------------------------
C-   Purpose and Methods :  USE THE MISSING ET PROJECTION FRACTION (MPF)
C-      METHOD TO DERIVE A FUNCTIONAL FORM FOR THE ENERGY RESPONSE OF JETS IN
C-      COLLIDER OR MC DATA.  THE METHOD EMPLOYS DIRECT PHOTON CANDIDATES
C-      AND INVOLVES THE USE OF MISSING ET TO MEASURE THE RESPONSE DIFFERENCE
C-      BETWEEN 'PHOTONIC' AND 'HADRONIC' SIDES OF THE EVENT.  THE QUANTITY
C-      MPF IS DEFINED AS:
C-
C-            MPF = MET(dot)N-HAT/ET,  N-HAT=DIRECTION OF PHOTON
C-                        ET = ET OF PHOTON
C-
C-      AN NTUPLE FOR FURTHER ANALYSIS IS FILLED IF REQUESTED.  RESPONSE PLOTS
C-      ARE MADE FOR FOUR FIXED-CONE AND ONE NEAREST NEIGHBOR JET RECONSTRUCTION
C-      ALGORITHM.  (See D0notes 2052, and 2053 for details.)
C-
C-   Controls : MPF_JET_RESPONSE_RCP
C-
C-   Created  Feb-11-1994   Bob Kehoe
C-   Updated  Mar-07-1994   Bob Kehoe -- add histo's for different cuts
C-                                       and jet reconstruction algorithms
C-   Updated  Mar-27-1994   Bob Kehoe -- use vertex_info for vertex, modify RCP
C-                                       handling
C-   Updated  Jun-04-1994   Bob Kehoe -- add isolation vars. to ntuple, add
C-                                       mpf_z_boson, dE/dx and trackmatch cuts
C-   Updated  Apr-20-1995   Chip Stewart, Bob Kehoe -- CWNize
C-   Updated  Oct-18-1995   Bob Kehoe -- change mpf include for IBM's
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:MPF_JETRES.INC'
      INCLUDE 'D0$INC:MPF_FILLJET.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER wanted
      PARAMETER (wanted = 3)
      INTEGER i,j,ier,k,njets,nz,nx,x,nbias,nprime
      INTEGER lpnut,gzpnut,lisv1,gzisv1,lrcp,lpmuo,gzpmuo
      INTEGER lpnut_icd,lcaid,gzcaid
      INTEGER pass_release,em_bin,istart,num_name,index,dpho_mode
      INTEGER nfilt,filter_bits,nmet,version,vaxtime(2),d_y
      REAL bias_corr_bins(nbin_et),eprime_corr_bins(nmax_e)
      REAL mex_offset,mey_offset,del_phix,bias_bin,eprime_bin
      REAL info(3,wanted),zv(wanted),dz(wanted),abs_zvert
      REAL fz,sz, lum(2),age(2),met_icd(2)
      REAL zboson_e(50,limit),zboson_eta(10,limit)
      REAL zboson_phi(10,limit),emx(50,limit)
      REAL par_e(50,limit),par_eta(10,limit)
      REAL par_phi(10,limit),par_qual(50,limit),em_quans(50,limit)
      REAL jet_eta_all(10,limit),jet_phi_all(10,limit)
      REAL jet_e_all(50,limit),jet_qual_all(50,limit)
      REAL min_delta_phi(10),match_dis,etj_ratio,dis,delta_et(2)
      REAL nrg(4,2),response,ej_prime
      REAL pho_dedx_lo(10),pho_dedx_hi(10),pho_trkm(10),dedx,treff
      REAL pho_trd_eff_lo(10),pho_trd_eff_hi(10)
      REAL e_jet,muon_pt,mu_hiest,max_muon_pt(10),max_lum_ec,max_lum_cc
      REAL jet_deta_low,jet_det_eta,jet_emf_hi
      REAL pm_cut(10),met_fract(10),et3_fract(10),photon_et_thresh(20)
      REAL cone_sizes(num_algo)
      LOGICAL first,pass_parton,pass_jets,write_dst,jet_ok,ok,yep
      LOGICAL ntp_set,monte_carlo_data,no_imass,write_if_em
      LOGICAL met_corr,mc_set,fg,sg,mrveto
      LOGICAL microblank_veto,veto_caid,passx
      CHARACTER*32 filter_names(128)
      CHARACTER*255 xfile
      CHARACTER*9 name_of_file(20)
      DATA cone_sizes/0.7, 0.5, 0.3, 0.3, 1.0, 0.7/
      DATA first /.true./

C----------------------------------------------------------------------
C-        INITIALIZE VARIABLES
      CALL hcdir(pawcdir,' ')
      CALL hcdir(filedir,' ')
      IF (first) THEN
C        CALL dhshow
        CALL ezloc('MPF_JET_RESPONSE_RCP',lrcp)
        ok = lrcp .GT. 0
        IF (.NOT. ok) THEN
          CALL inrcp('MPF_JET_RESPONSE_RCP',ier)
          IF (ier.EQ.0) CALL ezpick('MPF_JET_RESPONSE_RCP')
          IF (ier.EQ.0) CALL ezerr(ier)
          IF(ier.NE.0) THEN
            CALL errmsg('RCP not found','MPF_JET_RESPONSE',
     &        'MPF_JET_RESPONSE_RCP','F')
          ENDIF
          CALL ezrset
        ENDIF                 ! *** read in RCP parameters ***
        CALL ezpick('MPF_JET_RESPONSE_RCP')
        CALL ezerr(ier)
        IF (ier.EQ.0) THEN
          CALL ezget('dpho_mode',dpho_mode,ier)
          IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_jet_response','dpho_mode','F')
          IF (ier.EQ.0) CALL ez_get_chars('filter_names',nfilt,
     &        filter_names,ier)
          IF (ier.EQ.0) CALL ez_get_chars('name_of_file',num_name,
     &        name_of_file,ier)
          IF (ier.EQ.0) CALL ezgeta('PHOTON_ET_THRESH',1,num_name,1,
     &        photon_et_thresh,ier)
          IF (ier.EQ.0) CALL ezget('met_corr',met_corr,ier)
          IF (ier.EQ.0) CALL ezget('microblank_veto',microblank_veto,
     &        ier)
          IF (ier.EQ.0) CALL ezget('veto_caid',veto_caid,ier)
          IF (ier.EQ.0) CALL ezget('mex_offset',mex_offset,ier)
          IF (ier.EQ.0) CALL ezget('mey_offset',mey_offset,ier)
          IF (ier.EQ.0) CALL ezget('abs_zvert',abs_zvert,ier)
          IF (ier.EQ.0) CALL ezget('jet_emf_hi',jet_emf_hi,ier)
          IF (ier.EQ.0) CALL ezget('jet_deta_low',jet_deta_low,ier)
          IF (ier.EQ.0) CALL ezget('jet_det_eta',jet_det_eta,ier)
          IF (ier.EQ.0) CALL ezgeta('PM_CUT',0,0,0,nmet,ier)
          IF (ier.EQ.0) CALL ezgeta('PM_CUT',1,nmet,1,pm_cut,ier)
          IF (ier.EQ.0) CALL ezgeta('PHO_DEDX_LO',1,nmet,1,pho_dedx_lo,
     &      ier)
          IF (ier.EQ.0) CALL ezgeta('PHO_DEDX_HI',1,nmet,1,pho_dedx_hi,
     &      ier)
          IF (ier.EQ.0) CALL ezgeta('PHO_TRD_EFF_LO',1,nmet,1,
     &          pho_trd_eff_lo,ier)
          IF (ier.EQ.0) CALL ezgeta('PHO_TRD_EFF_HI',1,nmet,1,
     &          pho_trd_eff_hi,ier)
          IF (ier.EQ.0) CALL ezgeta('PHO_TRKM',1,nmet,1,pho_trkm,ier)
          IF (ier.EQ.0) CALL ezgeta('MET_FRACT',1,nmet,1,met_fract,ier)
          IF (ier.EQ.0) CALL ezgeta('MAX_MUON_PT',1,nmet,1,max_muon_pt,
     &      ier)
          IF (ier.EQ.0) CALL ezgeta('MIN_DELTA_PHI',1,nmet,1,
     &      min_delta_phi,ier)
          IF (ier.EQ.0) CALL ezgeta('ET3_FRACT',1,nmet,1,et3_fract,ier)
          IF (ier.EQ.0) CALL ezget('max_lum_cc',max_lum_cc,ier)
          IF (ier.EQ.0) CALL ezget('max_lum_ec',max_lum_ec,ier)
          IF (ier.EQ.0) CALL ezget('match_dis',match_dis,ier)
          IF (ier.EQ.0) CALL ezget('write_dst',write_dst,ier)
          IF (ier.EQ.0) CALL ezget('ntp_set',ntp_set,ier)
          IF (ier.EQ.0) CALL ezget('write_if_em',write_if_em,ier)
          CALL ezgeta('BIAS_CORR_BINS',0,0,0,nbias,ier)
          IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_jet_response',
     &      'nbias','F')
          CALL ezgeta('BIAS_CORR_BINS',1,nbias,1,bias_corr_bins,ier)
          IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_jet_response',
     &      'bias_corr_bins','F')
          CALL ezgeta('EPRIME_CORR_BINS',0,0,0,nprime,ier)
          IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_jet_response',
     &      'nprime','F')
          CALL ezgeta('EPRIME_CORR_BINS',1,nprime,1,eprime_corr_bins,
     &      ier)
          IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_jet_response',
     &      'eprime_corr_bins','F')
          IF (ier.NE.0) CALL errmsg('rcp error','mpf_jet_response',' ',
     &      'F')
          CALL ezrset
        ELSE
          CALL errmsg('NO MPF_JET_RESPONSE_RCP','MPF_JET_RESPONSE',
     &        'NO RCP file to work with','F')
        ENDIF
        first = .false.
      ENDIF
      CALL vzero(zv,wanted)
      CALL vzero(dz,wanted)
      CALL hfill(300,1.,0.,1.)
      CALL flgset('WRITE_STREAM_DST',.false.)
      nevt = nevt + 1
      mc_set = monte_carlo_data()
      runnum = float(iq(lhead+12))
      eventnum = float(iq(lhead+9))
      met = 0.
      mex = 0.
      mey = 0.
      z_vert = 0.
      isa_zv = 0.
      inv_mass = 0.
      mpf = 0.
      mu_pt1 = 0.
      scalr_et = 0.
      met_phi = 0.
      reco_vrs = 0.
      lumin = 0.
      mc_etthresh = 0.
      mpf_z = 0.
      soft_et = 0.
      soft_ex = 0.
      soft_ey = 0.
      soft_phi = 0.
      mex_icd = 0.
      mey_icd = 0.
      ht_em = 0.
      z_e = 0.
      z_et = 0.
      z_eta = 0.
      z_phi = 0.
      drell_yan = .false.
      n_emc = 0
      DO k = 1,2
        emc_et(k)   = 0.
        emc_epr(k)  = 0.
        emc_e(k)    = 0.
        emc_ex(k)   = 0.
        emc_ey(k)   = 0.
        emc_eta(k)  = 0.
        emc_deta(k) = 0.
        emc_phi(k)  = 0.
        emc_emf(k)  = 0.
        emc_fiso(k) = 0.
        emc_ecor(k) = 0.
        emc_emcor(k) = 0.
        emc_eiso(k) = 0.
        emc_emiso(k) = 0.
        emc_eti7(k) = 0.
        emc_stat(k) = 0
        emc_chsq(k) = 0.
        emc_dedx(k) = 0.
        emc_trkm(k) = 0.
        emc_ncen(k) = 0.
        emc_clus(k) = .false.
        emc_treff(k) = 0.
      ENDDO
      algo = num_algo
      DO k = 1,num_algo
        eprime(k) = 0.
        del_phi(k) = 0.
        mpf_jet(k) = 0.
        ht(k) = 0.
      ENDDO
      nj7 = 0
      nj5 = 0
      nj3 = 0
      njn = 0
      nj1 = 0
      njt = 0

C-        *** obtain information concerning 'partons' which are observed in data
C-        *** or monte carlo.  these partons can be jets or photons, or Z
C-        *** bosons reconstructed from a dielectron.
      IF ((dpho_mode.EQ.1).OR.(dpho_mode.EQ.2)) THEN
        pass_parton = .false.
        CALL mpf_photons(d_y,n_emc,par_e,par_eta,par_phi,
     &          par_qual,delta_et)
        IF (d_y.GT.0) pass_parton = .true.
        IF (d_y.EQ.2) drell_yan = .true.
        CALL mpf_z_boson(passx,zboson_e,zboson_eta,zboson_phi,nx,emx)
      ELSEIF (dpho_mode.EQ.3) THEN
        CALL mpf_z_boson(pass_parton,par_e,par_eta,par_phi,n_emc,
     &          em_quans)
        drell_yan = .true.
      ELSE
        CALL errmsg('run mode error','mpf_jet_response',
     &      'dpho_mode value invalid','F')
      ENDIF
      IF (.NOT.pass_parton) GOTO 999
      CALL hfill(300,2.,0.,1.)

C-        ** obtain mc vertex and reconstructed vertex.  if v11 reco showerlib,
C-        ** must use mc vertex in place of reco. vertex.
      isa_zv = 0.
      IF (mc_set) THEN
        lisv1 = gzisv1()
        IF (lisv1.NE.0) isa_zv = q(lisv1+9)
      ENDIF
      CALL reco_version(version,pass_release)
      reco_vrs = float(version) + 0.01*float(pass_release)
      IF (reco_vrs.LT.1.0) CALL errmsg('reco wrong','mpf_jet_response',
     &        'strange value gotten for reco version','W')
      CALL vertex_info(wanted,nz,info,yep)
      IF (.NOT.yep) THEN
        CALL errmsg('vertex problem','mpf_jet_response',
     &      'failure in vertex_info','W')
        GOTO 999
      ENDIF
      DO k = 1,min(wanted,nz)
        zv(k) = info(1,k)
        dz(k) = info(2,k)
      ENDDO
      IF ((abs(zv(1)).GT.abs_zvert).OR.(abs(zv(1)).LE.0.0001)) GOTO 999
      CALL hfill(300,3.,0.,1.)

C-        *** obtain leading muon pt
      mu_hiest = 0.
      lpmuo = gzpmuo(0)
      DO WHILE (lpmuo.NE.0)
        muon_pt = q(lpmuo + 14)
        IF (muon_pt.GT.mu_hiest) mu_hiest = muon_pt
        lpmuo = lq(lpmuo)
      ENDDO

C-        GET MISSING ET VECTOR (MEX,MEY) AND CORRECT FOR OFFSETS
      lpnut_icd = gzpnut(1)
      lpnut = gzpnut(2)
      met_icd(1) = q(lpnut+3) - q(lpnut_icd+3)
      met_icd(2) = q(lpnut+4) - q(lpnut_icd+4)
      IF (.NOT.met_corr) THEN
        lpnut = gzpnut(2)
      ELSE
        lpnut = gzpnut(4)
      ENDIF
      IF (mc_set) THEN
        mex = q(lpnut+3) - delta_et(1)
        mey = q(lpnut+4) - delta_et(2)
      ELSE
        mex = q(lpnut+3) + mex_offset
        mey = q(lpnut+4) + mey_offset
      ENDIF
      met = sqrt(mex**2. +mey**2.)
      IF (dpho_mode.EQ.1) THEN
        ok = .false.
        DO k = 1,nmet
          IF (k.EQ.1) THEN
            IF (par_e(5,1).lt.pm_cut(k)) then
              em_bin = k
              IF ((met/par_e(5,1)).lt.met_fract(k)) ok = .true.
            ENDIF
          ELSE
            IF ((par_e(5,1).ge.pm_cut(k-1)).and.(par_e(5,1).lt.
     &           pm_cut(k))) THEN
              em_bin = k
              IF ((met/par_e(5,1)).lt.met_fract(k)) ok = .true.
            ENDIF
          ENDIF
        ENDDO
        IF (mu_hiest.GT.max_muon_pt(em_bin)) GOTO 999
        dedx = par_qual(6,1)
        IF (dedx.EQ.0.) dedx = par_qual(8,1)
        treff = par_qual(19,1)
        IF (.NOT.ok) THEN
C-            *** if large missing Et throw away if leading em cluster
C-            *** might be single charged EM particle (ie. electron)
          IF ((dedx.GE.pho_dedx_lo(em_bin)).AND.(dedx.LE.
     &          pho_dedx_hi(em_bin))) GOTO 999
          IF (treff.GT.-0.5) then
            IF ((treff.GE.pho_trd_eff_lo(em_bin)).AND.(treff.LE.
     &        pho_trd_eff_hi(em_bin))) GOTO 999
          ENDIF
          IF (par_qual(5,1).le.pho_trkm(em_bin)) goto 999
        ELSE
C-            *** if small missing Et throw away only those events whose
C-            *** leading EM cluster is likely an electron.
          IF ((dedx.GE.pho_dedx_lo(em_bin)).AND.
     &          (dedx.LE.pho_dedx_hi(em_bin))) THEN
            IF (treff.GT.-0.5) then
              IF ((treff.GE.pho_trd_eff_lo(em_bin)).AND.
     &            (treff.LE.pho_trd_eff_hi(em_bin))) THEN
                IF (par_qual(5,1).le.pho_trkm(em_bin)) goto 999
              ENDIF
            ELSEIF (par_qual(5,1).le.pho_trkm(em_bin)) then
              GOTO 999
            ENDIF
          ENDIF
        ENDIF
        IF (mc_set) THEN
CCCC          CALL input_file_name(xfile)
          DO k = 1,num_name
            istart = 0
            istart = index(xfile,name_of_file(k))
            IF (istart.NE.0) THEN
              mc_etthresh = photon_et_thresh(k)
              IF (par_e(5,1).lt.photon_et_thresh(k)) goto 999
            ENDIF
          ENDDO
        ENDIF
      ENDIF
      CALL hfill(300,5.,0.,1.)

C-        *** throw away events with mainring status, hot cells, or multiple
C-        *** interactions.
      CALL gtplv0_zonly(fz,fg,sz,sg,mi_flag)
      lcaid = gzcaid()
      ncaid = iq(lcaid + 4)
      IF (veto_caid.AND.(ncaid.NE.0)) GOTO 999
      ublank = 0
      IF (mrveto('MRBS_LOSS').AND.runnum.GT.72000) THEN
        ublank = 2
        IF (mrveto('MICRO_BLANK')) THEN
          ublank = 3
        ENDIF
      ELSEIF (mrveto('MICRO_BLANK')) THEN
        ublank = 1
      ENDIF
      IF (ublank.NE.0.AND.microblank_veto) GOTO 999
      IF (.NOT.mc_set) THEN
        vaxtime(1) = iq(lhead+4)
        vaxtime(2) = iq(lhead+5)
        CALL getlum(vaxtime,lum,age,ier)
        IF (ier.NE.0) THEN
          lum(1) = 0.
          lum(2) = 0.
        ENDIF
      ENDIF

C-        CALCULATE MISSING ET PROJECTION FRACTION AND FILL NON-JET
C-        QUANTITIES IN NTUPLE.  'Response' HERE AN ESTIMATE OF THE RAW
C-        CALORIMETER RESPONSE TO PARTONS OF A GIVEN ET
      mpf = (mex*par_e(1,1)+mey*par_e(2,1))/par_e(5,1)**2.
      response = 1. + mpf
      CALL hfill(500,par_e(5,1),response,1.)
      DO k = 1,nbias
        IF (k.LT.nbias) THEN
          IF ((par_e(5,1).gt.bias_corr_bins(k)).and.(par_e(5,1).le.
     &            bias_corr_bins(k+1))) then
            bias_bin = epsilon +
     &              ((bias_corr_bins(k)+bias_corr_bins(k+1))/2.0)
          ENDIF
        ENDIF
      ENDDO
      IF (lumin.LT.max_lum_ec.AND.mi_flag.LE.2)
     &    CALL hfill(501,bias_bin,response,1.)
      IF (ntp_set) THEN
        z_vert = zv(1)
        IF (mc_set) THEN
          filter = filter_bits(.false.,nfilt,filter_names)
        ELSE
          filter = filter_bits(.true.,nfilt,filter_names)
        ENDIF
        IF ((dpho_mode.EQ.1).OR.(dpho_mode.EQ.2)) THEN
          DO k = 1,4
            nrg(k,1) = par_e(k,1)
            nrg(k,2) = par_e(k,2)
          ENDDO
        ELSEIF (dpho_mode.EQ.3) THEN
          DO k = 1,4
            nrg(k,1) = em_quans(10+k,1)
            nrg(k,2) = em_quans(10+k,2)
          ENDDO
        ENDIF
        IF ((nrg(4,1).eq.0.).or.(nrg(4,2).eq.0.)) no_imass = .true.
        IF (.NOT.no_imass) CALL invariant_mass(2,nrg,0,inv_mass)
        mu_pt1 = mu_hiest
        scalr_et = q(lpnut + 14)
        met_phi = atan2(mey,(mex+epsilon))
        IF (met_phi.LT.0.) met_phi = met_phi + 2.*pi
        lumin = (lum(1)+lum(2))/2.
        mex_icd = met_icd(1)
        mey_icd = met_icd(2)
        IF (n_emc.GT.2) n_emc = 2
        nele2 = n_emc
        nele3 = n_emc
        IF ((dpho_mode.EQ.1).OR.(dpho_mode.EQ.2)) THEN
          IF (zboson_e(5,1).gt.0) mpf_z =
     &        (mex*zboson_e(1,1)+mey*zboson_e(2,1))/zboson_e(5,1)**2.
          z_et   = zboson_e(5,1)
          z_e    = zboson_e(4,1)
          z_eta  = zboson_eta(1,1)
          z_phi  = zboson_phi(1,1)
          DO k = 1,n_emc
            emc_et(k)   = par_e(5,k)
            emc_epr(k)  = par_e(6,k)
            emc_e(k)    = par_e(4,k)
            emc_ex(k)   = par_e(1,k)
            emc_ey(k)   = par_e(2,k)
            emc_eta(k)  = par_eta(1,k)
            emc_deta(k) = par_eta(2,k)
            emc_phi(k)  = par_phi(1,k)
            emc_stat(k) = par_qual(1,k)
            emc_chsq(k) = par_qual(2,k)
            emc_emf(k)  = par_qual(3,k)
            emc_fiso(k) = par_qual(4,k)
            emc_dedx(k) = par_qual(6,k)
            IF (par_qual(6,k).eq.0.) emc_dedx(k) = par_qual(8,k)
            emc_trkm(k) = par_qual(5,k)
            emc_ncen(k) = par_e(10,k)
            emc_ecor(k) = par_e(11,k)
            emc_emcor(k) = par_e(12,k)
            emc_eiso(k) = par_e(13,k)
            emc_emiso(k) = par_e(14,k)
            emc_eti7(k) = par_e(17,k)
            emc_clus(k) = .false.
            if (par_qual(17,k).ne.0.0) emc_clus(k) = .true.
            IF (par_qual(18,k).eq.3.0) then
              emc_treff(k) = par_qual(19,k)
            ELSE
              emc_treff(k) = -1.0
            ENDIF
          ENDDO
        ELSE
          z_et   = par_e(5,1)
          z_e    = par_e(4,1)
          z_eta  = par_eta(1,1)
          z_phi  = par_phi(1,1)
          DO k = 1,n_emc
            emc_et(k)   = em_quans(8,k)
            emc_eta(k)  = em_quans(9,k)
            emc_phi(k)  = em_quans(10,k)
            emc_ex(k)   = em_quans(11,k)
            emc_ey(k)   = em_quans(12,k)
            emc_stat(k) = em_quans(1,k)
            emc_chsq(k) = em_quans(2,k)
            emc_emf(k)  = em_quans(3,k)
            emc_fiso(k) = em_quans(4,k)
            emc_dedx(k) = em_quans(6,k)
            emc_trkm(k) = em_quans(5,k)
            emc_clus(k) = .false.
            if (em_quans(17,k).ne.0.0) emc_clus(k) = .true.
            IF (em_quans(18,k).eq.3.0) then
              emc_treff(k) = em_quans(19,k)
            ELSE
              emc_treff(k) = -1.0
            ENDIF
          ENDDO
        ENDIF
        ht_em=0
        DO i = 1,n_emc
          ht_em = ht_em + par_e(5,1)
        ENDDO
      ENDIF

C-        OBTAIN JET INFORMATION FOR DIFFERENT RECONSTRUCTION ALGORITHMS,
C-        CALCULATE EJ_PRIME FOR EACH, AND FILL HISTOGRAMS
      ok = .false.
      DO algo = 1,num_algo
        CALL mpf_jets(algo,pass_jets,njets,jet_e_all,jet_eta_all,
     &          jet_phi_all,jet_qual_all)
        IF (pass_jets) THEN
          nj_not_em = 0
          DO k = 1,limit
            DO i = 1,30
              jet_e(i,k) = 0.
            ENDDO
            DO i = 1,2
              jet_eta(i,k) = 0.
              jet_phi(i,k) = 0.
            ENDDO
            DO i = 1,30
              jet_qual(i,k) = 0.
            ENDDO
          ENDDO
          DO k = 1,njets
            jet_ok = .true.
C-            *** exclude EM clusters from jet list if they are leading 'photon'
C-            *** when in normal mode, or when they are from Z if in Z mode.
            IF ((dpho_mode.EQ.1).OR.(dpho_mode.EQ.2)) THEN
              del_phix = abs(par_phi(1,1)-jet_phi_all(1,k))
              IF (del_phix.GT.pi) del_phix = 2*pi - del_phix
              dis = sqrt((par_eta(1,1) - jet_eta_all(1,k))**2. +
     &            del_phix**2.)
              IF (dis.LT.cone_sizes(algo)) jet_ok = .false.
            ELSE  ! *** if Z mode, then exclude electrons from jet list ***
              DO j = 1,2
                del_phix = abs(em_quans(10,j)-jet_phi_all(1,k))
                IF (del_phix.GT.pi) del_phix = 2*pi - del_phix
                dis = sqrt((em_quans(9,j) - jet_eta_all(1,k))**2. +
     &              del_phix**2.)
                IF (dis.LT.match_dis) jet_ok = .false.
              ENDDO
            ENDIF
            IF (jet_ok) THEN
              IF (jet_qual_all(1,k).ge.jet_emf_hi) goto 300
              nj_not_em = nj_not_em + 1
              DO i = 1,50
                jet_e(i,nj_not_em) = jet_e_all(i,k)
              ENDDO
              DO i = 1,2
                jet_eta(i,nj_not_em) = jet_eta_all(i,k)
              ENDDO
              jet_phi(1,nj_not_em) = jet_phi_all(1,k)
              DO i = 1,50
                jet_qual(i,nj_not_em) = jet_qual_all(i,k)
              ENDDO
            ENDIF
          ENDDO
C-            PLOT RESPONSE OF EVENTS SATISFYING QUALITY CUTS
          IF (nj_not_em.GE.1) THEN
            ok = .true.
            ej_prime = par_e(5,1)*cosh(jet_eta(1,1))
            e_jet = jet_e(4,1)
            etj_ratio = jet_e(5,2)/jet_e(5,1)
            del_phix = abs(par_phi(1,1) - jet_phi(1,1))
            IF (del_phix.GT.pi) del_phix = 2.*pi - del_phix
            IF (algo.EQ.2) CALL hfill(215,del_phix,0.,1.)
C-            PLOT RESPONSE OF EVENTS SATISFYING TOPOLOGICAL CUTS
            IF ((abs(jet_eta(2,1)).ge.jet_deta_low).and.
     &            (abs(jet_eta(2,1)).le.jet_det_eta)) then
              IF ((del_phix.GT.min_delta_phi(em_bin)).AND.(etj_ratio.
     &              le.et3_fract(em_bin))) THEN
                IF (abs(jet_eta(2,1)).lt.1.0) then
                  CALL hfill(510+algo,bias_bin,response,1.)
                  CALL hfill(560+algo,bias_bin,jet_e(5,1),1.)
                ENDIF
                DO x = 1,nprime
                  IF (x.LT.nprime) THEN
                    IF ((ej_prime.GT.eprime_corr_bins(x)).AND.
     &                      (ej_prime.LE.eprime_corr_bins(x+1))) then
                      eprime_bin = epsilon + ((eprime_corr_bins(x)
     &                      + eprime_corr_bins(x+1))/2.0)
                    ENDIF
                  ENDIF
                ENDDO
                IF (abs(jet_eta(2,1)).lt.0.5.and.lumin.lt.max_lum_cc)
     &                  THEN
                  CALL hfill(520+algo,eprime_bin,response,1.)
                  CALL hfill(570+algo,eprime_bin,e_jet,1.)
                  CALL hfill(470+algo,eprime_bin,par_e(5,1),1.)
                ELSEIF (abs(jet_eta(2,1)).gt.2.0.and.abs(jet_eta(2,1))
     &                  .LT.jet_det_eta) THEN
                  IF (lumin.LT.max_lum_ec.AND.mi_flag.LE.2) THEN
                    IF (par_e(5,1).gt.30.) then
                      CALL hfill(530+algo,eprime_bin,response,1.)
                      CALL hfill(580+algo,eprime_bin,e_jet,1.)
                      CALL hfill(480+algo,eprime_bin,par_e(5,1),1.)
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            IF (ntp_set) THEN
              IF (nj_not_em.GT.5) nj_not_em = 5
              eprime(algo) = ej_prime
              del_phi(algo) = del_phix
              mpf_jet(algo) = (mex*jet_e(1,1)+mey*jet_e(2,1))/
     &               par_e(5,1)**2.
              ht(algo) = 0
              DO k = 1,nj_not_em
                ht(algo) = ht(algo) + jet_e(5,k)
              ENDDO
              CALL mpf_fill_jets(algo)
            ENDIF
          ENDIF
        ENDIF
  300   CONTINUE
      ENDDO
C
      IF (ok.OR.write_if_em) THEN
        CALL hfill(210,mex,0.,1.)
        CALL hfill(211,mey,0.,1.)
        CALL hfill(300,6.,0.,1.)
        npass = npass + 1
        soft_ex = mex
        soft_ey = mey
        DO k = 1,n_emc
          soft_ex = soft_ex + emc_ex(k)
          soft_ey = soft_ey + emc_ey(k)
        ENDDO
        DO k = 1,nj5
          soft_ex = soft_ex + jet_e(k,1)
          soft_ey = soft_ey + jet_e(k,2)
        ENDDO
        soft_et = sqrt(soft_ex**2. + soft_ey**2.)
        soft_phi = atan2(soft_ey,(soft_ex+epsilon))
        algo = num_algo
        IF (ntp_set) CALL hfnt(nt_id)
        CALL flgset('WRITE_STREAM_DST',write_dst)
      ENDIF

      mpf_jet_response = .true.

  999 CALL hcdir('//PAWC',' ')
      RETURN
      END
