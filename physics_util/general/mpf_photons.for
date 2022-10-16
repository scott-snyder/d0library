      SUBROUTINE mpf_photons(pass_phot,n_em,em_e,em_eta,em_phi,
     &            em_qual,delta_et)

C----------------------------------------------------------------------
C-   Purpose and Methods :  Obtain Et sorted information about
C-        reconstructed 'photons' in collider data or monte carlo
C-
C-   Outputs :
C-        pass_phot      I  = 0 if event fails photon requirements,
C-                          = 1 if event passes photon requirements,
C-                          = 2 if event passes photon and Drell-Yan cuts.
C-        n_em           I  total # EM objects
C-        em_e(50,#)     R  energy quantities for em clusters
C-               (1,#) = Ex
C-               (2,#) = Ey
C-               (3,#) = Ez
C-               (4,#) = E
C-               (5,#) = reco. Et
C-               (6,#) = pjets Et, unsmeared
C-               (10,#) = em E in cluster w/o central tower
C-               (11,#) = tot E in R <= 0.2
C-               (12,#) = em E in R <= 0.2
C-               (13,#) = tot E in R <= 0.4
C-               (14,#) = em E in R <= 0.4
C-               (15,#) = E in physics isol cone (R = 0.4)
C-               (16,#) = E in physics isol cone (R = 0.6)
C-               (17,#) = E in physics isol cone (R = 0.7)
C-               7 thru 9, 18 thru 50 spare
C-        em_eta(10,#)   R   physics and detector Eta
C-        em_phi(10,#)   R   phi
C-        em_qual(50,#)  R   quality of EM clusters
C-               (1,#) = CLEANEM status
C-               (2,#) = Hmatrix chi-squared
C-               (3,#) = EM fraction
C-               (4,#) = isolation fraction (Et, R = 0.4)
C-               (5,#) = track match significance
C-               (6,#) = CDC dE/dx for track
C-               (7,#) = VTX dE/dx for track
C-               (8,#) = FDC dE/dx for track
C-               (17,#) = PELC/PPHO switch (-1/0)
C-               (18,#) = TRD geometry (ie. # layers crossed)
C-               (19,#) = TRD efficiency (epsilon-t)
C-               20 thru 50 spare
C-        delta_et(2)    R   change in leading photon Et from
C-                           parton matching in a dijet MC event
C-
C-      note: in the case where dijet MC is being used, the following
C-        elements have different contents:
C-          em_e(5,#) = pjets Et smeared with 15%/sqrt(E)
C-          em_qual(2,#) = merge/split flag
C-          em_qual(5,#) = number of cells
C-          em_qual(6,#) = icd fraction
C-          em_qual(7,#) = ch fraction
C-          em_qual(8,#) = max-cell ratio
C-
C-   Controls : MPF_JET_RESPONSE_RCP
C-
C-   Created  Feb-28-1994   Bob Kehoe
C-   Updated  Mar-06-1994   Bob Kehoe  -- add simple smearing of 'photon' for
C-                                        dijet MC events, debug
C-   Updated  Apr-17-1994   Bob Kehoe  -- add EC 'photons'
C-   Updated  May-29-1994   Bob Kehoe  -- add EM scale parameters, filter cuts
C-   Updated  Jun-25-1995   Bob Kehoe  -- remove EM scale parameters, add TRD
C-   Updated  Oct-18-1995   Bob Kehoe  -- change mpf include for IBM's
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$INC:MPF_JETRES.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER bittot
      PARAMETER (bittot = 32)
      INTEGER i,ier,k,n_em,nparton,cleanem_mask,status
      INTEGER ljets,lcaph,gzcaph,lisv1,gzisv1,dpho_mode
      INTEGER filter_bits,nfilt,filtpass,selection(10),pass_phot
      INTEGER em_ident(20,limit),iarray(10),EM_BIN,NMET
      REAL filt_thresh_lo(bittot),filt_thresh_hi(bittot)
      REAL em_e(50,limit),em_eta(10,limit),em_phi(10,limit)
      REAL em_qual(50,limit),em_qual1(50,limit)
      REAL parton_et(limit),parton_phi(limit),parton_eta(limit)
      REAL etph_fract(10),iso4_tight(10),rnum,theta,zv1
      REAL delta_et(2),delta_phi,phot_det_eta,dis,delta_phot
      REAL fisol_tight(10),PM_CUT(10),iso_Et,emclus_et
      REAL phot_deta_cc_low,phot_deta_ec_low,phot_deta_ec_hi
      REAL par_max_delta_r,pho_emf(10),em_resolution,pjarray(10)
      LOGICAL first,use_cc,use_ec,in_cc,in_ec,mc_set
      LOGICAL monte_carlo_data,use_ppho,use_pelc,pass
      CHARACTER*8 str(10)
      CHARACTER*32 filter_names(128)
      DATA first /.true./

C----------------------------------------------------------------------
C-        INITIALIZE VARIABLES
      IF (first) THEN
        first = .false.
        mc_set = monte_carlo_data()
        CALL ezpick('MPF_JET_RESPONSE_RCP')
        CALL ezget_i('dpho_mode',dpho_mode,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','dpho_mode','F')
        IF (ier.EQ.0) CALL ez_get_chars('filter_names',nfilt,
     &        filter_names,ier)
        CALL ezget_i('cleanem_mask',cleanem_mask,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','cleanem_mask','F')
        CALL ezgeta_i('FILT_THRESH_LO',0,0,0,nfilt,ier)
        IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_photons','nfilt',
     &    'F')
        CALL ezgeta('FILT_THRESH_LO',1,nfilt,1,filt_thresh_lo,ier)
        IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_photons',
     &    'filt_thresh_lo','F')
        CALL ezgeta_i('FILT_THRESH_HI',0,0,0,nfilt,ier)
        IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_photons','nfilt',
     &    'F')
        CALL ezgeta('FILT_THRESH_HI',1,nfilt,1,filt_thresh_hi,ier)
        IF (ier.NE.0) CALL errmsg('ezgeta error','mpf_photons',
     &    'filt_thresh_hi','F')
        CALL ezget_l('use_ppho',use_ppho,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','use_ppho','F')
        CALL ezget_l('use_pelc',use_pelc,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','use_pelc','F')
        CALL ezget_l('use_cc',use_cc,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','use_cc','F')
        CALL ezget('phot_deta_cc_low',phot_deta_cc_low,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','phot_deta_cc_low','F')
        CALL ezget('phot_det_eta',phot_det_eta,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','phot_det_eta','F')
        CALL ezget_l('use_ec',use_ec,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','use_ec','F')
        CALL ezget('phot_deta_ec_low',phot_deta_ec_low,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','phot_deta_ec_low','F')
        CALL ezget('phot_deta_ec_hi',phot_deta_ec_hi,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','phot_deta_ec_hi','F')
        IF (ier.EQ.0) CALL ezgeta_i('PM_CUT',0,0,0,nmet,ier)
        IF (ier.EQ.0) CALL ezgeta('PM_CUT',1,nmet,1,pm_cut,ier)
        IF (ier.EQ.0) CALL ezgeta('PHO_EMF',1,nmet,1,pho_emf,ier)
        IF (ier.EQ.0) CALL ezgeta('FISOL_TIGHT',1,nmet,1,fisol_tight,
     &      ier)
        IF (ier.EQ.0) CALL ezgeta('ISO4_TIGHT',1,nmet,1,iso4_tight,ier)
        IF (ier.EQ.0) CALL ezgeta('ETPH_FRACT',1,nmet,1,etph_fract,ier)
        CALL ezget('em_resolution',em_resolution,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','em_resolution','F')
        CALL ezget('par_max_delta_r',par_max_delta_r,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_photons','par_max_delta_r','F')
        IF (ier.EQ.0) CALL ezget_i('nalgo',iarray(1),ier)
        IF (ier.EQ.0) CALL ezget_i('iter',iarray(2),ier)
        IF (ier.EQ.0) CALL ezget_i('irst',iarray(3),ier)
        IF (ier.EQ.0) CALL ezget_i('imuon',iarray(4),ier)
        IF (ier.EQ.0) CALL ezget('etcut',pjarray(1),ier)
        IF (ier.EQ.0) CALL ezget('spl_mrg',pjarray(3),ier)
        CALL ezrset
      ENDIF
      DO i = 1,limit
        DO k = 1,2
          em_eta(k,i) = 0.
          em_phi(k,i) = 0.
        ENDDO
        DO k = 1,20
          em_e(k,i) = 0.
        ENDDO
        DO k = 1,20
          em_qual(k,i) = 0.
        ENDDO
      ENDDO
      delta_et(1) = 0.
      delta_et(2) = 0.
      pass_phot = 0
      in_cc = .false.
      in_ec = .false.
      n_em = 0
      IF (mc_set) THEN
        filtpass = filter_bits(.false.,nfilt,filter_names)
      ELSE
        filtpass = filter_bits(.true.,nfilt,filter_names)
      ENDIF

C-        OBTAIN INFO CONCERNING RECONSTRUCTED 'PHOTONS'
      selection(1) = cleanem_mask
      IF (dpho_mode.EQ.1) THEN
        CALL cal_emcluster(selection,str,limit,n_em,em_e,em_eta,em_phi,
     &            em_qual1,em_ident)
        DO k = 1,n_em
          em_qual(1,k) = float(em_ident(1,k))
          em_qual(17,k) = float(em_ident(4,k))
          em_qual(18,k) = float(em_ident(3,k))
          DO i = 2,9
            em_qual(i,k) = em_qual1(i,k)
          ENDDO
          em_qual(19,k) = em_qual1(19,k)
        ENDDO
      ELSE
C-        GET 'PHOTON' INFORMATION FROM DIJET EVENTS BY LOOKING AT LEADING JETS
C-        SO THEY WILL REQUIRE THE LEAST CHANGE IN ENERGY TO GET PARTON ENERGY
        lisv1 = gzisv1()
        IF (lisv1.NE.0) zv1 = q(lisv1 + 9)
        CALL set_caph_alg(1)
        lcaph = gzcaph()
        CALL reset_caph
        IF (lcaph.NE.0) THEN
          n_em = iq(lcaph+3)
          ljets = lq(lcaph-izjets)
          IF (ljets.NE.0) THEN
            DO i = 1, n_em
              em_e(1,i) = q(ljets+2)
              em_e(2,i) = q(ljets+3)
              em_e(3,i) = q(ljets+4)
              em_e(4,i) = q(ljets+5)
              em_e(5,i) = q(ljets+6)
              em_eta(1,i) = q(ljets+9)
              theta = 2.*atan(exp(-em_eta(1,i)))
              CALL det_eta(zv1,theta,em_eta(2,1))
              em_phi(1,i) = q(ljets+8)
              em_qual(2,i) = q(ljets+15)
              em_qual(4,i) = q(ljets+14)
              em_qual(5,i) = q(ljets+16)
              em_qual(6,i) = q(ljets+17)
              em_qual(7,i) = q(ljets+18)
              em_qual(8,i) = q(ljets+19)
              ljets = lq(ljets)
            ENDDO
          ENDIF
        ENDIF
      ENDIF

C-        *** check leading photon with user specified CC/EC qualifications ***
      IF (use_cc) THEN
        IF ((abs(em_eta(2,1)).gt.phot_deta_cc_low).and.(abs(em_eta(2,
     &      1)).LT.phot_det_eta)) in_cc = .true.
      ENDIF
      IF (.NOT.in_cc.AND.use_ec) THEN
        IF ((abs(em_eta(2,1)).gt.phot_deta_ec_low).and.
     &        (abs(em_eta(2,1)).lt.phot_deta_ec_hi)) in_ec = .true.
      ENDIF
      IF ((.NOT.in_cc).AND.(.NOT.in_ec)) GOTO 999
C-        *** check if leading photon adheres to PELC/PPHO qualifications
      IF (.NOT.use_pelc) THEN
        IF (em_qual(17,1).ne.0.) goto 999
      ELSEIF (.NOT.use_ppho) THEN
        IF (em_qual(17,1).eq.0.) goto 999
      ENDIF
C-        *** require tight quality for leading em cluster ***
      DO k = 1,nmet
        IF (k.EQ.1) THEN
          IF (em_e(5,1).lt.pm_cut(k)) em_bin = k
        ELSE
          IF ((em_e(5,1).ge.pm_cut(k-1)).and.(em_e(5,1).lt.
     &           pm_cut(k))) em_bin = k
        ENDIF
      ENDDO
      iso_et = em_e(15,1) - em_e(5,1)
      IF ((em_qual(4,1).gt.fisol_tight(em_bin)).or.(iso_et.
     &        gt.iso4_tight(em_bin))) GOTO 999
      IF (em_qual(3,1).lt.pho_emf(em_bin)) goto 999
      status = int(em_qual(1,1))
      IF (btest(status,11)) goto 999
C-        *** kinematic cuts on the leading and second leading 'photons'
      pass = .false.
      emclus_et = em_e(5,1)
      if (mc_set) emclus_et = em_E(5,1)/1.07
      DO k = 1,nfilt
        IF (btest(filtpass,k-1).and.(emclus_et.ge.filt_thresh_lo(k)).
     &          and.(emclus_et.lt.filt_thresh_hi(k))) pass = .true.
      ENDDO
      IF (.NOT.pass) GOTO 999
      pass_phot = 1
      IF ((em_e(5,2)/em_e(5,1)).gt.etph_fract(em_bin)) pass_phot = 2

C-        *** get PJETS level and match with reconstructed quantities ***
      IF (mc_set) THEN
        pjarray(2) = 0.3
        CALL get_pjet_vectors(iarray,pjarray,'CONE',limit,
     &      nparton,parton_et,parton_eta,parton_phi)
        DO k = 1,n_em
          DO i = 1,nparton
            delta_phi = abs(em_phi(1,k) -  parton_phi(i))
            IF (delta_phi.GT.pi) delta_phi = 2.*pi - delta_phi
            dis = sqrt((em_eta(1,k)-parton_eta(i))**2. + delta_phi**2.)
            IF (dis.LT.par_max_delta_r) em_e(6,k) = parton_et(i)
          ENDDO
        ENDDO
        IF (em_e(6,1).le.0.001) then
          CALL errmsg('zero photon parton Et','mpf_photons',' ','W')
          IF (dpho_mode.EQ.2) pass_phot = 0
        ELSEIF (dpho_mode.EQ.2) THEN
C-          *** for dijet MC smear 'photon' and get delta Et to correct
C-              missing Et vector ***
          CALL rnorml(rnum)
          delta_phot = em_resolution*sqrt(em_e(6,1))*rnum
          em_e(5,1) = em_e(6,1) + delta_phot
          delta_et(1) = em_e(5,1)*cos(em_phi(1,1)) - em_e(1,1)
          delta_et(2) = em_e(5,1)*sin(em_phi(1,1)) - em_e(2,1)
          em_e(1,1) = em_e(5,1)*cos(em_phi(1,1))
          em_e(2,1) = em_e(5,1)*sin(em_phi(1,1))
        ENDIF
      ENDIF

  999 RETURN
      END
