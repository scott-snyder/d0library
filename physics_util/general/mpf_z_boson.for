      SUBROUTINE mpf_z_boson(pass_z,zboson_e,zboson_eta,zboson_phi,n_em,
     &    em_qual)

C----------------------------------------------------------------------
C-   Purpose and Methods :  Obtain information about Z's in collider data or MC
C-
C-   Outputs :
C-        pass_z          L  .TRUE. if event passes photon requirements
C-        zboson_e(50,#)       R  energy quantities for em clusters
C-               (1,#) = Ex
C-               (2,#) = Ey
C-               (3,#) = Ez
C-               (4,#) = E
C-               (5,#) = reco. Et
C-               (6,#) = Z parton Et, unsmeared
C-               7 thru 20 spare
C-        zboson_eta(10,#)     R   (1) = physics Eta
C-        zboson_phi(10,#)     R   phi
C-        n_em            I   number of em clusters
C-        em_qual(50,#)   R   quality of EM clusters
C-               (1,#) = CLEANEM status
C-               (2,#) = Hmatrix chi-squared
C-               (3,#) = EM fraction
C-               (4,#) = isolation fraction (Et, R = 0.4)
C-               (5,#) = track match significance
C-               (6,#) = CDC/FDC dE/dx for track
C-               (7,#) = VTX dE/dx for track
C-               (8,#) = Et of em clusters
C-               (9,#) = eta of em clusters
C-               (10,#) = phi of em clusters
C-               (11,#) = Ex of em clusters
C-               (12,#) = Ey of em clusters
C-               (13,#) = Ez of em clusters
C-               (17,#) = PELC/PPHO switch (-1/0)
C-               (18,#) = TRD geometry (ie. number layers crossed)
C-               (19,#) = TRD efficiency (epsilon-t)
C-                  14 thru 16, 20 thru 50 spare
C-
C-   Controls : MPF_JET_RESPONSE_RCP
C-
C-   Created  May-25-1994   Bob Kehoe
C-   Updated  Sep-24-1994   Bob Kehoe -- add Ei for em clusters
C-   Updated  Jun-25-1995   Bob Kehoe -- add TRD, PELC/PPHO switch, fix output
C-                                       arrays
C-   Updated  Oct-18-1995   Bob Kehoe -- change mpf include for IBM's
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:MPF_JETRES.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER i,ier,k,n_em,nparton,nem_x,status
      INTEGER lisaj,gzisaj,selection(10),em_ident(20,20)
      REAL em_e_all(50,limit),em_eta_all(10,limit),em_phi_all(10,limit)
      REAL em_qual_all(50,limit)
      REAL em_e(50,limit),em_eta(10,limit),em_phi(10,limit)
      REAL em_qual(50,limit),nrg(4,2),em_qual1(50,limit)
      REAL zboson_e(50,limit),zboson_eta(10,limit),zboson_phi(10,limit)
      REAL z_theta
      REAL parton_et(limit),parton_phi(limit),parton_eta(limit)
      REAL fisol_tight,phot_deta_cc_low,phot_det_eta
      REAL zmass_hi,zmass_lo,par_max_delta_r,pho_emf,z_ele_et_cut
      REAL mass_ee,delta_phi,dis
      LOGICAL first,pass_z,monte_carlo_data,use_ppho,use_pelc,no_imass
      CHARACTER*8 str(10)
      DATA first /.true./

C----------------------------------------------------------------------
C-        INITIALIZE VARIABLES
      IF (first) THEN
        first = .false.
        CALL ezpick('MPF_JET_RESPONSE_RCP')
        CALL ezget('use_ppho',use_ppho,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','use_ppho','F')
        CALL ezget('use_pelc',use_pelc,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','use_pelc','F')
        CALL ezget('pho_emf',pho_emf,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','pho_emf','F')
        CALL ezget('fisol_tight',fisol_tight,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','fisol_tight','F')
        CALL ezget('z_ele_et_cut',z_ele_et_cut,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','z_ele_et_cut','F')
        CALL ezget('phot_deta_cc_low',phot_deta_cc_low,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','phot_deta_cc_low','F')
        CALL ezget('phot_det_eta',phot_det_eta,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','phot_det_eta','F')
        CALL ezget('zmass_lo',zmass_lo,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','zmass_lo','F')
        CALL ezget('zmass_hi',zmass_hi,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','zmass_hi','F')
        CALL ezget('par_max_delta_r',par_max_delta_r,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_z_boson','par_max_delta_r','F')
        CALL ezrset
      ENDIF
      DO i = 1,limit
        DO k = 1,10
          em_phi_all(k,i) = 0.
          em_phi(k,i) = 0.
          zboson_phi(k,i) = 0.
          em_eta_all(k,i) = 0.
          em_eta(k,i) = 0.
          zboson_eta(k,i) = 0.
        ENDDO
        DO k = 1,50
          em_e_all(k,i) = 0.
          em_e(k,i) = 0.
          zboson_e(k,i) = 0.
          em_qual_all(k,i) = 0.
          em_qual1(k,i) = 0.
          em_qual(k,i) = 0.
        ENDDO
      ENDDO
      mass_ee = 0.
      pass_z = .false.

C-        OBTAIN INFO CONCERNING RECONSTRUCTED EM clusters
      selection(1) = 0
      CALL cal_emcluster(selection,str,limit,nem_x,em_e_all,em_eta_all,
     &            em_phi_all,em_qual_all,em_ident)
      n_em = 0
      DO k = 1,nem_x
C-        *** require quality for EM clusters ***
        IF (em_qual_all(4,k).lt.fisol_tight) then
          IF (em_qual_all(3,k).gt.pho_emf) then
            status = int(em_qual_all(1,k))
            IF (.NOT.btest(status,11)) then
              IF (em_e_all(5,k).ge.z_ele_et_cut) then
                n_em = n_em + 1
                DO i = 1,50
                  em_e(i,n_em) = em_e_all(i,k)
                ENDDO
                em_phi(1,n_em) = em_phi_all(1,k)
                DO i = 1,2
                  em_eta(i,n_em) = em_eta_all(i,k)
                ENDDO
                em_qual1(1,n_em) = float(em_ident(1,k))
                DO i = 2,7
                  em_qual1(i,n_em) = em_qual_all(i,k)
                ENDDO
                IF (em_qual1(6,n_em).eq.0.)   
     &             em_qual1(6,n_em) = em_qual_all(8,k)
                em_qual1(17,n_em) = float(em_ident(4,k))
                em_qual1(18,n_em) = float(em_ident(3,k))
                em_qual1(19,n_em) = em_qual_all(19,k)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      IF (n_em.LT.2) GOTO 999
C-        *** check if leading EM cluster adheres to PELC/PPHO qualifications
      IF (use_pelc) THEN
        IF ((em_qual1(17,1).eq.0.).or.(em_qual1(17,2).eq.0.)) goto 999
      ELSEIF (use_ppho) THEN
        IF ((em_qual1(17,1).eq.0.).and.(em_qual1(17,2).eq.0.)) goto 999
      ENDIF
C-        *** invariant mass cut to specify Z window ***
      DO k = 1,4
        nrg(k,1) = em_e(k,1)
        nrg(k,2) = em_e(k,2)
        IF ((nrg(k,1).eq.0.).or.(nrg(k,2).eq.0.)) no_imass = .true.
      ENDDO
      IF (.NOT.no_imass) CALL invariant_mass(2,nrg,0,mass_ee)
      IF ((mass_ee.GE.zmass_lo).AND.(mass_ee.LE.zmass_hi)) GOTO 999

C-        *** reconstruct Z boson energy and direction ***
      DO k = 1,3
        zboson_e(k,1) = em_e(k,1) + em_e(k,2)
      ENDDO
      zboson_e(4,1) = sqrt(zboson_e(1,1)**2. + zboson_e(2,1)**2. +
     &        zboson_e(3,1)**2.)
      zboson_e(5,1) = sqrt(zboson_e(1,1)**2. + zboson_e(2,1)**2.)
      IF (zboson_e(5,1).lt.0.1) goto 999
      z_theta = atan2(zboson_e(5,1),zboson_e(3,1)+0.00001)
      IF (tan(z_theta/2.).le.0.000001) then
        zboson_eta(1,1) = 0.01
      ELSE
        zboson_eta(1,1) = -log(tan(z_theta/2.))
      ENDIF
      zboson_phi(1,1) = atan2(zboson_e(2,1),zboson_e(1,1)+0.00001)
      DO k = 1,n_em
        DO i = 1,20
          em_qual(i,k) = em_qual1(i,k)
        ENDDO
        em_qual(8,k) = em_e(5,k)
        em_qual(9,k) = em_eta(1,k)
        em_qual(10,k) = em_phi(1,k)
        em_qual(11,k) = em_e(1,k)
        em_qual(12,k) = em_e(2,k)
        em_qual(13,k) = em_e(3,k)
      ENDDO

C-        *** get isajet level and match with reconstructed quantities ***
      IF (monte_carlo_data()) THEN
        nparton = 0
        lisaj = gzisaj()
        DO WHILE (lisaj.NE.0)
          IF (abs(iq(lisaj + 1)).EQ.90) THEN
            nparton = nparton + 1
            parton_et(nparton) = sqrt(q(lisaj+2)**2. + q(lisaj+3)**2.)
            parton_eta(nparton) = q(lisaj + 9)
            parton_phi(nparton) = q(lisaj + 7)
          ENDIF
          lisaj = lq(lisaj)
        ENDDO
        delta_phi = abs(zboson_phi(1,1) -  parton_phi(1))
        IF (delta_phi.GT.pi) delta_phi = 2.*pi - delta_phi
        dis = sqrt((zboson_eta(1,1)-parton_eta(1))**2. + delta_phi**2.)
        IF (dis.LT.par_max_delta_r) zboson_e(6,1) = parton_et(1)
        IF (zboson_e(6,1).le.0.001) call errmsg('zero photon parton Et',
     &    'mpf_z_boson',' ','W')
      ENDIF
      pass_z = .true.

  999 RETURN
      END
