      SUBROUTINE mpf_jets(algorithm,pass_jets,njets,jet_e,jet_eta,
     &  jet_phi,jet_qual)

C----------------------------------------------------------------------
C-   Purpose and Methods :  Obtain information about reconstructed jets along
C-        with matching monte carlo partons
C-
C-   Inputs   :
C-        algorithm : jet reconstruction algorithm (1 = 0.7; 2 = 0.5;
C-               3 = 0.3; 4 = NN; 5 = 1.0; 6 = PTAU)
C-
C-   Outputs  :
C-        pass_jets         L  flags whether event passed jet cuts
C-        njets             I  # of jets in event
C-        jet_e(50,20)      R  energy of jets
C-              1 = Ex
C-              2 = Ey
C-              3 = Ez
C-              4 = E
C-              5 = reco. Et
C-              6 = pjets Et, unsmeared
C-                7 thru 50 spare
C-        jet_eta(10,20)    R  physics and detector eta of jets
C-        jet_phi(10,20)    R  phi of jets
C-        jet_qual(50,20)   R  quality of jets
C-              1 = EM fraction
C-              2 = merge/split flag
C-              3 = rms in eta (rms in R for PTAU)
C-              4 = rms in phi
C-              5 = # tracks in road (30 degrees wide in R for PTAU)
C-              6 = # cells above threshold
C-              7 = ICD fraction
C-              8 = CH fraction
C-              9 = max cell ratio
C-              10 = -1 if PTAU, 0 if 0.5 cone JETS
C-                11 thru 50 spare
C-
C-   Controls : MPF_JET_RESPONSE_RCP
C-
C-   Created  Feb-11-1994   Bob Kehoe
C-   Updated  Mar-06-1994   Bob Kehoe  --  add quality cuts to jets
C-   Updated  Jun-04-1994   Bob Kehoe  --  minor changes, remove Et and high
C-                                         emfraction cut
C-   Updated  Jun-25-1995   Bob Kehoe  --  add PTAU as additional jet algo.
C-   Updated  Oct-18-1995   Bob Kehoe  --  change mpf include for IBM's
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:MPF_JETRES.INC'
      INTEGER wanted
      PARAMETER (wanted = 3)
      INTEGER i,ier,k,njets,nparton,selection(10),reco_version
      INTEGER lisv1,gzisv1,iarray(10),lptau,gzptau,lhstr,gzhstr
      INTEGER nz,nz_old,jet_ident(20,limit),jet_ident1(20,limit)
      INTEGER njetsx,nj_ntau,algorithm
      REAL zv(wanted),dz(wanted),info(3,wanted)
      REAL jet_eta1(10,limit),jet_phi1(10,limit)
      REAL jet_e1(50,limit),jet_qual1(50,limit)
      REAL jet_eta(10,limit),jet_phi(10,limit)
      REAL jet_e(50,limit),jet_qual(50,limit)
      REAL jet_emf_low,maxcell_ratio,ch_frac,icd_frac,pjarray(10)
      REAL parton_et(limit),parton_phi(limit),parton_eta(limit)
      REAL delta_phi,dis,par_max_delta_r,cone,theta
      LOGICAL first,pass_jets,monte_carlo_data,yep,jet_ok
      CHARACTER*8 str(10)
      CHARACTER*40 pnam
      DATA first /.true./

C----------------------------------------------------------------------
C-        INITIALIZE VARIABLES
      IF (first) THEN
        first = .false.
        CALL ezpick('MPF_JET_RESPONSE_RCP')
        CALL ezget('icd_frac',icd_frac,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_jets','icd_frac','F')
        CALL ezget('ch_frac',ch_frac,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_jets','ch_frac','F')
        CALL ezget('maxcell_ratio',maxcell_ratio,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_jets','maxcell_ratio','F')
        CALL ezget('jet_emf_low',jet_emf_low,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_jets','jet_emf_low','F')
        CALL ezget('par_max_delta_r',par_max_delta_r,ier)
        IF (ier.NE.0) CALL errmsg('ezget error',
     &        'mpf_jets','par_max_delta_r','F')
        IF (ier.EQ.0) CALL ezget('nalgo',iarray(1),ier)
        IF (ier.EQ.0) CALL ezget('iter',iarray(2),ier)
        IF (ier.EQ.0) CALL ezget('irst',iarray(3),ier)
        IF (ier.EQ.0) CALL ezget('imuon',iarray(4),ier)
        IF (ier.EQ.0) CALL ezget('etcut',pjarray(1),ier)
        IF (ier.EQ.0) CALL ezget('spl_mrg',pjarray(3),ier)
        CALL ezrset
      ENDIF
      pass_jets = .false.

C-        *** obtain energy, direction, and quality arrays for jets ***
      selection(1) = algorithm
      IF (algorithm.NE.6) THEN
        CALL cal_jets(selection,str,limit,njets,jet_e,jet_eta,jet_phi,
     &      jet_qual1,jet_ident)
        DO k = 1,njets
C-          *** reject event if fails good jet cuts ***
          IF (jet_qual1(1,k).lt.jet_emf_low) goto 999
          IF (jet_qual1(7,k).gt.icd_frac) goto 999
          IF (jet_qual1(8,k).gt.ch_frac) goto 999
          IF (jet_qual1(9,k).ge.maxcell_ratio) goto 999
          DO i = 1,50
            jet_qual(i,k) = jet_qual1(i,k)
          ENDDO
          jet_qual(2,k) = float(jet_ident(1,k))
          jet_qual(5,k) = float(jet_ident(3,k))
          jet_qual(6,k) = float(jet_ident(4,k))
        ENDDO
      ELSE
        lhstr = gzhstr()  !  obtain vertex for detector eta calculation
        reco_version = 0
        DO WHILE((lhstr.GT.0).AND.(reco_version.EQ.0))
          CALL uhtoc(iq(lhstr+7),40,pnam,40)
          IF (pnam(1:11).EQ.'FULL_D0RECO') THEN
            reco_version = iq(lhstr + 3)
          ENDIF
          lhstr = lq(lhstr)
        ENDDO
        nz_old = nz
        CALL vzero(zv,wanted)
        CALL vzero(dz,wanted)
        CALL vertex_info(wanted,nz,info,yep)
        IF (monte_carlo_data().AND.reco_version.EQ.11.AND.nz_old.EQ.0.
     &      and.(nz.EQ.0)) THEN
          CALL errmsg('reco 11 showerlibrary','mpf_jets',
     &          'getting isv1 vertex for detector eta calc.','W')
          lisv1 = gzisv1()     ! v11 of reco needs isajet vert. for shower lib.
          IF (lisv1.NE.0) THEN
            nz = 1
            zv(1) = q(lisv1 + 9)
            dz(1) = 0.
          ENDIF
        ELSEIF (.NOT.yep) THEN
          CALL errmsg('problem getting vertex','mpf_jets',
     &          'vertex set to zero','W')
          nz = 1
          zv(1) = 0.
          dz(1) = 150.
        ELSE
          DO k = 1,min(wanted,nz)
            zv(k) = info(1,k)
            dz(k) = info(2,k)
          ENDDO
        ENDIF
        lptau = gzptau()
        njets = 0
        nj_ntau = 0
        DO WHILE(lptau.GT.0)
          njets = njets + 1
          jet_e(1,njets) = q(lptau + 3)
          jet_e(2,njets) = q(lptau + 4)
          jet_e(3,njets) = q(lptau + 5)
          jet_e(4,njets) = q(lptau + 6)
          jet_e(5,njets) = q(lptau + 7)
          jet_eta(1,njets) = q(lptau + 10)
          theta = 2.*atan(exp(-jet_eta(1,njets)))
          CALL det_eta(zv(1),theta,jet_eta(2,njets))
          jet_phi(1,njets) = q(lptau + 9)
          jet_qual(1,njets) = 0.0
          jet_qual(3,njets) = q(lptau + 11)
          jet_qual(5,njets) = float(iq(lptau + 21))
          IF (q(lptau+13).gt.0) jet_qual(9,njets) =
     &        q(lptau+12)/q(lptau+13)
          jet_qual(10,njets) = -1.0
          lptau = lq(lptau)
        ENDDO
        IF (njets.NE.0) THEN
          selection(1) = 2
          CALL cal_jets(selection,str,limit,njetsx,jet_e1,jet_eta1,
     &        jet_phi1,jet_qual1,jet_ident1)
          DO k = 1,njetsx
            jet_ok = .true.
            delta_phi = abs(jet_phi(1,1)-jet_phi1(1,k))
            IF (delta_phi.GT.pi) delta_phi = 2*pi - delta_phi
            dis = sqrt((jet_eta(1,1) - jet_eta1(1,k))**2. +
     &            delta_phi**2.)
            IF (dis.LT.0.5) jet_ok = .false.
            IF (jet_ok) THEN
              nj_ntau = nj_ntau + 1
              DO i = 1,50
                jet_e(i,nj_ntau+njets) = jet_e1(i,k)
                jet_qual(i,nj_ntau+njets) = jet_qual1(i,k)
              ENDDO
              DO i = 1,10
                jet_eta(i,nj_ntau+njets) = jet_eta1(i,k)
                jet_phi(i,nj_ntau+njets) = jet_phi1(i,k)
              ENDDO
              jet_qual(2,nj_ntau+njets) = float(jet_ident1(1,k))
              jet_qual(5,nj_ntau+njets) = float(jet_ident1(3,k))
              jet_qual(6,nj_ntau+njets) = float(jet_ident1(4,k))
              jet_qual(10,nj_ntau+njets) = 0.0
            ENDIF
          ENDDO
        ENDIF
        njets = njets + nj_ntau
      ENDIF

C-        *** get PJETS level and match with reconstructed quantities ***
      IF (monte_carlo_data()) THEN
        IF (algorithm.EQ.1.or.algorithm.eq.6) THEN
          cone = 0.7            ! approx. of PTAU algorithm
        ELSEIF (algorithm.EQ.2) THEN
          cone = 0.5
        ELSEIF (algorithm.EQ.3) THEN
          cone = 0.3
        ELSEIF (algorithm.EQ.4) THEN
          cone = 0.3            ! approx. of nearest neighbor algorithm
        ELSEIF (algorithm.EQ.5) THEN
          cone = 1.0
        ENDIF
        pjarray(2) = cone
        CALL get_pjet_vectors(iarray,pjarray,'CONE',limit,nparton,
     &        parton_et,parton_eta,parton_phi)
        DO k = 1,njets
          DO i = 1,nparton
            delta_phi = abs(jet_phi(1,k) -  parton_phi(i))
            IF (delta_phi.GT.pi) delta_phi = 2.*pi - delta_phi
            dis = sqrt((jet_eta(1,k)-parton_eta(i))**2. + delta_phi**2.)
            IF (dis.LE.par_max_delta_r) jet_e(6,k) = parton_et(i)
          ENDDO
        ENDDO
      ENDIF
      pass_jets = .true.

  999 RETURN
      END
