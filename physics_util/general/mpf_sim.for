      SUBROUTINE mpf_sim

C----------------------------------------------------------------------
C-   Purpose and Methods :  to simulate basic direct photon events to study
C-        general characteristics of mpf method.
C-
C-   Created   4-NOV-1993   Rich Astur, Bob Kehoe
C-   Updated   Oct-18-1994  Bob Kehoe -- add Pt kick to events and make menu
C-   Updated   Mar-12-1995  Bob Kehoe -- put into subroutine form and merge into
C-                                       mpf_jet_response package
C-   Updated   Jun-29-1995  Bob Kehoe -- CWN-ize
C-   Updated   Oct-18-1995  Bob Kehoe -- change mpf include for IBM's
C-   Updated   Feb-20-1996  V.Daniel Elvira -- replace RAN generator by RANF
C-                          (cern) since RAN is not a standard fortran function
C-                          (d0cha complained)
C----------------------------------------------------------------------

      IMPLICIT NONE
      include 'D0$INC:MPF_JETRES.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER spectrum_power,ier,seed,icount
      REAL energy,em_et_thresh
      REAL em_e(5),em_phi,det_phot,phot_resolution
      REAL jet_e(5),jet_phi,det_jet,jet_eta,ej_prime,theta
      REAL jet_response,jet_response_base,jet_response_slope
      REAL jet_resolution,jet_et_thresh,jet_et_unrecod
      REAL kt_mean,kt_vec(5),soft_response,soft_e(5)
      REAL rj,rjg,mpf_old,delta_phi,x,r,a0,y,nex,ney
      real noise,noise_resol,under_evt
      real ranf
      logical ntp_set,first
      external ranf
      data first/.true./

c----------------------------------------------------------------------
      jet_response(energy) = (jet_response_base +
     &      jet_response_slope*energy)
      PRINT *,'starting simulator run...'

c-      *** initialize variables ***
      call dhshow
      IF (first) THEN
        CALL inrcp('MPF_JET_RESPONSE_RCP',ier)
        IF (ier.NE.0) CALL errmsg('inrcp failed','mpf_sim',
     &    'error in inrcp','F')
        CALL ezpick('MPF_JET_RESPONSE_RCP')
        CALL ezget('parton_et_thresh',mc_etthresh,ier)
        IF (ier.EQ.0) CALL ezget('em_et_thresh',em_et_thresh,ier)
        IF (ier.EQ.0) CALL ezget('jet_et_thresh',jet_et_thresh,ier)
        IF (ier.EQ.0) CALL ezget('spectrum_power',spectrum_power,ier)
        IF (ier.EQ.0) CALL ezget('em_resolution',phot_resolution,ier)
        IF (ier.EQ.0) CALL ezget('jet_resolution',jet_resolution,ier)
        IF (ier.EQ.0) CALL ezget('jet_response_base',jet_response_base,
     &          ier)
        IF (ier.EQ.0) CALL ezget('jet_response_slope',
     &          jet_response_slope,ier)
        IF (ier.EQ.0) CALL ezget('kt_mean',kt_mean,ier)
        IF (ier.EQ.0) CALL ezget('soft_response',soft_response,ier)
        IF (ier.EQ.0) CALL ezget('under_evt',under_evt,ier)
        IF (ier.EQ.0) CALL ezget('noise',noise,ier)
        IF (ier.EQ.0) CALL ezget('noise_resol',noise_resol,ier)        
        IF (ier.EQ.0) CALL ezget('seed',seed,ier)
        IF (ier.EQ.0) CALL ezget('num_generate',icount,ier)
        IF (ier.EQ.0) CALL ezget('ntp_set',ntp_set,ier)
        IF (ier.NE.0) CALL errmsg('ezget failed','mpf_sim',
     &    'error in rcp','F')
        CALL ezrset
        call ranset(seed)
        first = .false.
      ENDIF

C-      *** generate partons with cross-section given by spectrum_power law ***
   10 r = min(ranf(),.9998)
      IF (spectrum_power.LT.0) THEN
        a0 = (abs(spectrum_power)-1)*mc_etthresh**
     &          (abs(spectrum_power)-1)
        x = a0/(1.-r)
        x = x/float(abs(spectrum_power)-1)
        et_prtn = exp((1./float(abs(spectrum_power)-1))*alog(x))
      ELSEIF (spectrum_power.EQ.0) THEN
        et_prtn = r*100.
      ELSE
        et_prtn = 0.
      ENDIF

C-      *** generate kt kick ***
      CALL NORRAN(y)
      kt_vec(5) = 1.45*abs(kt_mean*y)
      kt_phi = 2.0*pi*ranf()
      IF (kt_phi.LT.0.) kt_phi = kt_phi + 2.*pi
      kt_vec(1) = kt_vec(5)*cos(kt_phi)
      kt_vec(2) = kt_vec(5)*sin(kt_phi)
      soft_phi = kt_phi + pi
      IF (soft_phi.GT.2.*pi) soft_phi = kt_phi - pi
      soft_e(1) = kt_vec(5)*cos(soft_phi)*soft_response
      soft_e(2) = kt_vec(5)*sin(soft_phi)*soft_response
      soft_e(5) = sqrt(soft_e(1)**2. + soft_e(2)**2.)

C-      *** add kt to photon and smear photon with resolution ***
      em_e(1) = kt_vec(1)/2.0
      em_e(2) = et_prtn + kt_vec(2)/2.0
      em_e(5) = sqrt(em_e(1)**2. + em_e(2)**2.)
      emc_epr(1) = em_e(5)
      em_phi = atan2(em_e(2),(em_e(1)+epsilon))
      IF (em_phi.LT.0.) em_phi = em_phi + 2.*pi
      CALL NORRAN(y)
      det_phot = y*phot_resolution/sqrt(em_e(5))
      em_e(1) = em_e(1)*(1. + det_phot)
      em_e(2) = em_e(2)*(1. + det_phot)
      em_e(5) = em_e(5)*(1. + det_phot)
      IF (em_e(5).LT.em_et_thresh) GOTO 10

C-      *** add kt to jet, apply response and smear jet with resolution ***
      jet_e(1) = kt_vec(1)/2.0
      jet_e(2) = -et_prtn + kt_vec(2)/2.0
      jet_e(5) = sqrt(jet_e(1)**2. + jet_e(2)**2.)
      jet_phi = atan2(jet_e(2),(jet_e(1)+epsilon))
      IF (jet_phi.LT.0.) jet_phi = jet_phi + 2.*pi
      theta = 0.08 + 0.95*pi*ranf()
      jet_eta = -alog(tan(theta/2.0))
      jet_e(3) = jet_e(5)/tan(theta)
      jet_e(4) = sqrt(jet_e(5)**2. + jet_e(3)**2.)
      jt_epr7(1) = jet_e(4)
      CALL NORRAN(y)
      det_jet = y*jet_resolution/sqrt(jt_epr7(1))
      jet_et_unrecod = jet_e(5)*jet_response(jt_epr7(1))*(1. + det_jet)
      call norran(y)
      offset = noise*sin(theta) + under_evt
      offset = offset + y*noise_resol*offset
      jet_et_unrecod = jet_et_unrecod + offset
      if (jet_et_unrecod.gt.jet_et_thresh) then
        jet_e(1) = jet_e(1)*jet_response(jt_epr7(1))*(1. + det_jet)
        jet_e(2) = jet_e(2)*jet_response(jt_epr7(1))*(1. + det_jet)
        jet_e(3) = jet_e(3)*jet_response(jt_epr7(1))*(1. + det_jet)
        jet_e(4) = jet_e(4)*jet_response(jt_epr7(1))*(1. + det_jet)
        jet_e(5) = jet_et_unrecod
      else
        jet_e(1) = 0.
        jet_e(2) = 0.
        jet_e(3) = 0.
        jet_e(4) = 0.
        jet_e(5) = 0.
      endif

C-      *** calculate missing Et and mpf with old definition and new ***
      nevt = nevt + 1
      mex = -1.0*(em_e(1) + jet_e(1) + soft_e(1))
      mey = -1.0*(em_e(2) + jet_e(2) + soft_e(2))
      met = sqrt(mex**2. + mey**2.)
      met_phi = atan2(mey,(mex+epsilon))
      IF (met_phi.LT.0.) met_phi = met_phi + 2.*pi
      nex = jet_e(1)/jet_e(5)
      ney = jet_e(2)/jet_e(5)
      mpf_old = (mex*nex + mey*ney)/jet_e(5)
      rj = 1./(1. + mpf_old)
      nex = em_e(1)/em_e(5)
      ney = em_e(2)/em_e(5)
      mpf = (mex*nex + mey*ney)/em_e(5)
      rjg = 1. + mpf
      ej_prime = em_e(5)*cosh(jet_eta)
      delta_phi = abs(em_phi - jet_phi)
      IF (delta_phi.GT.pi) delta_phi = 2.*pi - delta_phi

C-      *** fill histograms and ntuples ***
      CALL hfill(100,et_prtn,0.,1.)
      CALL hfill(101,em_e(5),0.,1.)
      CALL hfill(102,jet_e(5),0.,1.)
      CALL hfill(103,kt_vec(5),0.,1.)
      CALL hfill(104,soft_e(5),0.,1.)
      CALL hfill(105,met,0.,1.)
      CALL hfill(111,em_phi,0.,1.)
      CALL hfill(112,jet_phi,0.,1.)
      CALL hfill(113,kt_phi,0.,1.)
      CALL hfill(114,soft_phi,0.,1.)
      CALL hfill(115,met_phi,0.,1.)
      CALL hfill(121,(jt_epr7(1)-ej_prime),0.,1.)
      CALL hfill(122,jet_eta,0.,1.)
      CALL hfill(202,jet_e(5),mpf_old,1.)
      CALL hfill(203,em_e(5),mpf_old,1.)
      CALL hfill(205,em_e(5),mpf,1.)
      CALL hfill(212,jet_e(5),rj,1.)
      CALL hfill(213,em_e(5),rj,1.)
      CALL hfill(215,delta_phi,0.,1.)
      CALL hfill(562,em_e(5),jet_e(5),1.)
      CALL hfill(512,em_e(5),rjg,1.)
      CALL hfill(572,ej_prime,jet_e(4),1.)
      CALL hfill(522,ej_prime,rjg,1.)
      IF (ntp_set) THEN
        eventnum = nevt
        kt = kt_vec(5)
        soft_et = soft_e(5)
        soft_ex = soft_e(1)
        soft_ey = soft_e(2)
        drell_yan = .false.
        n_emc = 1
        nele2 = 0
        nele3 = 0
        emc_et(1) = em_e(5)
        emc_ex(1) = em_e(1)
        emc_ey(1) = em_e(2)
        emc_phi(1) = em_phi
        algo = 1
        eprime(1) = ej_prime
        del_phi(1) = delta_phi
        mpf_jet(1) = mpf_old
        nj7 = 1
        jt_et7(1) = jet_e(5)
        jt_e7(1) = jet_e(4)
        jt_ex7(1) = jet_e(1)
        jt_ey7(1) = jet_e(2)
        jt_eta7(1) = jet_eta
        jt_deta7(1) = jet_eta
        jt_phi7(1) = jet_phi
        scalr_et = (soft_et + emc_et(1) + jt_et7(1))
        ht_em = emc_et(1)
        ht(1) = jt_et7(1)
        CALL hfnt(nt_id)
      ENDIF
      icount = icount - 1
      IF (icount.GT.0) GOTO 10

  999 RETURN
      END
