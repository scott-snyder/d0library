      SUBROUTINE FILL_GLOBAL_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : setup array with global information
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-APR-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:TOP_DILEP_ANALYSIS.INC'
      INTEGER I,J,K,NEM,NSORT,INDEX(20),JPTR
      INTEGER NJETS15,NJETS20,LPMUO
      INTEGER IOFF,IFIRST,ILAST,NTAGS,NPARTICLE
      REAL    XDATA(1000)
      REAL  DPHI_LEP12, DETA_LEP12,VECT(4),PHI12
      REAL  MISSET_CORR2_PHI, DPHI12_ECORR
      REAL  MISSET_CORR_PHI,DPHIM1MET,CONV,PSQ,MSQ,DIMUMASS
      REAL  AMASS_ETMISSPROJ,AMASS_LEP_ETMISSPROJ,ETMISS
      REAL    ELE_PX,ELE_PY,ELE_ET(3),ELE_PHI(3)
      REAL    EM_E(3),EM_ET(3),EM_PHI(3),EM_THETA(3),EM_ETA_ID(3)
      REAL    MTM_VEC(4,MAX_PARTICLE),JET(4,NWANT_JET)
      REAL    MTW,PTW,PZ_W2ND,ANUM,EM_ID(3), ELE_VEC(4),W_VEC(4)
      REAL    MTW5,PTW5,MTWG,PTWG,MTW5G,PTW5G
      REAL    GAM_ET(3),GAM_PHI(3),GAM_PX,GAM_PY
      REAL    NUT_VEC(2),EIGVAL(3),EIGVEC(3,3),PTOT(4)
      REAL    SPHERICITY,Y,APLANARITY,INVMASS_OBJ,SP,AP,Y2
      REAL    EM_MASS12,EM_MASS23,EM_MASS13
      REAL    PTR_EM12,PTR_EM23,PTR_EM13
      REAL    ETA_EM12,ETA_EM23,ETA_EM13
      REAL    PTX_EM1,PTY_EM1,PTX_EM2,PTY_EM2,PTSQ,PT_EM12
      REAL    XTHETA(2),XETA(2),XPHI(2),ET1,ET2,P1,P2,E1,E2
      REAL    W_MASS,ZMASS,XJETBIN(6),COSX,ETA
      REAL    MET5C,MET5X,MET5Y,MET5C_PHI
      LOGICAL QCC,OK
      PARAMETER( W_MASS = 80.2 )
      DATA  ZMASS/91.2/
      DATA  XJETBIN/8.,10.,15.,20.,25.,30./
      DATA CONV/ 57.29578/
C----------------------------------------------------------------------
C ****  inline function to check whether the object is in the CC
C
      QCC(ETA) = ABS(ETA).LE.12
C----------------------------------------------------------------------
C
C ****  get links for all objects
C
      CALL gtslink('ELE_TGHT', nwant_eletght,ntot_eletght,
     &    elec_link_tght)
      CALL gtslink('ELE_LSE', nwant_ele, ntot_ele, elec_link)
      CALL gtslink('GAM_TGHT', nwant_gamtght,ntot_gamtght,
     &    phot_link_tght)
      CALL gtslink('GAM_LSE', nwant_phot, ntot_phot, phot_link)
      CALL gtslink('HARD_MUO', nwant_muon, ntot_muon, muon_link)
      CALL gtslink('ISOLMUON', nwant_muon_tght,
     &  ntot_muon_tght, muon_link_tght)
      CALL gtslink('TOP_JETS', nwant_jet, ntot_jet, jets_link)
C
      MET5C=0.0
      MET5X=0.0
      MET5Y=0.0
      MET5C_PHI=0.0
C
      nem = 0
      IF (nphot+nelec.eq.0) goto 35
      IF (nphot.EQ.0) THEN
        nsort = nelec
      ELSE
        nsort = 10+nphot
      ENDIF
      CALL SORTZV(YEM,INDEX,NSORT,1,1,0)
      DO i=1,nsort
        IF (yem(index(i)).GT.0) THEN
          nem = nem+1
          IF (index(i).GT.10) THEN
            jptr = index(i) -10
            ixphot(jptr+17*(nwant_phot)+1) = nem
            lppho = phot_link(jptr)
            em_e (nem)    = q(lppho+6)
            em_et(nem)    = q(lppho+7)
            em_phi(nem)   = q(lppho+10)
            em_theta(nem) = q(lppho+8)
            em_id(nem)    = 0                   ! 0 for gam, 1 for ele
            em_eta_id(nem) = 0                  ! 0 for EC, 1 for CC
            IF (qcc(q(lppho+19))) em_eta_id(nem) = 1
          ELSE
            jptr = index(i)
            ixelec(jptr+17*(nwant_ele)+1) = nem
            lpelc = elec_link(jptr)
            em_e (nem)    = q(lpelc+6)
            em_et(nem)    = q(lpelc+7)
            em_phi(nem)   = q(lpelc+10)
            em_theta(nem) = q(lpelc+8)
            em_id(nem)    = 1
            em_eta_id(nem) = 0
            IF (qcc(q(lpelc+19))) em_eta_id(nem) = 1
          ENDIF
          IF (nem.EQ.3) GOTO 35
        ENDIF
      END DO
   35 CONTINUE
C
C ****
C
      if (nem.gt.0) then
        htej = htej + em_et(1)
      endif

C
C ****  compute inavrianty masses of first three em combinations
C
      em_mass12=-1.
      em_mass13=-1.
      em_mass23=-1.
      ptr_em12=-1.
      ptr_em13=-1.
      ptr_em23=-1.
      eta_em12=-1.
      eta_em13=-1.
      eta_em23=-1.
      pt_em12 = 0.
      IF (nem.GE.2) THEN
        cosx=cos(em_theta(1))*cos(em_theta(2))
     &      +sin(em_theta(1))*sin(em_theta(2))*
     &      cos(em_phi(1)-em_phi(2))
        em_mass12=sqrt(2*em_e(1)*em_e(2)*(1-cosx))
        ptr_em12  = em_id(1) + em_id(2)
        eta_em12  = em_eta_id(1)*10+em_eta_id(2)
c - Pt of Z
        ptx_em1 = em_et(1)*cos(em_theta(1))
        pty_em1 = em_et(1)*sin(em_theta(1))
        ptx_em2 = em_et(2)*cos(em_theta(2))
        pty_em2 = em_et(2)*sin(em_theta(2))
        ptsq = (ptx_em1+ptx_em2)*(ptx_em1+ptx_em2) +
     &          (pty_em1+pty_em2)*(pty_em1+pty_em2)
        pt_em12 = sqrt(ptsq)
c
        IF (nem.GT.2) THEN
          cosx=cos(em_theta(1))*cos(em_theta(3))
     &        +sin(em_theta(1))*sin(em_theta(3))*
     &        cos(em_phi(1)-em_phi(3))
          em_mass13=sqrt(2*em_e(1)*em_e(3)*(1-cosx))
          ptr_em13  = em_id(1) + em_id(3)
          eta_em13  = em_eta_id(1)*10+em_eta_id(3)
          cosx=cos(em_theta(2))*cos(em_theta(3))
     &        +sin(em_theta(2))*sin(em_theta(3))*
     &        cos(em_phi(2)-em_phi(3))
          em_mass23=sqrt(2*em_e(2)*em_e(3)*(1-cosx))
          ptr_em23  = em_id(2) + em_id(3)
          eta_em23  = em_eta_id(2)*10+em_eta_id(3)
        ENDIF
        xglobal(1 )=em_mass12
        ixglobal(2 )=ptr_em12
        ixglobal(3 )=eta_em12
        xglobal(4 )=em_mass13
        ixglobal(5 )=ptr_em13
        ixglobal(6 )=eta_em13
        xglobal(7 )=em_mass23
        ixglobal(8 )=ptr_em23
        ixglobal(9 )=eta_em23
        xglobal(60)=pt_em12
      ENDIF
C
C ****  count jets      - use   xglobal(10 - 15)
C
      DO i=1,njet
        IF (abs(xjet(i+5*nwant_jet+1)).lt.2.5) then
          ifirst = 0
          ilast = 0
          DO j=1,6
            IF (xjet(i+nwant_jet+1).ge.xjetbin(j)) then
              IF (ifirst.EQ.0) ifirst = j
              ilast = j
            ENDIF
          ENDDO
          DO k=ifirst,ilast
            ixglobal(9+k) = ixglobal(9+k) + 1
          ENDDO
        ENDIF
      ENDDO
      DO j=1,6
        IF (xjetbin(j).EQ.15.) THEN
          njets15 = ixglobal(9+j)
        ELSE IF (xjetbin(j).EQ.20.) THEN
          njets20 = ixglobal(9+j)
        ENDIF
      ENDDO
C
C ****  HT
C
C
      DO i=1,4
        xglobal(15+i) = ht(i)        ! HT
      END DO
      xglobal(20) = htej        ! HTEJJ

C
C ****  PTW, MTW, sphericity, aplanarity etc for e+jets
C
      mtw = 0
      ptw = 0
      mtw5 = 0
      ptw5 = 0
      DO i = 1,min(nelec,2)
        lpelc = elec_link(i)
        ele_px = q(lpelc+3)
        ele_py = q(lpelc+4)
        ele_et(i) = q(lpelc+7)
        ele_phi(i) = q(lpelc+10)
        mtw = sqrt(2*ele_et(i)*metc*(1.-cos(ele_phi(i)-metc_phi)))
        ptw = sqrt((ele_px+metx)**2 + (ele_py+mety)**2)
        mtw5 = sqrt(2*ele_et(i)*met5c*(1.-cos(ele_phi(i)-met5c_phi)))
        ptw5 = sqrt((ele_px+met5x)**2 + (ele_py+met5y)**2)
        DO k = 1, 4
          ele_vec(k) = q(lpelc+2+k)
        END DO
        IF (i.EQ.1) THEN
          CALL find_wlnu(w_mass,ele_vec,nut_vec,w_vec, pz_w2nd, ok)
          IF(nparticle.GE.max_particle) nparticle = max_particle -1
          nparticle = nparticle+1
          DO k=1,4
            mtm_vec(k,nparticle) = w_vec(k)
          END DO
          CALL sphericity0(nparticle,mtm_vec,0,sphericity,y,
     &        aplanarity,
     &        eigval,eigvec,ptot,invmass_obj)
C
          nparticle = nparticle-1
          IF(nparticle+2.ge.max_particle) nparticle = max_particle -2
          nparticle = nparticle+1
          DO k=1,4
            mtm_vec(k,nparticle) = ele_vec(k)
          END DO
          nparticle = nparticle+1
          DO k=1,2
            mtm_vec(k,nparticle) = nut_vec(k)
          END DO
          mtm_vec(3,nparticle) = 0
          mtm_vec(4,nparticle) = metc
          CALL sphericity0(nparticle,mtm_vec,0,sp,y2,ap,
     &        eigval,eigvec,ptot,invmass_obj)
        ENDIF
        xglobal(21+2*(i-1)) = mtw      ! Transverse mass W
        xglobal(22+2*(i-1)) = ptw      ! Pt W
        xglobal(25+2*(i-1)) = mtw5      ! Transverse mass W
        xglobal(26+2*(i-1)) = ptw5      ! Pt W
        IF (i.EQ.1) THEN
          xglobal(29)  = sphericity    ! sph
          xglobal(30)  = y            ! normal to sp
          xglobal(31)  = aplanarity   ! aplanarity
          xglobal(32)  = sp
          xglobal(33)  = y2
          xglobal(34)  = ap
        ENDIF
      ENDDO
C
C ****  MWJ masses
C
      DO i=1,min(njet,5)
        anum =               (w_vec(4)+jet(4,i))**2
     &                     - (w_vec(1)+jet(1,i))**2
     &                     - (w_vec(2)+jet(2,i))**2
     &                     - (w_vec(3)+jet(3,i))**2
        IF (anum.GT.0) THEN
          xglobal(34+i) = sqrt(anum)
        ELSE
          CALL ERRMSG(' Sqrt of -ve in wj mass comb','ee',' ','W')
        ENDIF
      ENDDO
C
C-    alplanarity, y and sphericity for gam+jets events
C
      mtwg = 0
      ptwg = 0
      mtw5g = 0
      ptw5g = 0
      IF (NPHOT.GT.0) THEN
        DO i = 1,min(nphot,2)
          lppho = phot_link(i)
          gam_px = q(lppho+3)
          gam_py = q(lppho+4)
          gam_et(i) = q(lppho+7)
          gam_phi(i) = q(lppho+10)
          mtwg = sqrt(2*gam_et(i)*metc*(1.-cos(gam_phi(i)-metc_phi)))
          ptwg = sqrt((gam_px+metx)**2 + (gam_py+mety)**2)
          mtw5g = sqrt(2*gam_et(i)*met5c*(1.-cos(gam_phi(i)-met5c_phi)))
          ptw5g = sqrt((gam_px+met5x)**2 + (gam_py+met5y)**2)
          xglobal(41+2*(i-1)) = mtwg      ! Transverse mass W
          xglobal(42+2*(i-1)) = ptwg      ! Pt W
          xglobal(45+2*(i-1)) = mtw5g      ! Transverse mass W
          xglobal(46+2*(i-1)) = ptw5g      ! Pt W
        end do
        nparticle = nparticle_jets
        DO k=1,4
          DO j=1,nparticle
            mtm_vec(k,nparticle) = jet(k,nparticle)
          ENDDO
        ENDDO
        IF(nparticle.GE.max_particle) nparticle = max_particle -1
        nparticle = nparticle+1
        lppho = phot_link(1)
        DO k=1,4
          mtm_vec(k,nparticle) = q(lppho+2+k)
        END DO
        CALL sphericity0(nparticle,mtm_vec,0,sp,y2,ap,
     &      eigval,eigvec,ptot,invmass_obj)
        xglobal(49)  = sp
        xglobal(50)  = ap
        IF (nphot.GT.1) THEN
          nparticle = nparticle+1
          lppho = phot_link(2)
          DO k=1,4
            mtm_vec(k,nparticle) = q(lppho+2+k)
          END DO
          CALL sphericity0(nparticle,mtm_vec,0,sp,y2,ap,
     &        eigval,eigvec,ptot,invmass_obj)
          xglobal(51)  = ap
        ENDIF
      ENDIF
C
C ****  COMPUTE INVARIANT MASS USING THE MISSING ET PROJECTIONS ON LEPTON AXES
C
      IOFF = 50
      IF (NELEC_TGHT.GE.1.AND.NMUON_TGHT.GE.1) THEN
        lpelc=elec_link_tght(1)
        lpmuo=muon_link_tght(1)
        xtheta(1) = q(lpelc+8)
        xtheta(2) = q(lpmuo+15)
        xphi(1)   = q(lpelc+10)
        xphi(2)   = q(lpmuo+17)
        xeta(1) = q(lpelc+9)
        xeta(2) = q(lpmuo+16)
        et1     = q(lpelc+7)
        et2     = q(lpmuo+14)
        p1 = xphi(1) - xmet(4+nwant_met+1)
        p2 = xphi(2) - xmet(4+nwant_met+1)
        etmiss = xmet(4+1)
        if (xphi(1).ne.xphi(2)) then
        cosx=cos(xtheta(1))*cos(xtheta(2))
     &      +sin(xtheta(1))*sin(xtheta(2))*
     &      cos(p1-p2)
        E1=(ETMISS/COS(P1)/(1-TAN(P1)/TAN(P2))+et1)*COSH(xeta(1))
        E2=(ETMISS/COS(P2)/(1-TAN(P2)/TAN(P1))+ET2)*COSH(xeta(2))
        amass_lep_etmissproj=2*e1*e2*(1-cosx)
        if (amass_lep_etmissproj.gt.0)
     &    amass_lep_etmissproj=sqrt(amass_lep_etmissproj)
        E1=(ETMISS/COS(P1)/(1-TAN(P1)/TAN(P2)))*COSH(xeta(1))
        E2=(ETMISS/COS(P2)/(1-TAN(P2)/TAN(P1)))*COSH(xeta(2))
        amass_etmissproj=2*e1*e2*(1-cosx)
        if (amass_etmissproj.gt.0)
     &    amass_etmissproj=sqrt(amass_etmissproj)
        endif
        XGLOBAL(IOFF+1) = amass_etmissproj
        XGLOBAL(IOFF+2) = amass_lep_etmissproj
      ENDIF
      IF (nem.GE.2) THEN
        xtheta(1) = em_theta(1)
        xtheta(2) = em_theta(2)
        xphi(1)   = em_phi(1)
        xphi(2)   = em_phi(2)
        et1       = em_et(1)
        et2       = em_et(2)
        xeta(1)   = -ALOG(TAN(xtheta(1)/2.))
        xeta(2)   = -ALOG(TAN(xtheta(2)/2.))
        if (xphi(1).ne.xphi(2)) then
        p1 = xphi(1) - xmet(4+nwant_met+1)
        p2 = xphi(2) - xmet(4+nwant_met+1)
        etmiss = xmet(4+1)
        cosx=cos(xtheta(1))*cos(xtheta(2))
     &      +sin(xtheta(1))*sin(xtheta(2))*
     &      cos(p1-p2)
        E1=(ETMISS/COS(P1)/(1-TAN(P1)/TAN(P2))+ET1)*COSH(xeta(1))
        E2=(ETMISS/COS(P2)/(1-TAN(P2)/TAN(P1))+ET2)*COSH(xeta(2))
        amass_lep_etmissproj=2*e1*e2*(1-cosx)
        if (amass_lep_etmissproj.gt.0)
     &    amass_lep_etmissproj=sqrt(amass_lep_etmissproj)
        E1=(ETMISS/COS(P1)/(1-TAN(P1)/TAN(P2)))*COSH(xeta(1))
        E2=(ETMISS/COS(P2)/(1-TAN(P2)/TAN(P1)))*COSH(xeta(2))
        amass_etmissproj=2*e1*e2*(1-cosx)
        if (amass_etmissproj.gt.0)
     &    amass_etmissproj=sqrt(amass_etmissproj)
        else
          XGLOBAL(IOFF+3) = -99.
          XGLOBAL(IOFF+4) = -99.
        endif
        XGLOBAL(IOFF+3) = amass_etmissproj
        XGLOBAL(IOFF+4) = amass_lep_etmissproj
      ENDIF
      IF (NMUON_TGHT.GE.2) THEN
        lpmuo=muon_link_tght(1)
        et1       = q(muon_link_tght(1)+14)
        xtheta(1) = q(muon_link_tght(1)+15)
        xphi(1)   = q(muon_link_tght(1)+17)
        xeta(1)   = q(muon_link_tght(1)+16)
        lpmuo=muon_link_tght(2)
        et2       = q(muon_link_tght(2)+14)
        xtheta(2) = q(muon_link_tght(2)+15)
        xphi(2)   = q(muon_link_tght(2)+17)
        xeta(2)   = q(muon_link_tght(2)+16)
        if (xphi(1).ne.xphi(2)) then
        p1 = xphi(1) - xmet(4+nwant_met+1)
        p2 = xphi(2) - xmet(4+nwant_met+1)
        etmiss = xmet(4+1)
        cosx=cos(xtheta(1))*cos(xtheta(2))
     &      +sin(xtheta(1))*sin(xtheta(2))*
     &      cos(p1-p2)
c        E1=(ETMISS/COS(P1)/(1-TAN(P1)/TAN(P2))+ET1)*COSH(xeta(1))
c        E2=(ETMISS/COS(P2)/(1-TAN(P2)/TAN(P1))+ET2)*COSH(xeta(2))
        amass_lep_etmissproj=2*e1*e2*(1-cosx)
        if (amass_lep_etmissproj.gt.0)
     &    amass_lep_etmissproj=sqrt(amass_lep_etmissproj)
c        E1=(ETMISS/COS(P1)/(1-TAN(P1)/TAN(P2)))*COSH(xeta(1))
c        E2=(ETMISS/COS(P2)/(1-TAN(P2)/TAN(P1)))*COSH(xeta(2))
        amass_etmissproj=2*e1*e2*(1-cosx)
        if (amass_etmissproj.gt.0)
     &    amass_etmissproj=sqrt(amass_etmissproj)
        else
          XGLOBAL(IOFF+5) = -99.
          XGLOBAL(IOFF+6) = -99.
        endif
        XGLOBAL(IOFF+5) = amass_etmissproj
        XGLOBAL(IOFF+6) = amass_lep_etmissproj
      ENDIF

C
C ****  EE analysis
C
C
C- 2 electron cut
      ioff = 45
      IF (.NOT.(nem.GE.2)) GOTO 800
      ixglobal(ioff+7) = 1
      IF (abs(ptr_em12).EQ.2) THEN        ! ee
        ixglobal(ioff+1) = 1
      ELSE IF (ptr_em12.EQ.0) THEN        ! gg
      ELSE                                ! eg
        ixglobal(ioff+6) = 1
      ENDIF
C- Reject Z events
      ixglobal(ioff+2) = 1
      IF (abs(em_mass12-zmass).le.12.and.metc.lt.40.) then
        ixglobal(ioff+2) = 0
      ENDIF
C- reject events with metc.lt.25 GeV
      ixglobal(ioff+3) = 1
      IF (metc.LT.25) THEN
        ixglobal(ioff+3) = 0
      ENDIF
C- reject events with njet15<1
      ixglobal(ioff+4) = 1
      njets15 = ixglobal(12)
      IF (njets15.LT.1) THEN
        ixglobal(ioff+4) = 0
      ENDIF
C- reject events with njet15<2
      ixglobal(ioff+5) = 1
      IF (njets15.LT.2) THEN
        ixglobal(ioff+5) = 0
      ENDIF
  800 CONTINUE
C
C ****  mumu global variables
C ****  use HARD_MUO muons, NOT tight (ie, isolated) muons
C
      ioff = 64
C
C *** dphi(max):leading muon pt, Et(miss3)
C
      MISSET_CORR_PHI = xmet(3+nwant_met+1)
      if(ntot_muon.gt.0) then
        DPHIM1MET=Q(muon_link(1)+17)-MISSET_CORR_PHI
        IF(DPHIM1MET.LT.0) DPHIM1MET=DPHIM1MET+TWOPI
        IF(DPHIM1MET.GT.PI) DPHIM1MET=TWOPI-DPHIM1MET
        DPHIM1MET=DPHIM1MET*CONV
        xglobal(ioff+4) = DPHIM1MET
      endif
C
C
C *** pmuon bank statistics
C
      ixglobal(ioff+6) = ntot_muon
      CALL gtslink('SOFT_MUO', nwant_pmuo, ntot_pmuo, pmuo_link)
      ixglobal(ioff+7) = ntot_pmuo + ntot_muon
C
C ***** Dimuons (nmuon.gt.2 from here on)
C
      IF (ntot_muon.LT.2) GOTO 850
C
C *** dPhi max (muon1-muon2)
C
      DPHI_LEP12=ABS( Q(muon_link(1)+17)-Q(muon_link(2)+17) )
      IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
      DPHI_LEP12=DPHI_LEP12*CONV
      xglobal(ioff+1) = DPHI_LEP12
C
C *** dEta max (muon1-muon2)
C
      DETA_LEP12=ABS(Q(muon_link(1)+16)+Q(muon_link(2)+16))
      xglobal(ioff+2) = DETA_LEP12
C
C *** dphi(max):dimuon pt, Et(miss2)
C
      CALL VZERO(VECT,4)
      CALL VADD(Q(muon_link(1)+10),VECT(1),VECT(1),4)
      CALL VADD(Q(muon_link(2)+10),VECT(1),VECT(1),4)
      PHI12=ATAN2(VECT(2),VECT(1))
      IF(PHI12.LT.0) PHI12=PHI12+TWOPI
      IF(PHI12.GT.PI) PHI12=TWOPI-PHI12
      MISSET_CORR2_PHI = xmet(2+nwant_met+1)
      DPHI12_ECORR=ABS(PHI12-MISSET_CORR2_PHI)
      IF(DPHI12_ECORR.GT.PI) DPHI12_ECORR=TWOPI-DPHI12_ECORR
      DPHI12_ECORR=CONV*DPHI12_ECORR
      xglobal(ioff+3) = DPHI12_ECORR
C
C *** dimuon invariant mass
C
      CALL VZERO(VECT,4)
      CALL VADD(Q(muon_link(1)+10),VECT(1),VECT(1),4)
      CALL VADD(Q(muon_link(2)+10),VECT(1),VECT(1),4)
      PSQ=VECT(1)**2+VECT(2)**2+VECT(3)**2
      MSQ=VECT(4)**2-PSQ
      IF(MSQ.GT.0.) THEN
        DIMUMASS=SQRT(MSQ)
      ELSE
        DIMUMASS=0.0
      ENDIF
      xglobal(ioff+5) = DIMUMASS
  850 continue
  999 RETURN
C...........................................................................
C
      ENTRY EVT_GLBINFO(NVAR,XDATA)
      nvar = 72
      CALL ucopy(xglobal,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY EVT_GLBINFO_TAGS(NTAGS,TGLB)
      ntags = 72
      TGLB(1) ='MEM12'
      TGLB(2) ='PTREM12:I'
      TGLB(3) ='ETAEM12:I'
      TGLB(4) ='MEM23'
      TGLB(5) ='PTREM23:I'
      TGLB(6) ='ETAEM23:I'
      TGLB(7) ='MEM13'
      TGLB(8) ='PTREM13:I'
      TGLB(9) ='ETAEM13:I'
      TGLB(10)='NJET8:I'
      TGLB(11)='NJET10:I'
      TGLB(12)='NJET15:I'
      TGLB(13)='NJET20:I'
      TGLB(14)='NJET25:I'
      TGLB(15)='NJET30:I'
      TGLB(16)='HT40'
      TGLB(17)='HT25'
      TGLB(18)='HT20'
      TGLB(19)='HT10'
      TGLB(20)='HTEJJ'
      TGLB(21)='MTWE41'
      TGLB(22)='PTWE41'
      TGLB(23)='MTWE42'
      TGLB(24)='PTWE42'
      TGLB(25)='MTWE51'
      TGLB(26)='PTWE51'
      TGLB(27)='MTWE52'
      TGLB(28)='PTWE52'
      TGLB(29)='SPHER'
      TGLB(30)='Y'
      TGLB(31)='APLANAR'
      TGLB(32)='SPHERC'
      TGLB(33)='YC'
      TGLB(34)='APC'
      TGLB(35)='MWJ1'
      TGLB(36)='MWJ2'
      TGLB(37)='MWJ3'
      TGLB(38)='MWJ4'
      TGLB(39)='MWJ5'
      TGLB(40)='DPHI'   ! ==> not implemented
      TGLB(41)='MTWP41'
      TGLB(42)='PTWP41'
      TGLB(43)='MTWP42'
      TGLB(44)='PTWP42'
      TGLB(45)='MTWP51'
      TGLB(46)='PTWP51'
      TGLB(47)='MTWP52'
      TGLB(48)='PTWP52'
      TGLB(49)='APLNARG'
      TGLB(50)='SPHERG'
      TGLB(51)='APLNARGG'
      TGLB(52)='EMUMASS_METPROJ'
      TGLB(53)='EMUMASS_LEPMETPROJ'
      TGLB(54)='EEMASS_METPROJ'
      TGLB(55)='EEMASS_LEPMETPROJ'
      TGLB(56)='MM_MASS_METPROJ'
      TGLB(57)='MM_MASS_LEPMETPROJ'
      TGLB(58)='EV_EE:I'
      TGLB(59)='EV_ZMS:I'
      TGLB(60)='EV_MSET:I'
      TGLB(61)='EV_N1J15:I'
      TGLB(62)='EV_N2J15:I'
      TGLB(63)='EV_EEEG:I'
      TGLB(64)='EV_2EM:I'
      TGLB(65)='DIMU_DPHI'
      TGLB(66)='DIMU_DETAMAX'
      TGLB(67)='DIMU_DPHI_PTMET2'
      TGLB(68)='DIMU_DPHI_MUMET3'
      TGLB(69)='DIMU_MASS'
      TGLB(70)='NMUON_TOT:I'
      TGLB(71)='NPMUO_TOT:I'
      TGLB(72)='PT_EM12'
      RETURN
      END
