      SUBROUTINE FILL_MUON_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : setup array with muon related information
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-APR-1995   Meenakshi Narain
C-   Updated  26-OCT-1995   Jeffrey Bantly  add ptfix, dr_new
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:pi.def'
      INCLUDE 'D0$INC:TOP_DILEP_ANALYSIS.INC'
      INTEGER LPMUO, I,J,KMUON
      REAL  XDATA(1000), XVAR(100), DPHI,DETA
      REAL  ETAELE,ETAMUO,PHIELE,PHIMUO,DR,DREMU,ETAJET,PHIJET,DRMUJET
      REAL  ETJET_CLOSE,UETJET_CLOSE,DRGMU
      REAL  DRR,DEDIFF,PROXIM, DR_NEW, PTREL_NEW
      INTEGER NTAGS, TRULEN,KMAX,KTAGS
      INTEGER NPVERT,LVERT,GZVERH
      REAL    FIXED_PT, PTFIX
      EXTERNAL GZVERH, PTFIX
C----------------------------------------------------------------------
C
C..   get links
C
      CALL gtslink('ELE_TGHT', nwant_eletght,ntot_eletght,
     &    elec_link_tght)
      CALL gtslink('GAM_TGHT', nwant_gamtght,ntot_gamtght,
     &    phot_link_tght)
      CALL gtslink('TOP_JETS', nwant_jet, ntot_jet, jets_link)
C
      CALL gtslink('HARD_MUO', nwant_muon, ntot_muon, muon_link)
      nmuon = min(nwant_muon, ntot_muon)
      CALL gtslink('ISOLMUON', nwant_muon_tght,
     &  ntot_muon_tght, muon_link_tght)
      nmuon_tght = min(nwant_muon, ntot_muon_tght)
C
      ixmuon(1) = nmuon
      DO i=1,nmuon
        lpmuo=muon_link(i)      ! pmuo link for isolated muon
        CALL top_dilep_muinfo(lpmuo,xvar,kmuon)
        DO j=1, kmuon
          xmuon(i+(j-1)*nwant_muon+1) = xvar(j)
        END DO
        CALL MUJETS_mu_getpar(lpmuo,drr,dediff)
        xmuon(i+kmuon*nwant_muon+1) = drr
        xmuon(i+(kmuon+1)*nwant_muon+1) = dediff
        ETAMUO         = q(lpmuo+16) ! eta
        PHIMUO         = q(lpmuo+17) ! phi
        dremu = 1000.
        DO j = 1, nelec_tght
          lpelc = elec_link_tght(j)
          ETAELE         = Q(LPELC+9)
          PHIELE         = Q(LPELC+10)
          deta = etamuo - etaele
          dphi = proxim(phimuo-phiele, 0.)
          dr = sqrt(deta**2 + dphi**2)
          IF (dr.LT.dremu) dremu = dr
        END DO
        xmuon(i+(kmuon+2)*nwant_muon+1) = dremu
        drgmu = 1000.
        DO j = 1, ngam_tght
          lppho = phot_link_tght(j)
          ETAELE         = Q(LPPHO+9)
          PHIELE         = Q(LPPHO+10)
          deta = etamuo - etaele
          dphi = proxim(phimuo-phiele, 0.)
          dr = sqrt(deta**2 + dphi**2)
          IF (dr.LT.drgmu) drgmu = dr
        END DO
        xmuon(i+(kmuon+3)*nwant_muon+1) = drgmu
        drmujet=1000.
        etjet_close =  -1.
        uetjet_close = -1.
        DO j=1,njet
          ljets = jets_link(j)
          phijet = q(ljets+8)
          etajet  = q(ljets+9)
          deta = etamuo - etajet
          dphi = proxim(phimuo-phijet, 0.)
          dr = sqrt(deta**2 + dphi**2)
          IF (dr.LT.drmujet) THEN
            drmujet = dr
            etjet_close = xjet(j+1*nwant_jet+1)
            uetjet_close = xjet(j+1)
          ENDIF
        END DO
        xmuon(i+(kmuon+4)*nwant_muon+1) = drmujet
        xmuon(i+(kmuon+5)*nwant_muon+1) = etjet_close
        xmuon(i+(kmuon+6)*nwant_muon+1) = uetjet_close
        DO j=1,nmuon_tght
          IF (muon_link(i).EQ.muon_link_tght(j)) THEN
            ptr_muon_tght(i)=1
          ENDIF
        END DO
        xmuon(i+(kmuon+7)*nwant_muon+1) = ptr_muon_tght(i)
C
        fixed_pt=0.0
        LVERH=GZVERH()
        IF( LVERH.GT.0 ) NPVERT=IQ(LVERH+2)
        IF( NPVERT.LE.1 .OR. LVERH.LE.0) GOTO 15      ! Must have mult. verts.
        LVERT=LQ(LVERH-1)
        IF( LVERT.LE.0 ) GOTO 15
        fixed_pt=ptfix(lpmuo,lvert)
   15   continue
        xmuon(i+(kmuon+8)*nwant_muon+1) = fixed_pt
        CALL MUJETS_GET_RECALC_ISOLATION(LPMUO,DR_NEW,PTREL_NEW)
        xmuon(i+(kmuon+9)*nwant_muon+1) = dr_new
        xmuon(i+(kmuon+10)*nwant_muon+1) = ptrel_new
      END DO
  999 RETURN
C...........................................................................
      ENTRY MUON_INFO(NVAR,XDATA)
      nvar = nwant_muon*nvar_muon+1
      CALL ucopy(xmuon,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY MUON_TAGS(NTAGS,TMUON,KMAX)
      KMAX = nwant_muon
      CALL MUINFO_TAGS(KTAGS,TAGS)
      ntags = nvar_muon+1
      xtags(1)='DRJETM'
      xtags(2)='DENM'
      xtags(3)='DRELEM'
      xtags(4)='DRGAMM'
      xtags(5)='DRCJETM'
      xtags(6)='ETCJET_CLOSEM'
      xtags(7)='ETJET_CLOSEM'
      xtags(8)='ISOLMUONM'
      xtags(9)='PTFIXM'
      xtags(10)='DRCJET_NEWM'
      xtags(11)='PTREL_NEWM'
      IF (ktags+11.ne.nvar_muon) then
      ENDIF
      TMUON(1) = 'NMUON'
      DO i=1,ktags
        tmuon(i+1) = tags(i)(1:trulen(tags(i)))//'M'
      ENDDO
      DO i=1,11
        tmuon(ktags+i+1) = xtags(i)
      ENDDO
      RETURN
      END
