CDECK  ID>, IPARTNS.
      SUBROUTINE IPARTNS(NPRTNS,IDS,PRTNS,IDQ,WEIGHT,WZDK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     fill PJETS array from a list of input partons
C-   Inputs  :
C-     NPRTNS          = number of partons
C-     IDS(NPRTNS)     = parton ids
C-     PRTNS(4,NPRTNS) = parton 4 vectors
C-     IDQ(2)          = initial partons
C-     WEIGHT          = weight
C-     WZDK            = if true last 2 partons are from W,Z decay
C-
C-
C-   Created   8-OCT-1991   Serban D. Protopopescu
C-   Updated  17-APR-1996   Serban D. Protopopescu
C-    added entry evolv_cuts to supply evolution limits
C-    modified DrellYan (keys(3)) to stay within VECBOS jet ranking
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NPRTNS,IDS(NPRTNS),IDQ(2)
      REAL    PRTNS(4,NPRTNS),WEIGHT
      LOGICAL WZDK
      COMMON/FINAL/NKINF,SIGF,ALUM,ACCEPT,NRECS
      SAVE /FINAL/
      INTEGER   NKINF,NRECS
      REAL      SIGF,ALUM,ACCEPT
      COMMON/IDRUN/IDVER,IDG(2),IEVT,IEVGEN
      SAVE /IDRUN/
      INTEGER   IDVER,IDG,IEVT,IEVGEN
      COMMON/JETPAR/P(3),PT(3),YJ(3),PHI(3),XJ(3),TH(3),CTH(3),STH(3)
     1 ,JETTYP(3),SHAT,THAT,UHAT,QSQ,X1,X2,PBEAM(2)
     2 ,QMW,QW,QTW,YW,XW,THW,QTMW,PHIW,SHAT1,THAT1,UHAT1,JWTYP
     3 ,ALFQSQ,CTHW,STHW,Q0W
     4 ,INITYP(2),ISIGS,PBEAMS(5)
      SAVE /JETPAR/
      INTEGER   JETTYP,JWTYP,INITYP,ISIGS
      REAL      P,PT,YJ,PHI,XJ,TH,CTH,STH,SHAT,THAT,UHAT,QSQ,X1,X2,
     +          PBEAM,QMW,QW,QTW,YW,XW,THW,QTMW,PHIW,SHAT1,THAT1,UHAT1,
     +          ALFQSQ,CTHW,STHW,Q0W,PBEAMS
      COMMON/KEYS/IKEYS,KEYON,KEYS(10)
      COMMON/XKEYS/REAC
      SAVE /KEYS/,/XKEYS/
      LOGICAL KEYS
      LOGICAL KEYON
      CHARACTER*8 REAC
      INTEGER   IKEYS
      COMMON/NODCAY/NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOGRAV
      SAVE /NODCAY/
      LOGICAL NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOGRAV
      INTEGER   MXPTCL,IPACK
      PARAMETER (MXPTCL=4000,IPACK=10000)
      COMMON/PARTCL/NPTCL,PPTCL(5,MXPTCL),IORIG(MXPTCL),IDENT(MXPTCL)
     1,IDCAY(MXPTCL)
      SAVE /PARTCL/
      INTEGER   NPTCL,IORIG,IDENT,IDCAY
      REAL      PPTCL
      INTEGER MXJETS
      PARAMETER (MXJETS=10)
      COMMON/PJETS/PJETS(5,MXJETS),IDJETS(MXJETS),QWJET(5),IDENTW
     $,PPAIR(5,4),IDPAIR(4),JPAIR(4),NPAIR,IFRAME(MXJETS)
      SAVE /PJETS/
      INTEGER   IDJETS,IDENTW,IDPAIR,JPAIR,NPAIR,IFRAME
      REAL      PJETS,QWJET,PPAIR
      COMMON/PRIMAR/NJET,SCM,HALFE,ECM,IDIN(2),NEVENT,NTRIES,NSIGMA
      SAVE /PRIMAR/
      INTEGER   NJET,IDIN,NEVENT,NTRIES,NSIGMA
      REAL      SCM,HALFE,ECM
      INTEGER MXGOQ
      PARAMETER (MXGOQ=85)
      COMMON/Q1Q2/GOQ(MXGOQ,3),GOALL(3),GODY(4),STDDY,GOWW(25,2),
     $ALLWW(2),GOWMOD(25,3)
      SAVE /Q1Q2/
      LOGICAL GOQ,GOALL,GODY,STDDY,GOWW,ALLWW,GOWMOD
      COMMON/TOTALS/NKINPT,NWGEN,NKEEP,SUMWT,WT
      SAVE /TOTALS/
      INTEGER   NKINPT,NWGEN,NKEEP
      REAL      SUMWT,WT
      REAL    SUM(4),AMASS
      INTEGER K,J,IWZ,ID,NQS
      INTEGER maxq
      PARAMETER (maxq=15)
      INTEGER i,np,jdord(maxq),jiord(maxq),npj
      REAL    etaq(maxq),phiq(maxq),thq(maxq),ptq(maxq)
      REAL    etcut,et_cut,rcut,r_cut,r
      REAL    p_pt(maxq),p_eta(maxq),p_phi(maxq)
      LOGICAL doevol,do_evol
C----------------------------------------------------------------------
C
      NJET=0
C
C          handle W's and Z's
C
      IEVT=IEVT+1
      IWZ=0
      NQS=NPRTNS
      IF(WZDK) NQS=NPRTNS-2
      DO 1 J=1,NPRTNS
        ID=IABS(IDS(J))
        IF(ID.GT.79) THEN
          IF(ID.EQ.90) JWTYP=4
          IF(IDS(J).EQ.80) JWTYP=2
          IF(IDS(J).EQ.-80) JWTYP=3
          IDENTW=IDS(J)
          DO 2 K=1,4
            QWJET(K)=PRTNS(K,J)
   2      CONTINUE
          QWJET(5)=SQRT(QWJET(4)**2-QWJET(1)**2-QWJET(2)**2-QWJET(3)**2)
          IWZ=J
        ENDIF
   1  CONTINUE
      DO 4 J=NQS+1,NPRTNS
        ID=IABS(IDS(J))
        NJET=NJET+1
        DO 3 K=1,4
          PJETS(K,NJET)=PRTNS(K,J)
    3   CONTINUE
        IDJETS(NJET)=IDS(J)
        PJETS(5,NJET)=AMASS(ID)
    4 CONTINUE
C          W,Z decays were not in input
      IF(IWZ.NE.0.AND.NJET.EQ.0) THEN
        NJET=2
        CALL ISWDKY
      ENDIF
C
C      fill with the other partons
C
      DO 5 K=1,4
        SUM(K)=0
   5  CONTINUE
      DO 11 J=1,NQS
        ID=IABS(IDS(J))
        IF(IWZ.NE.J.AND.ID.LT.11) THEN
          NJET=NJET+1
          IDJETS(NJET)=IDS(J)
          DO 12 K=1,4
            PJETS(K,NJET)=PRTNS(K,J)
  12      CONTINUE
          PJETS(5,NJET)=PRTNS(4,J)**2-PRTNS(1,J)**2-PRTNS(2,J)**2-
     $      PRTNS(3,J)**2
          IF ( PJETS(5,NJET).GT.0. ) THEN
            PJETS(5,NJET)=SQRT(PJETS(5,NJET))
          ELSE
            PJETS(4,NJET)=SQRT(PRTNS(4,J)**2-PJETS(5,NJET))
            PJETS(5,NJET)=0.
          ENDIF
        ENDIF
        DO 13 K=1,4
          SUM(K)=SUM(K)+PRTNS(K,J)
  13    CONTINUE
  11  CONTINUE
c
c        eta and phi of incoming partons
      if(doevol) then
        NP=NQS-1
        do i=1,np
          call ispeta(prtns(1,i),thq(i),phiq(i),etaq(i))
          ptq(i)=sqrt(prtns(1,i)**2+prtns(2,i)**2)
        enddo
C
C ... ORDER PARTONS IN PT
C
        DO  I = 1 , NP
          JIORD(I) = I
          P_PT(I)=PTQ(I)
        ENDDO
        CALL ISASRT(P_PT(1),NP,JIORD)
        DO I = 1 , NP
          P_PT(I)=PTQ(I)
          P_ETA(I)=ETAQ(I)
          P_PHI(I)=PHIQ(I)
          JDORD(I) = JIORD(NP-I+1)
        ENDDO
        DO I = 1 , NP
          PTQ(I)=P_PT(JDORD(I))
          ETAQ(I)=P_ETA(JDORD(I))
          PHIQ(I)=P_PHI(JDORD(I))
        ENDDO
      endif
C
C
  15  CONTINUE
      PBEAM(1)=(ECM-SUM(4)-SUM(3))/2.
      PBEAM(2)=(ECM-SUM(4)+SUM(3))/2.
      QSQ=SQRT(SUM(4)**2-SUM(3)**2-SUM(2)**2-SUM(1)**2)
      CALL RANFMT
      NPTCL=0
      IF(KEYS(3)) THEN
        STDDY=.FALSE.
        IF(NQS.EQ.1.OR.NJET.LT.3) STDDY=.TRUE.
      ENDIF
      CALL IPRTNS(NQS,PRTNS,IDQ)
      IF(.NOT.NOEVOL) THEN
        CALL EVOLVE
c
c            special check for VECBOS
        if(doevol) then
c       find parton jets
          call isa_pjets(rcut,etcut,npj,p_pt,p_phi,p_eta)
          if(npj.ge.np.and.p_pt(np).gt.ptq(np)) then
            r=sqrt((p_eta(np)-etaq(np))**2+(p_phi(np)-phiq(np))**2)
            if(r.gt.rcut) goto 15
          endif
        endif
c
        IF(.NOT.NOHADR) THEN
          CALL FRGMNT
          CALL MBIAS
        ENDIF
      ENDIF
      WT=WEIGHT
      SUMWT=SUMWT+WT
      SIGF=SUMWT
      NKINF=IEVT
      NEVENT=IEVT
  999 RETURN
      entry evol_cuts(r_cut,et_cut,do_evol)
      rcut=r_cut
      etcut=et_cut
      doevol=do_evol
      return
      END
