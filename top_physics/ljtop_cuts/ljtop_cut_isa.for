      SUBROUTINE LJTOP_CUT_ISA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      make Ntuples for 2 algorithm top searches
C-      using ISAJET information only
C-
C-   Created  26-JUL-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER IER,LPJET,GZPJET,LPJPT
      INTEGER ABSID,INDX(4)
      INTEGER GZISAJ,GZPJHD,LPJHD
      INTEGER LISAJ,LISAQ,GZISAQ,ID,GZISAE,LISV1,GZISV1,LISP1
      INTEGER NJETS,NMUONS,NELEC
      INTEGER NTELEC,NTMUON,IOK,NTTAU,ICHOICE
      INTEGER ISEL,I,NBS,US,USUNIT,IPTYPE,N1,N2,RUN
      INTEGER N3,N4,N5,N6,LTOP,L,NQS
      REAL    ETCUTB(2),ETCUTW,ELEC_CUT,MUON_CUT
      REAL    MISET_CUT,JET_ETA_CUT,DIST_EL_CUT,CORR_MISET
      LOGICAL ALGO(3),DO_DUMPS,CORRJ
      REAL    ETAE(2),ETAM(2),ETATAU(2),PHIE(2),PHIM(2),PHITAU(2)
      REAL    ETE(2),ETM(2),ETTAU(2),DEL_ET,TH,PNUT(5),MWTR
      REAL    AN,PJETS(5,20),DIST_EL,WTM,ETW,ETAW,PHIW,ETJ,ETAJ
      REAL    MUON_ET,MUON_ETA,MUON_PHI,LEP_ET,APTYPE
      REAL    ET,ELC_ET,ELC_ETA,ELC_PHI,P(4),PLEP(4),ESUM(4)
      REAL    LET(3),LISO(2)
      EQUIVALENCE (LET(1),ELC_ET),(LET(2),MUON_ET)
      REAL    ETMIN,MISET
      REAL    PWLEP(5),PWHAD(7),PTOP1(5,2),PTOP2(5,2),TAU_CUT
      REAL    WPZ2,THW,PHI(6),ETA(6)
      LOGICAL FIRST,OK
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL LJTOP_GET_CUTS(ETCUTB,ETCUTW,ELEC_CUT,MUON_CUT,MISET_CUT,
     &    JET_ETA_CUT,DIST_EL_CUT)
        CALL LJTOP_GET_CUT_OPT(ALGO,CORR_MISET,CORRJ,DO_DUMPS,ICHOICE)
        ETMIN=AMIN1(ETCUTB(1),ETCUTB(2),ETCUTW)
        IF(ALGO(1)) CALL LJTOP_CUT_BOOK(11)
        IF(ALGO(2)) CALL LJTOP_CUT_BOOK(12)
        IF(ALGO(3)) CALL LJTOP_CUT_BOOK(13)
      ENDIF
C
C            remake PJET banks
C      LPJHD=GZPJHD()
C      IF(LPJHD.NE.0) CALL MZDROP(IXCOM,LPJHD,'L')
C      CALL PJETFL
C
C        make Ntuples from ISAJET information
C
      LISO(1)=0
      LISO(2)=0
      DIST_EL=0
      LET(1)=0.
      LET(2)=0.
      LET(3)=0.
      MISET=0
      NTELEC=0
      NTMUON=0
C         partons
      LISAQ=GZISAQ()
      DO WHILE (LISAQ.GT.0)    ! Loop for leptons
        ID=IABS(IQ(LISAQ+1))
C
        IF ( ID.EQ.12 ) THEN   ! electrons
          ET=SQRT(Q(LISAQ+2)**2+Q(LISAQ+3)**2)
          IF(ET.GE.ELEC_CUT) THEN
            NTELEC=NTELEC+1
            ETE(NTELEC)=ET
            ETAE(NTELEC)=Q(LISAQ+9)
            PHIE(NTELEC)=Q(LISAQ+7)
            CALL UCOPY(Q(LISAQ+2),PLEP,4)
            LEP_ET=ETE(NTELEC)
            ELC_ET=LEP_ET
            LISAJ=LQ(LISAQ-1)
          ENDIF
C
        ELSEIF (ID.EQ.14) THEN  ! muons
          ET=SQRT(Q(LISAQ+2)**2+Q(LISAQ+3)**2)
          IF(ET.GT.MUON_CUT) THEN
            NTMUON=NTMUON+1
            ETM(NTMUON)=ET
            ETAM(NTMUON)=Q(LISAQ+9)
            PHIM(NTMUON)=Q(LISAQ+7)
            ETAE(NTMUON)=Q(LISAQ+9)
            PHIE(NTMUON)=Q(LISAQ+7)
            CALL UCOPY(Q(LISAQ+2),PLEP,4)
            LEP_ET=ETM(NTMUON)
            MUON_ET=LEP_ET
            LISAJ=LQ(LISAQ-1)
          ENDIF
C
        ENDIF
        LISAQ=LQ(LISAQ)
      ENDDO
      IF(NTELEC+NTMUON.NE.1) RETURN
C
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LPJET must be refetched
      LPJET=GZPJET()
      CALL ZSORT(IXCOM,LPJET,2)
      LPJET=GZPJET()
      CALL ZTOPSY(IXCOM,LPJET)
C
C        loop over all jets to remove possible electron jet
      NJETS=0
      LPJET=GZPJET()
      IF(LPJET.NE.0) THEN
        DO WHILE (LPJET.NE.0.AND.NJETS.LT.6)
          ETJ=SQRT(Q(LPJET+3)**2+Q(LPJET+4)**2)
          ETAJ=Q(LPJET+10)
          IF(ETJ.GT.ETMIN.AND.ABS(ETAJ).LT.2.0) THEN
            NJETS=NJETS+1
            CALL UCOPY(Q(LPJET+3),PJETS(1,NJETS),4)
            PJETS(5,NJETS)=ETJ
            ETA(NJETS)=ETAJ
            PHI(NJETS)=Q(LPJET+8)
c            IF(NTELEC.GT.0) THEN
            IF(ABS(ETAE(1)-ETA(NJETS)).LT..05.AND.
     &        ABS(PHIE(1)-PHI(NJETS)).LT..05) THEN
              CALL MZDROP(IXCOM,LPJET,' ')
              NJETS=NJETS-1
c              ENDIF
            ENDIF
          ENDIF
          LPJET=LQ(LPJET)  ! pointer to next jet
        ENDDO
      ENDIF
C
C             now look for top candidates (using PJET banks)
C
      IF(NJETS.GT.2) THEN
C
C            find first W-> leptons
        CALL ISA_NUS_SUM(PNUT,OK)
        CALL FIND_WLNU(80.,PLEP,PNUT,PWLEP,WPZ2,OK)
        IF(.NOT.OK) GOTO 999
        MISET=SQRT(PNUT(1)**2+PNUT(2)**2)
        PNUT(4)=MISET
        MWTR=SQRT((PLEP(4)+PNUT(4))**2-(PLEP(1)+PNUT(1))**2-
     &    (PLEP(2)+PNUT(2))**2)
C
        IF(ALGO(1)) THEN
          CALL LJFIND_TOPS1(NJETS,PJETS,PWLEP,
     &      PTOP1,PTOP2,PWHAD,INDX,IOK)         ! 1ST algorithm
C
          IF(IOK.GT.0) CALL LJTOP_CUT_NTUPLES(11,PJETS,PWLEP,PTOP1,
     &      PTOP2,MISET,LET,LISO,DIST_EL,PWHAD,MWTR,INDX)
        ENDIF
C
C
        IF(ALGO(2)) THEN
          OK=.FALSE.
          IF(NJETS.GT.3) CALL LJFIND_TOPS2(PWLEP,PJETS,
     &      PTOP1,PTOP2,PWHAD,INDX,OK)           ! 2ND algorithm
C
          IF(OK) CALL LJTOP_CUT_NTUPLES(12,PJETS,PWLEP,PTOP1,PTOP2,
     &      MISET,LET,LISO,DIST_EL,PWHAD,MWTR,INDX)
        ENDIF
        IF(ALGO(3)) THEN
          CALL LJFIND_TOPS3(NJETS,PJETS,PWLEP,
     &      PTOP1,PTOP2,PWHAD,INDX,IOK)         ! 3rd algorithm
C
          IF(IOK.GT.0) CALL LJTOP_CUT_NTUPLES(13,PJETS,PWLEP,PTOP1,
     &      PTOP2,MISET,LET,LISO,DIST_EL,PWHAD,MWTR,INDX)
        ENDIF
      ENDIF
      GOTO 999
  999 RETURN
      END
