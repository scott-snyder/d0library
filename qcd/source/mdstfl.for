      SUBROUTINE MDSTFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills MDST (micro DST) bank
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Original Idea:  1989   Geoff Forden
C-   Created  25-MAR-1992   Andrew J. Milder
C-   Updated  30-APR-1993   Andrew J. Milder  Add GLOB,JTCS- Version 2
C-   Updated  13-JUL-1993   Rich Astur Add PELC/PPHO words - version 3
C-   Updated  27-SEP-1993   Add new GLOB words - version 3
C-   Updated  17-NOV-1993   R. Astur Add extra PTAU words - version 4
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N_AUX_PELC_WORDS          ! Extra PELC/PPHO words for version 3
      INTEGER N_AUX_PPHO_WORDS, NWORDS, N_AUX_PTAU_WORDS, N_AUX_PTAU
      INTEGER II
      REAL AUX_PTAU_WORDS(6)
      PARAMETER( N_AUX_PELC_WORDS = 21 )
      PARAMETER( N_AUX_PPHO_WORDS = 8)
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
      INCLUDE 'D0$LINKS:IZPTAU.LINK'
      INCLUDE 'D0$LINKS:IZPNUT.LINK'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$LINKS:IZMDST.LINK'
      INCLUDE 'D0$LINKS:IZANLS.LINK'
      INCLUDE 'D0$LINKS:IZHMTE.LINK'
      INCLUDE 'D0$LINKS:IZHMTP.LINK'
      INCLUDE 'D0$LINKS:IZJTCS.LINK'
      INCLUDE 'D0$LINKS:IZGLOB.LINK'
      INTEGER PACKWD,GZCAEP,GZCAEH,GZJETS,LPARH,GZCATD,LCATD,MDST_LCATD
      INTEGER INDXX(5),NPSUMS(9),GZHSTR,GZPELC,GZPPHO,GZPNUT
      INTEGER GZPARH,GZPMUO,GZPROC,GZCAPH,LCAPH,NUMJT,LJETS,IJET,IVERS
      INTEGER IESHAR,I,IER,LPMUO,LPELC,LPTAU,LPPHO,NV,NUM_WORDS,LZMDST
      INTEGER K,JET_START,NUM_JET,MU_START,NUM_MU,NUM_NUT,NUT_START
      INTEGER NUM_ELC,ELC_START,NUM_TAU,TAU_START,NUM_PHO,PHO_START
      INTEGER NUM_CAPH,CAPH_START,TYPE,LPROC,LEN_PNUT,LPNUT
      INTEGER LEN_CAPH,LEN_JETS,LEN_PMUO,LEN_PELC,LEN_PPHO,LEN_PTAU
      INTEGER LHMTE,LHMTP,GLOB_START,NUM_GLOB,LEN_GLOB,LGLOB,LJTCS
      INTEGER LEN_JTCS,NUM_JTCS,JTCS_START,CAPH_VERSION,IG
      REAL E(7),SIG(3),SIGMU(5),TEMPLATE(20),PHIWID,ETAWID,ET_EM_FRAC
      REAL ZV(10),DZ(10),ET,THETA,ETA,PHI,CHIS
      REAL    MDST_VEC(15000)
      INTEGER IMDST_VEC(15000)
      EQUIVALENCE (MDST_VEC(1),IMDST_VEC(1))
C
      INTEGER HEADER_LENGTH
      PARAMETER( HEADER_LENGTH = 27)
C
C----------------------------------------------------------------------
C
C  Transfer CAPH and JETS information
C
      LPROC = GZPROC()
      LCAPH = LQ(LPROC-IZCAPH)
      CAPH_START = 2
      LEN_CAPH = IQ(LCAPH-1) + CAPH_START
      NUM_CAPH = 0
      K = 0
      NUM_JET = 0
C
C   STORE ALL JET TYPES
C
      DO WHILE (LCAPH.GT.0)
        TYPE = IQ(LCAPH+4)
        NUMJT = IQ(LCAPH+3)
        IF (NUMJT.GT.0) THEN
          LJETS = LQ(LCAPH-IZJETS)
          LEN_JETS = IQ(LJETS-1)
          NUM_CAPH = NUM_CAPH + 1
          JET_START = K + LEN_CAPH + 1
          MDST_VEC(K+1) = FLOAT(LEN_JETS)
          MDST_VEC(K+2) = FLOAT(JET_START+HEADER_LENGTH)
          MDST_VEC(K+3) = FLOAT(IQ(LCAPH+1))
          MDST_VEC(K+4) = FLOAT(IQ(LCAPH+2))
          MDST_VEC(K+5) = FLOAT(IQ(LCAPH+3))
          MDST_VEC(K+6) = FLOAT(IQ(LCAPH+4))
          MDST_VEC(K+7) = FLOAT(IQ(LCAPH+5))
          CALL UCOPY(Q(LCAPH+6),MDST_VEC(K+8),LEN_CAPH-7)
          K = K + LEN_CAPH
C
C  Transfer jet information
C
          DO IJET=1,NUMJT
            CALL UCOPY(Q(LJETS+1),MDST_VEC(K+1),LEN_JETS)
            MDST_VEC(K+1) = FLOAT(IQ(LJETS+1))
            MDST_VEC(K+15) = FLOAT(IQ(LJETS+15))
            MDST_VEC(K+16) = FLOAT(IQ(LJETS+16))
            MDST_VEC(K+20) = FLOAT(IQ(LJETS+20))
            MDST_VEC(K+21) = FLOAT(IQ(LJETS+21))
            IVERS = IQ(LJETS+1)
            IF (IVERS.EQ.1) THEN
              CALL ERRMSG('MDST-JETS','MDSTFL',
     &          'OLD JETS FORMAT-NO JTSH INFO STORED','W')
            ENDIF
            K = K + LEN_JETS
            LJETS = LQ(LJETS)
          ENDDO
        ENDIF
        LCAPH = LQ(LCAPH)
      ENDDO
C
C  Transfer JTCS info (if exists)
C
      LEN_JTCS = 0
      NUM_JTCS = 0
      JTCS_START = 0
      LCAPH = LQ(LPROC-IZCAPH)
      DO WHILE (LCAPH.GT.0)
        CAPH_VERSION = IQ(LCAPH+1)
        LJTCS = LQ(LCAPH-IZJTCS)
        IF (CAPH_VERSION.GT.3.AND.LJTCS.GT.0) THEN
          NUM_JTCS = 1
          LEN_JTCS = IQ(LJTCS-1)
          MDST_VEC(K+1) = Q(LCAPH+6)
          K = K + 1
          JTCS_START = K+1
          CALL UCOPY(Q(LJTCS+1),MDST_VEC(K+1),LEN_JTCS)
          MDST_VEC(K+1) = FLOAT(IQ(LJTCS+1))
          MDST_VEC(K+2) = FLOAT(IQ(LJTCS+2))
          MDST_VEC(K+3) = FLOAT(IQ(LJTCS+3))
          K = K + LEN_JTCS
        ENDIF
        LCAPH = LQ(LCAPH)
      ENDDO
C
C  Transfer muon information
C
      MU_START=0
      NUM_MU=0
      I=0
      LPMUO = 0
      LPARH=GZPARH()
      IF ( LPARH.GT.0 ) THEN
        LPMUO=LQ(LPARH-IZPMUO)
      ELSE
        CALL ERRMSG('MDST-PARH','MDSTFL',
     &     'MISSING PARH BANK-NO PARTICLE INFO','W')
      ENDIF
      IF (LPMUO.GT.0) THEN
        MU_START = K+1
        LEN_PMUO = IQ(LPMUO-1)
      ENDIF
      DO WHILE (LPMUO.GT.0)
        NUM_MU=NUM_MU+1
        CALL UCOPY(Q(LPMUO+10),MDST_VEC(K+10),LEN_PMUO-9)
        MDST_VEC(K+1) = FLOAT(IQ(LPMUO+1))
        MDST_VEC(K+2) = FLOAT(IQ(LPMUO+2))
        MDST_VEC(K+3) = FLOAT(IQ(LPMUO+3))
        MDST_VEC(K+4) = FLOAT(IQ(LPMUO+4))
        MDST_VEC(K+5) = FLOAT(IQ(LPMUO+5))
        MDST_VEC(K+6) = FLOAT(IQ(LPMUO+6))
        MDST_VEC(K+7) = FLOAT(IQ(LPMUO+7))
        MDST_VEC(K+8) = FLOAT(IQ(LPMUO+8))
        MDST_VEC(K+9) = FLOAT(IQ(LPMUO+9))
        MDST_VEC(K+44) = FLOAT(IQ(LPMUO+44))
        K = K + LEN_PMUO
        LPMUO=LQ(LPMUO)
      ENDDO
C
C  Transfer neutrino (missing energy) information
C
      NUM_NUT=0
      NUT_START=0
      LPNUT = 0
      IF (LPARH.GT.0) THEN
        LPNUT = LQ(LPARH-IZPNUT)
      ENDIF
      IF (LPNUT.GT.0) THEN
        NUT_START = K+1
        LEN_PNUT = IQ(LPNUT-1)
        CALL GTPNUT_TOTAL(NUM_NUT,IER)
        IF (IER.EQ.0) THEN
          DO I= 1, NUM_NUT
            LPNUT = GZPNUT(I)
            CALL UCOPY(Q(LPNUT+1),MDST_VEC(K+1),LEN_PNUT)
            MDST_VEC(K+1) = FLOAT(IQ(LPNUT+1))
            MDST_VEC(K+2) = FLOAT(IQ(LPNUT+2))
            K = K + LEN_PNUT
          ENDDO
        ENDIF
      ENDIF
C
C  Transfer certified electron information
C
      NUM_ELC=0
      ELC_START = 0
      LPELC = 0
      IF (LPARH.GT.0) THEN
        LPELC = LQ(LPARH-IZPELC)
      ENDIF
      IF (LPELC.GT.0) THEN
        ELC_START = K + 1
        LEN_PELC = IQ(LPELC-1) + 1 + N_AUX_PELC_WORDS
      ENDIF
      DO WHILE ( LPELC.GT.0 )
        NUM_ELC = NUM_ELC + 1
        LHMTE = LQ(LPELC-IZHMTE)
        IF (LHMTE.GT.0) THEN
          CHIS = Q(LHMTE+7)
        ELSE
          CHIS = -100.
        ENDIF
        CALL UCOPY(Q(LPELC+1),MDST_VEC(K+1),LEN_PELC-1)
        MDST_VEC(K+1) = FLOAT(IQ(LPELC+1))
        MDST_VEC(K+2) = FLOAT(IQ(LPELC+2))
        MDST_VEC(K+LEN_PELC-N_AUX_PELC_WORDS) = CHIS
C
C: Add auxiliary words
C
        CALL QCD_GET_AUX_PELC_WORDS( 1, LPELC, N_AUX_PELC_WORDS,
     &    MDST_VEC( K + LEN_PELC + 1 - N_AUX_PELC_WORDS ), NWORDS )
        K = K + LEN_PELC
        LPELC = LQ(LPELC)
      ENDDO
C
C  Transfer Tau information
C
      NUM_TAU = 0
      TAU_START = 0
      LPTAU = 0
      IF (LPARH.GT.0) THEN
        LPTAU = LQ(LPARH-IZPTAU)
      ENDIF
      IF (LPTAU.GT.0) THEN
        TAU_START = K+1
        N_AUX_PTAU_WORDS = 6
        CALL PTAU_TRK( LPTAU, N_AUX_PTAU_WORDS, AUX_PTAU_WORDS )
        LEN_PTAU = IQ(LPTAU-1) + N_AUX_PTAU_WORDS
      ENDIF
      DO WHILE ( LPTAU.GT.0 )
        NUM_TAU = NUM_TAU + 1
        CALL UCOPY(Q(LPTAU+1),MDST_VEC(K+1),LEN_PTAU)
        MDST_VEC(K+1) = FLOAT(IQ(LPTAU+1))
        MDST_VEC(K+2) = FLOAT(IQ(LPTAU+2))
C
C: Add auxiliary words
C
        N_AUX_PTAU = N_AUX_PTAU_WORDS
        CALL PTAU_TRK( LPTAU, N_AUX_PTAU, AUX_PTAU_WORDS )
        DO II = 1, N_AUX_PTAU_WORDS
          MDST_VEC( K + LEN_PTAU + II - N_AUX_PTAU_WORDS ) =
     &      AUX_PTAU_WORDS( II )
        ENDDO
        K = K + LEN_PTAU
        LPTAU = LQ(LPTAU)
      ENDDO
C
C  Transfer certified photon information
C
      NUM_PHO = 0
      PHO_START = 0
      LPPHO = 0
      IF (LPARH.GT.0) THEN
        LPPHO = LQ(LPARH-IZPPHO)
      ENDIF
      IF (LPPHO.GT.0) THEN
        PHO_START = K+1
        LEN_PPHO = IQ(LPPHO - 1) + 1 + N_AUX_PPHO_WORDS
      ENDIF
      DO WHILE ( LPPHO.GT.0 )
        NUM_PHO = NUM_PHO + 1
        LHMTP = LQ(LPPHO-IZHMTP)
        IF (LHMTP.GT.0) THEN
          CHIS = Q(LHMTP+7)
        ELSE
          CHIS = -100.
        ENDIF
        CALL UCOPY(Q(LPPHO+1),MDST_VEC(K+1),LEN_PPHO-1)
        MDST_VEC(K+1) = FLOAT(IQ(LPPHO+1))
        MDST_VEC(K+2) = FLOAT(IQ(LPPHO+2))
        MDST_VEC(K+LEN_PPHO-N_AUX_PPHO_WORDS) = CHIS
C
C: Add auxiliary words
C
        CALL QCD_GET_AUX_PELC_WORDS( 2, LPPHO, N_AUX_PPHO_WORDS,
     &    MDST_VEC( K + LEN_PPHO + 1 - N_AUX_PPHO_WORDS ), NWORDS )
        K = K + LEN_PPHO
        LPPHO = LQ(LPPHO)
      ENDDO
C
C   Transfer GLOB bank info
C
      LEN_GLOB = 0
      NUM_GLOB = 0
      GLOB_START = 0
      LGLOB = LQ(LPROC-IZGLOB)
      IF (LGLOB.GT.0) THEN
        GLOB_START = K+1
        NUM_GLOB = 1
        LEN_GLOB = IQ(LGLOB-1)
        CALL UCOPY(Q(LGLOB+1),MDST_VEC(K+1),LEN_GLOB)
        MDST_VEC(K+1) = FLOAT(IQ(LGLOB+1))
        MDST_VEC(K+2) = FLOAT(IQ(LGLOB+2))
        MDST_VEC(K+3) = FLOAT(IQ(LGLOB+3))
        MDST_VEC(K+4) = FLOAT(IQ(LGLOB+4))
C
C: Version 3 of GLOB has extra integer words (18-28)
C
        IF(IQ(LGLOB+1).GE.3) THEN
          DO IG=1,11
            MDST_VEC(K+17+IG) = FLOAT(IQ(LGLOB+17+IG))
          ENDDO
        ENDIF
C
        K = K + LEN_GLOB
      ENDIF
C
      NUM_WORDS = HEADER_LENGTH + K
      CALL BKMDST(LZMDST,NUM_WORDS)
C      IQ(LZMDST+1) = 1                  ! BANK VERSION NUMBER
C     IQ(LZMDST+1) = 2      ! VERSION 2 - JTCS,GLOB INCLUDED
      IQ(LZMDST+1) = 3      ! VERSION 3 - Extra PELC/PPHO words added
C
      IQ(LZMDST+2) = LEN_CAPH           ! CAPH REPETITION NUMBER
      IQ(LZMDST+3) = NUM_CAPH           ! NUMBER OF CAPH BANKS
      IQ(LZMDST+4) = CAPH_START+HEADER_LENGTH        ! START OF CAPH INFO
C
      IQ(LZMDST+5) = LEN_PMUO           ! MUON REPETION NUMBER
      IQ(LZMDST+6) = NUM_MU             ! # OF MUONS
      IQ(LZMDST+7) = MU_START+HEADER_LENGTH          ! START OF MUON INFO
C
      IQ(LZMDST+8) = LEN_PNUT           ! PNUT REPETITION NUMBER
      IQ(LZMDST+9) = NUM_NUT            ! NUMBER OF PNUT BANKS
      IQ(LZMDST+10) = NUT_START+HEADER_LENGTH        ! START OF PNUT INFO
C
      IQ(LZMDST+11) = LEN_PELC          ! ELECTRON REP. NUMBER
      IQ(LZMDST+12) = NUM_ELC           ! NUMBER OF ELECTRONS
      IQ(LZMDST+13) = ELC_START+HEADER_LENGTH        ! START OF ELECTRON INFO
C
      IQ(LZMDST+14) = LEN_PTAU          ! TAU REP. NUMBER
      IQ(LZMDST+15) = NUM_TAU           ! NUMBER OF TAUS
      IQ(LZMDST+16) = TAU_START+HEADER_LENGTH        ! START OF TAU INFO
C
      IQ(LZMDST+17) = LEN_PPHO          ! PHOTON REP. NUMBER
      IQ(LZMDST+18) = NUM_PHO           ! NUMBER OF PHOTONS
      IQ(LZMDST+19) = PHO_START+HEADER_LENGTH       ! START OF PHOTON INFO
C
      IQ(LZMDST+20) = LEN_GLOB
      IQ(LZMDST+21) = NUM_GLOB
      IQ(LZMDST+22) = GLOB_START+HEADER_LENGTH
C
      IQ(LZMDST+23) = LEN_JTCS
      IQ(LZMDST+24) = NUM_JTCS
      IQ(LZMDST+25) = JTCS_START+HEADER_LENGTH
C
C  FILL MDST BANK
C
      CALL UCOPY(MDST_VEC(1),Q(LZMDST+HEADER_LENGTH+1),K)
      IF ( K .GT. 15000 ) THEN
        CALL ERRMSG('Too many words','MDSTFL','Too many MDST words','W')
      ENDIF
C
  999 RETURN
      END
