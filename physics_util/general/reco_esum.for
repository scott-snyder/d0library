      SUBROUTINE RECO_ESUM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Fill ESUM bank, summary of DST objects
C-
C-   Created  21-JAN-1992   Serban D. Protopopescu
C-   Updated   5-NOV-1992   Qizhong Li-Demarteau  introducing MXNTOT
C-                     and added check NTOT to avoid the overwriting
C-   Updated  14-DEC-1993   Serban Protopopescu
C-                      always drop existing ESUM bank
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
      INCLUDE 'D0$LINKS:IZCAPH.LINK/LIST'
      INTEGER MXNTOT
      PARAMETER( MXNTOT = 500 )
      REAL    ET(MXNTOT),ETA(MXNTOT),PHI(MXNTOT),ETA_DET(MXNTOT)
      INTEGER LVERH,GZVERH,LVERT,LPELC,LPPHO,LPTAU,LJETS,LPMUO
      INTEGER LPNUT,GZPPHO,GZPELC,GZPTAU,GZPMUO,GZJETS,GZPNUT,LESUM
      INTEGER LCAPH,GZCAPH,GZESUM
      INTEGER NOBJ(0:LAST_TYPE),ID_OBJ(MXNTOT),NTOT,IER,NFIX,NR,N
      INTEGER LDSUM,I,K,IFLAGS(MXNTOT),IDJ,IDJS(0:5),ICONE,IDJ_PREV
      INTEGER FLAGR,NFOUND(ID_ALL:LAST_TYPE)
      INTEGER GZPROC, PLPROC
      LOGICAL BITON(MXNTOT)
      REAL    DELTA_COS,DCOS(10)
      REAL    ETR,ETAR,ETA_DETR,PHIR,THETA,Z
      REAL    DCOS_FROM_ETA_PHI
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      LESUM=GZESUM('RECO')
      IF(LESUM.GT.0) THEN !  bank already exists
          CALL MZDROP(IXCOM,LESUM,' ')
      ENDIF
      IF(FIRST) THEN
        IDJS(0)=ID_JET
        IDJS(1)=ID_JET_1
        IDJS(2)=ID_JET_2
        IDJS(3)=ID_JET_3
        IDJS(4)=ID_JET_4
        IDJS(5)=ID_JET_5
        CALL EZPICK('RECO_ESUM_RCP')
        CALL EZGETA_i('DELTA_COS',0,0,0,N,IER)
        CALL EZGETA('DELTA_COS',1,N,1,DCOS,IER)
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
      NTOT=0
C
C       vertices
C
      LVERT=0
      LVERH=GZVERH()
      IF(LVERH.GT.0) LVERT=LQ(LVERH-IZVERT)
      NOBJ(ID_VERTEX)=0
      Z=0.
C
      IF(LVERT.NE.0) THEN
C
C         loop through vertices
        DO WHILE (LVERT.GT.0)
          NTOT=NTOT+1
          IF (NTOT .GT. MXNTOT) GOTO 998
          NOBJ(ID_VERTEX)=NOBJ(ID_VERTEX)+1
          ID_OBJ(NTOT)=ID_VERTEX
          IFLAGS(NTOT)=0
          ET(NTOT)=Q(LVERT+6)
          ETA_DET(NTOT)=Q(LVERT+3)
          ETA(NTOT)=Q(LVERT+4)
          PHI(NTOT)=Q(LVERT+5)
          IF(NTOT.EQ.1) Z=PHI(NTOT)
          LVERT=LQ(LVERT)          ! pointer to next vertex
        ENDDO
C
      ENDIF
C
C       photons
C
      LPPHO=GZPPHO()
      NOBJ(ID_PHOTON)=0
C
      IF(LPPHO.NE.0) THEN
C
C          sort banks so they are in increasing order of Et
        CALL ZSORT(IXCOM,LPPHO,7)
        LPPHO=GZPPHO()
        CALL ZTOPSY(IXCOM,LPPHO)
        LPPHO=GZPPHO()
C
C         loop through photon banks
        DO WHILE (LPPHO.GT.0)
          NTOT=NTOT+1
          IF (NTOT .GT. MXNTOT) GOTO 998
          NOBJ(ID_PHOTON)=NOBJ(ID_PHOTON)+1
          ID_OBJ(NTOT)=ID_PHOTON
          IFLAGS(NTOT)=0
          ET(NTOT)=Q(LPPHO+7)
          THETA=Q(LPPHO+8)
          CALL DET_ETA(Z,THETA,ETA_DET(NTOT))
          ETA(NTOT)=Q(LPPHO+9)
          PHI(NTOT)=Q(LPPHO+10)
          LPPHO=LQ(LPPHO)          ! pointer to next electron
        ENDDO
C
      ENDIF
C
C       electrons
C
      NOBJ(ID_ELECTRON)=0
      NOBJ(ID_ELECTRON)=0
      LPELC=GZPELC()
C
      IF(LPELC.NE.0) THEN
C
C          sort banks so they are in increasing order of Et
        CALL ZSORT(IXCOM,LPELC,7)
        LPELC=GZPELC()
        CALL ZTOPSY(IXCOM,LPELC)
        LPELC=GZPELC()
C
C         loop through electron banks
        DO WHILE (LPELC.GT.0)
          NTOT=NTOT+1
          IF (NTOT .GT. MXNTOT) GOTO 998
          NOBJ(ID_ELECTRON)=NOBJ(ID_ELECTRON)+1
          ID_OBJ(NTOT)=ID_ELECTRON
          IFLAGS(NTOT)=0
          ET(NTOT)=Q(LPELC+7)
          THETA=Q(LPELC+8)
          CALL DET_ETA(Z,THETA,ETA_DET(NTOT))
          ETA(NTOT)=Q(LPELC+9)
          PHI(NTOT)=Q(LPELC+10)
          LPELC=LQ(LPELC)          ! pointer to next electron
        ENDDO
C
      ENDIF
C
C       muons
C
      NOBJ(ID_MUON)=0
      LPMUO=GZPMUO(0)
C
      IF(LPMUO.NE.0) THEN
C
C          sort banks so they are in increasing order of Et
        CALL ZSORT(IXCOM,LPMUO,14)
        LPMUO=GZPMUO(0)
        CALL ZTOPSY(IXCOM,LPMUO)
        LPMUO=GZPMUO(0)
C
C         loop through muon banks
        DO WHILE (LPMUO.GT.0)
          NTOT=NTOT+1
          IF (NTOT .GT. MXNTOT) GOTO 998
          NOBJ(ID_MUON)=NOBJ(ID_MUON)+1
          ID_OBJ(NTOT)=ID_MUON
          IFLAGS(NTOT)=0
          ET(NTOT)=Q(LPMUO+14)
          THETA=Q(LPMUO+15)
          CALL DET_ETA(Z,THETA,ETA_DET(NTOT))
          ETA(NTOT)=Q(LPMUO+16)
          PHI(NTOT)=Q(LPMUO+17)
          LPMUO=LQ(LPMUO)          ! pointer to next muon
        ENDDO
C
      ENDIF
C
C       taus
C
      NOBJ(ID_TAU)=0
      LPTAU=GZPTAU()
C
      IF(LPTAU.NE.0) THEN
C
C          sort banks so they are in increasing order of Et
        CALL ZSORT(IXCOM,LPTAU,7)
        LPTAU=GZPTAU()
        CALL ZTOPSY(IXCOM,LPTAU)
        LPTAU=GZPTAU()
C
C         loop through tau banks
        DO WHILE (LPTAU.GT.0)
          NTOT=NTOT+1
          IF (NTOT .GT. MXNTOT) GOTO 998
          NOBJ(ID_TAU)=NOBJ(ID_TAU)+1
          ID_OBJ(NTOT)=ID_TAU
          IFLAGS(NTOT)=0
          ET(NTOT)=Q(LPTAU+7)
          THETA=Q(LPTAU+8)
          CALL DET_ETA(Z,THETA,ETA_DET(NTOT))
          ETA(NTOT)=Q(LPTAU+10)
          PHI(NTOT)=Q(LPTAU+9)
          LPTAU=LQ(LPTAU)          ! pointer to next tau
        ENDDO
C
      ENDIF
C
C       jets
C
      DO I=0,5
        IDJ=IDJS(I)
        NOBJ(IDJ)=0
      ENDDO
      IDJ_PREV=-1000
C      LCAPH=GZCAPH()
      PLPROC = GZPROC()
      LCAPH = LQ(PLPROC-IZCAPH)
      DO WHILE (LCAPH.GT.0)             ! loop through all sets of jets
        IF (IQ(LCAPH+4) .LE. 1) GOTO 101
        IDJ=IDJS(3)
        ICONE=NINT(Q(LCAPH+6)*10.)
        IF(ICONE.EQ.7) IDJ=IDJS(0)
        IF(ICONE.EQ.5) IDJ=IDJS(1)
        IF(ICONE.EQ.3) IDJ=IDJS(2)
        IF(IDJ.EQ.IDJ_PREV) IDJ=IDJS(4)  ! protect against non-standard use
        IDJ_PREV=IDJ
C
        LJETS=LQ(LCAPH-2)
        IF(LJETS.NE.0) THEN
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LJETS must be refetched
          CALL ZSORT(IXCOM,LJETS,6)
          LJETS=LQ(LCAPH-2)
          CALL ZTOPSY(IXCOM,LJETS)
          LJETS=LQ(LCAPH-2)
C
C         loop through JETS banks
          DO WHILE (LJETS.GT.0)
            NTOT=NTOT+1
            IF (NTOT .GT. MXNTOT) GOTO 998
            NOBJ(IDJ)=NOBJ(IDJ)+1
            ID_OBJ(NTOT)=IDJ
            IFLAGS(NTOT)=ICONE
            ET(NTOT)=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
            THETA=Q(LJETS+7)
            CALL DET_ETA(Z,THETA,ETA_DET(NTOT))
            ETA(NTOT)=Q(LJETS+9)
            PHI(NTOT)=Q(LJETS+8)
            LJETS=LQ(LJETS)          ! pointer to next jet
          ENDDO
C
        ENDIF
C
  101   LCAPH=LQ(LCAPH)
      ENDDO
C
C       missing ET and ETSUM
 
C
      NOBJ(ID_ETMISS)=0
      NOBJ(ID_ETSUM)=0
      LPNUT=GZPNUT(0)
C
      IF(LPNUT.NE.0) THEN
C
C         loop through PNUT banks
        DO WHILE (LPNUT.GT.0)
          NTOT=NTOT+1
          IF (NTOT .GT. MXNTOT) GOTO 998
          NOBJ(ID_ETMISS)=NOBJ(ID_ETMISS)+1
          ID_OBJ(NTOT)=ID_ETMISS
          IFLAGS(NTOT)=0
          ET(NTOT)=Q(LPNUT+7)
          ETA_DET(NTOT)=Q(LPNUT+8)
          ETA(NTOT)=Q(LPNUT+9)
          PHI(NTOT)=Q(LPNUT+10)
          LPNUT=LQ(LPNUT)          ! pointer to next muon
        ENDDO
C
        LPNUT=GZPNUT(2)            ! get ETSUM without muon added
        IF(LPNUT.GT.0) THEN
          NTOT=NTOT+1
          IF (NTOT .GT. MXNTOT) GOTO 998
          NOBJ(ID_ETSUM)=NOBJ(ID_ETSUM)+1
          ID_OBJ(NTOT)=ID_ETSUM
          IFLAGS(NTOT)=0
          ET(NTOT)=Q(LPNUT+14)
          ETA_DET(NTOT)=-99.
          ETA(NTOT)=-99.
          PHI(NTOT)=-99.
        ENDIF
C
      ENDIF
      GOTO 102
C
  998 CALL ERRMSG('RECO_ESUM','RECO_ESUM',
     &    'number of objects exceeded MXNTOT','W')
C
C       book and fill ESUM
C
  102 CALL BKESUM('RECO',NTOT,LESUM)
      IQ(LESUM+4)=NTOT
      NFIX=IQ(LESUM+2)
      NR=IQ(LESUM+3)
      LDSUM=LESUM+NFIX
      DO I=0,LAST_TYPE
        IQ(LESUM+5+I)=NOBJ(I)
      ENDDO
      DO K=1,NTOT
        IQ(LDSUM+JESUM_ID)=ID_OBJ(K)
        IQ(LDSUM+JESUM_FLAG)=IFLAGS(K)
        Q(LDSUM+JESUM_PT)=ET(K)
        Q(LDSUM+JESUM_ETA_DET)=ETA_DET(K)
        Q(LDSUM+JESUM_ETA)=ETA(K)
        Q(LDSUM+JESUM_PHI)=PHI(K)
        LDSUM=LDSUM+NR
      ENDDO
C
C             find matches with FILT objects
C
      CALL GTESUM_COUNTS ('FILT',NFOUND,IER)
      IF(NFOUND(ID_ALL).GT.0) THEN
C
C       loop over all RECO objects
        DO K =  1, NTOT
          BITON(K)=.FALSE.
          IDJ=ID_OBJ(K)
          IF(IDJ.GE.1.AND.IDJ.LE.10.AND.IDJ.NE.7) THEN
C
C ****  Loop over objects in FILT bank
            DO I = 1,NFOUND(IDJ)
              CALL GTESUM('FILT',IDJ,I,ETR,ETAR,ETA_DETR,PHIR,FLAGR,IER)
C            calculate separation
              IF (IDJ.EQ.6) THEN
                DELTA_COS = DCOS_FROM_ETA_PHI(0.,PHI(K),0.,PHIR)
              ELSE
                DELTA_COS = DCOS_FROM_ETA_PHI(ETA_DET(K),PHI(K),
     &                                        ETA_DETR,PHIR)
              ENDIF
              IF ( DELTA_COS.GE.DCOS(IDJ)) BITON(K)=.TRUE.
            ENDDO
C
          ENDIF
        ENDDO
C
      ENDIF
C
C       fill RECO bits
C
      CALL PACK_RECO_BITS(NTOT,ID_OBJ,ET,BITON)
  999 RETURN
      END
