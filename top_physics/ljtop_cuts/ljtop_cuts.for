      FUNCTION LJTOP_CUTS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      analysis using 3 algorithms for Top -> lepton + jets
C-      initialization controlled by LJTOP_CUTS.RCP parameters
C-      can be modified at run time via 'User Dialog' in D0USER
C-
C-    loops through lepton and jet banks
C-
C-    calls FIND_TOPS1, FIND_TOPS2 and FIND_TOPS3 to order
C-    jets according to algorithms described in those subroutines
C-
C-    calls LJTOP_CUT_NTUPLES to fill Ntuples
C-
C-   Returned value  : true
C-
C-   Created  26-JUL-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LJTOP_CUTS,LJTOP_CUT_FIN
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER LJETS,LPMUO,LPELC,LPNUT,IER
      INTEGER GZPMUO,GZJETS,GZPELC,GZPPHO,LPPHO,GZPNUT
      INTEGER ABSID,INDX(4),ICHOICE
      INTEGER GZISAE
      INTEGER NJETS,NMUONS,NELEC,NPHOT
      INTEGER IADD,IOK,OFSET,EUNIT
      INTEGER ISEL,I,NBS,US,USUNIT,IPTYPE,N1,RUN
      REAL    DEL_ET,TH,PNUT(5),LET(3),LISO(2),DIST_EL_CUT,MWTR
      REAL    AN,TYPEG,PJETS(5,20),DIST_EL,WTM,ETW,ETAW,PHIW
      REAL    MUON_ET,MUON_ETA,MUON_PHI,LEP_ET,CONE
      REAL    ET,ELC_ET,ELC_ETA,ELC_PHI,P(4),PLEP(5),ESUM(4)
      REAL    DELW,ETCUTB(2),ETCUTW,ETMIN,ETAJ,PHIJ,MISET,ETOUT
      REAL    ELEC_CUT,MUON_CUT,MISET_CUT,PHOT_ET,PHOT_ETA,PHOT_PHI
      REAL    PWLEP(5),PWHAD(7),PTOP1(5,2),PTOP2(5,2),JET_ETA_CUT
      REAL    WPZ2,THW,PHI(6),ETA(6),TEMPLATE(5,4),CORR_MISET
      EQUIVALENCE (LET(1),ELC_ET),(LET(2),MUON_ET),(LET(3),PHOT_ET)
      LOGICAL FIRST,OK,ALGO(3)
      LOGICAL DO_DUMPS,CORRJ
      DATA FIRST,DO_DUMPS,CORRJ/.TRUE.,.FALSE.,.TRUE./
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7 
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
C----------------------------------------------------------------------
C
      LJTOP_CUTS=.FALSE.      ! set it to false to skip any additional
C                                   ! processing of current event
C
      CALL DHDIR('LJTOP_CUTS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF(FIRST) THEN
        CALL LJTOP_GET_CUTS(ETCUTB,ETCUTW,ELEC_CUT,MUON_CUT,MISET_CUT,
     &    JET_ETA_CUT,DIST_EL_CUT)
        CALL LJTOP_GET_CUT_OPT(ALGO,CORR_MISET,CORRJ,DO_DUMPS,ICHOICE)
        FIRST=.FALSE.
        IF(ALGO(1)) CALL LJTOP_CUT_BOOK(1)
        IF(ALGO(2)) CALL LJTOP_CUT_BOOK(2)
        IF(ALGO(3)) CALL LJTOP_CUT_BOOK(3)
        CALL GSLINK('DST',N1)   ! pointer to lepton from W
      ENDIF
C
      IF(GZISAE().NE.0) CALL LJTOP_CUT_ISA()
      DIST_EL=20.
      ETMIN=AMIN1(ETCUTB(1),ETCUTB(2),ETCUTW)
      NMUONS=0
      NJETS=0
      NELEC=0
C
C       muons
C
      LISO(2)=10.
      LPMUO=GZPMUO(0)
      MUON_ET=MUON_CUT
C
      IF(LPMUO.NE.0) THEN
C
C         loop through muon banks 
C         drop low angle muons and pick maximum
        DO WHILE (LPMUO.GT.0)
          ET=Q(LPMUO+14)
          ETA(1)=Q(LPMUO+16)
          IF ( ABS(ETA(1)).GT.2.2 ) THEN
            CALL MZDROP(IXCOM,LPMUO,' ')
          ELSE
            IF(ET.GT.MUON_ET.AND.ET.LT.200.) THEN
              NMUONS=NMUONS+1
              MUON_ET=ET
              MUON_ETA=ETA(1)
              MUON_PHI=Q(LPMUO+17)
              LEP_ET=ET
              LSLINK(N1)=LPMUO
              CALL UCOPY(Q(LPMUO+10),PLEP,5)
              LISO(2)=Q(LPMUO+31)
            ENDIF
          ENDIF
          LPMUO=LQ(LPMUO)          ! pointer to next muon
        ENDDO
C
      ENDIF
      IF(NMUONS.EQ.0) MUON_ET=-1.
C
C       electrons
C
      LISO(1)=10.
      LPELC=GZPELC()
      ELC_ET=ELEC_CUT
C
      IF(LPELC.NE.0) THEN
C
C         loop through electron banks and pick maximum
        DO WHILE (LPELC.GT.0)
          ET=Q(LPELC+7)
          IF(ET.GT.ELC_ET.AND.Q(LPELC+22).LT.DIST_EL_CUT) THEN
            NELEC=NELEC+1
            DIST_EL=Q(LPELC+22)
            ELC_ET=ET
            LEP_ET=ET
            ELC_ETA=Q(LPELC+9)
            ELC_PHI=Q(LPELC+10)
            LSLINK(N1)=LPELC
            CALL UCOPY(Q(LPELC+3),PLEP,5)
            IF(Q(LPELC+15).GT.0) THEN
              LISO(1)=(Q(LPELC+16)-Q(LPELC+15))/Q(LPELC+15)
            ELSE
              LISO(1)=-1.
            ENDIF
          ENDIF
          LPELC=LQ(LPELC)          ! pointer to next electron
        ENDDO
C
      ENDIF
      IF(NELEC.EQ.0) ELC_ET=-1.
C
C       photons
C
      NPHOT=0
      LPPHO=GZPPHO()
      PHOT_ET=ELEC_CUT
C
      IF(LPPHO.NE.0.AND.DIST_EL_CUT.GT.10.) THEN
C
C         loop through photon banks and pick maximum
        DO WHILE (LPPHO.GT.0)
          ET=Q(LPPHO+7)
          IF(ET.GT.PHOT_ET) THEN
            NPHOT=NPHOT+1
            PHOT_ET=ET
            PHOT_ETA=Q(LPPHO+9)
            PHOT_PHI=Q(LPPHO+10)
            LSLINK(N1)=LPPHO
          ENDIF
          LPPHO=LQ(LPPHO)          ! pointer to next photon
        ENDDO
      ENDIF
      IF(NPHOT.EQ.0.) PHOT_ET=-1.
      IF((NELEC+NMUONS+NPHOT).EQ.0) GOTO 999 ! no lepton candidate
      LJTOP_CUTS=.TRUE.      
      IF(.NOT.(ALGO(1).OR.ALGO(2).OR.ALGO(3))) GOTO 999 ! no analysis
C
C        missing ET
C
      LPNUT=GZPNUT(2)     ! pick missing ET bank with ICD correction
      IF(LPNUT.GT.0) THEN
        MISET=Q(LPNUT+7)*CORR_MISET
        PNUT(1)=Q(LPNUT+3)*CORR_MISET
        PNUT(2)=Q(LPNUT+4)*CORR_MISET
      ENDIF
      LPNUT=GZPNUT(3)     ! pick missing ET bank with MUON correction
      IF(LPNUT.GT.0) THEN
        MISET=Q(LPNUT+7)*CORR_MISET
        PNUT(1)=Q(LPNUT+3)*CORR_MISET
        PNUT(2)=Q(LPNUT+4)*CORR_MISET
      ENDIF
      IF(MISET.LT.MISET_CUT) RETURN
      PNUT(4)=MISET
      IF((NELEC+NMUONS).EQ.0.AND.NPHOT.EQ.1) THEN 
        LPPHO=LSLINK(N1)
        CALL UCOPY(Q(LPPHO+3),PLEP,5)
        ELC_ETA=PHOT_ETA
        ELC_PHI=PHOT_PHI
        LEP_ET=PHOT_ET
        NELEC=1
        DIST_EL=20.
        IF(Q(LPPHO+15).GT.0) THEN
          LISO(1)=(Q(LPPHO+16)-Q(LPPHO+15))/Q(LPPHO+15)
        ELSE
          LISO(1)=-1.
        ENDIF
      ENDIF
C
C       jets
C
      IF(ICHOICE.EQ.1) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(ICHOICE.EQ.2) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(ICHOICE.EQ.3) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(ICHOICE.EQ.4) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
      CONE=TEMPLATE(3,ICHOICE)
      LJETS=GZJETS()
C
C        loop over all jets to remove possible electron jet
      IF(LJETS.NE.0) THEN
        IF(NELEC.GT.0) THEN
          DO WHILE (LJETS.NE.0) 
            ETAJ=Q(LJETS+9)
            PHIJ=Q(LJETS+8)
            IF((ABS(ETAJ-ELC_ETA).LT..1).AND.
     &        (ABS(PHIJ-ELC_PHI).LT..1)) 
     &        CALL MZDROP(IXCOM,LJETS,' ')
            LJETS=LQ(LJETS)  ! pointer to next jet
          ENDDO
        ENDIF
      ENDIF
C
      LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LJETS must be refetched
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS=GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS=GZJETS()
        DO WHILE (LJETS.GT.0) 
          ET=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          ETAJ=ABS(Q(LJETS+9))
          IF(ET.GT.ETMIN.AND.ETAJ.LT.JET_ETA_CUT) THEN
            NJETS=NJETS+1
            IF(CORRJ) THEN
              ET=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
              CALL JET_ET_MCCORR(ETAJ,ET,CONE,ETOUT)   ! correct jets
              DO I=1,5
                Q(LJETS+I+1)=Q(LJETS+I+1)*ETOUT/ET
              ENDDO
            ENDIF
            DO I=1,4
              PJETS(I,NJETS)=Q(LJETS+I+1)
            ENDDO
            PJETS(5,NJETS)=ET
            ETA(NJETS)=ETAJ
            PHI(NJETS)=Q(LJETS+8)
          ENDIF
          LJETS=LQ(LJETS)  ! pointer to next jet
        ENDDO
      ENDIF
C
C             now look for top candidates 
C
      IF(NJETS.GT.2) THEN
C
C            find first W-> leptons
        CALL FIND_WLNU(80.,PLEP,PNUT,PWLEP,WPZ2,OK)
        IF(.NOT.OK) GOTO 999
        MWTR=SQRT((PLEP(4)+PNUT(4))**2-(PLEP(1)+PNUT(1))**2-
     &    (PLEP(2)+PNUT(2))**2-PLEP(3)**2)
C
C
        PWLEP(5)=WPZ2
        IF(ALGO(1)) THEN
          CALL LJFIND_TOPS1(NJETS,PJETS,PWLEP,
     &      PTOP1,PTOP2,PWHAD,INDX,IOK)         ! 1ST algorithm
C
          IF(IOK.GT.0) CALL LJTOP_CUT_NTUPLES(1,PJETS,PWLEP,PTOP1,PTOP2,
     &      MISET,LET,LISO,DIST_EL,PWHAD,MWTR,INDX)
        ENDIF
C
C
        IF(ALGO(2)) THEN
          OK=.FALSE.
          IF(NJETS.GT.3) CALL LJFIND_TOPS2(PWLEP,PJETS,
     &      PTOP1,PTOP2,PWHAD,INDX,OK)           ! 2ND algorithm
C
          IF(OK) CALL LJTOP_CUT_NTUPLES(2,PJETS,PWLEP,PTOP1,PTOP2,
     &      MISET,LET,LISO,DIST_EL,PWHAD,MWTR,INDX)
        ENDIF
        IF(ALGO(3)) THEN
          CALL LJFIND_TOPS3(NJETS,PJETS,PWLEP,
     &      PTOP1,PTOP2,PWHAD,INDX,IOK)         ! 3RD algorithm
          IF(IOK.GT.0) CALL LJTOP_CUT_NTUPLES(3,PJETS,PWLEP,PTOP1,PTOP2,
     &      MISET,LET,LISO,DIST_EL,PWHAD,MWTR,INDX)
        ENDIF
        IF(DO_DUMPS) THEN
C                 define here any criteria for dumping an event
          CALL FLGSET('DUMP_EVENT',.TRUE.)
        ENDIF
      ENDIF
C
      CALL RESET_CAPH
      GOTO 999
C
C
      ENTRY LJTOP_CUT_FIN()
C      dummy, put here any final summary
  999 RETURN
      END
