      FUNCTION DST_ANALYSIS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Sample subroutine for DST analysis. It loops through all 
C-     reconstructed particle and jet banks. 
C-
C-     As an example it reads an RCP file at first (DST.RCP)
C-     The constants read in from the RCP file can be modified
C-     unteractively using option 'User Dialog' which will
C-     call entry point DST_DIAL.
C-
C-     In this example bank information is accessed directly
C-     using pointers. Utility subroutines GTxxxx for bank
C-     xxxx exist for many of these banks and could be used instead.
C-     Use Eve Gold-E to read them if you want to know about them.
C-     GTxxxx must be used for microDST's
C-
C-     To get the documentation on any xxxx bank use Eve
C-     Command: ZEB xxxx
C-
C-       The DST only has banks described in D0$ZEB$DST
C-       and when apropriate in D0$ZEB$ISA (ISAJET banks)
C-       For an example of how to get the ISAJET banks
C-       see D0$ISAZEB$COMPACK:ISANAL.FOR
C-       If you need hit information in your analysis
C-       you must read STA files rather then DST.
C-
C-   Returned value  : true
C-
C-   ENTRY DST_DIAL : example of dialog for option 'User Dialog'
C-
C-   Created  26-JUL-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DST_ANALYSIS,DST_DIAL
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LJETS,LPMUO,LPELC,LPPHO,LPNUT,LPTAU,LZTRK
      INTEGER GZJETS,GZPMUO,GZPELC,GZPPHO,GZPNUT,GZPTAU
      INTEGER NJETS,NMUONS,NELEC,NPHOT
      INTEGER LJET2,LPELCMX,ICHOICE,IER
      REAL AN,EXMIS,EYMIS,ETA,PHI,TEMPLATE(5,4),RRMS
      REAL    MUON_ET,MUON_ETA,MUON_PHI,PHOT_ET,ETMIN
      REAL    ET,ELC_ET,ELC_ETA,ELC_PHI,P(4),JPAIR_MASS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA ICHOICE,ETMIN/1,15./
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7 
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
C----------------------------------------------------------------------
C
      DST_ANALYSIS=.TRUE.      ! set it to false to skip any additional
                               ! processing of current event
C
C       book histograms in directory DST
C
      IF(FIRST) THEN
        FIRST=.FALSE.
C
C        read RCP file and constants
C
        CALL INRCP('DST_RCP',IER)       
        IF(IER.EQ.0) THEN
          CALL EZPICK('DST_RCP')  
          CALL EZGET_i('JETS_ALGORITHM',ICHOICE,IER)
          CALL EZGET('JETS_MIN_ET',ETMIN,IER)
          CALL EZRSET
        ENDIF
C
C           book histograms
C        CALL HCDIR('//PAWC',' ')    ! go to top directory
        CALL HMDIR('DST','S')       ! create DST directory
        CALL HBOOK1(1,' Et of Electrons',50,0.,100.,0.)
        CALL HBOOK1(2,' Et Max. of Electrons',50,0.,100.,0.)
        CALL HBOOK1(10,' Highest Et Jet pair mass',50,0.,200.,0.)
        CALL HBOOK1(11,' Et 1st Jet',50,0.,100.,0.)
        CALL HBOOK1(12,' Et 2nd Jet',50,0.,100.,0.)
        CALL HBOOK1(13,' Et 3rd Jet',50,0.,100.,0.)
        CALL HBOOK1(14,' Et 4th Jet',50,0.,100.,0.)
        CALL HBOOK1(19,' Number of jets',40,0.5,20.5,0.)
        CALL HBOOK1(22,' Muon Et ',50,0.,100.,0.)
        CALL HBOOK1(23,' Max. muon Et ',50,0.,100.,0.)
        CALL HBOOK1(31,' Missing Et (not muon corrected)',50,0.,100.,0.)
        CALL HBOOK1(32,' Missing Et (muon corrected)',50,0.,100.,0.)
        CALL HBOOK1(41,' Photon Et ',50,0.,100.,0.)
        CALL HBOOK1(42,' Max. photon Et ',50,0.,100.,0.)
        CALL HBOOK1(121,' Et of Taus',50,0.,100.,0.)
        CALL HBOOK1(122,' R(rms) of Taus',50,0.,.5,0.)
        CALL HBOOK1(123,' Et of Taus + 2 jets',50,0.,100.,0.)
        CALL HBOOK2(3,' Eta vs. Phi for electrons (CUT)'
     &    ,60,-3.,3.,50,0.,6.28,0.)
        CALL HBOOK2(20,' Eta vs. Phi for muons',60,-3.,3.,50,0.,6.28,0.)
        CALL HBOOK2(21,' Eta vs. Phi for muons (CUT)'
     &    ,60,-3.,3.,50,0.,6.28,0.)
      ENDIF
C
      CALL HCDIR('//PAWC/DST',' ')  ! go to DST directory
C
C       electrons
C
      NELEC=0
      LPELC=GZPELC()
      ELC_ET=10.
      LPELCMX=0
C
      IF(LPELC.NE.0) THEN
C
C         loop through electron banks and pick maximum
        DO WHILE (LPELC.GT.0)
          NELEC=NELEC+1
          ET=Q(LPELC+7)
          CALL HFILL(1,ET,0.,1.)
          IF(ET.GT.ELC_ET) THEN
            ELC_ET=ET
            ELC_ETA=Q(LPELC+9)
            ELC_PHI=Q(LPELC+10)
            LPELCMX=LPELC
          ENDIF
          LZTRK=LQ(LPELC-3)        ! reference link to associated track
          LPELC=LQ(LPELC)          ! pointer to next electron
        ENDDO
C
        IF(LPELCMX.GT.0) THEN
          CALL HFILL(2,ELC_ET,0.,1.)
          CALL HFILL(3,ELC_ETA,ELC_PHI,1.)
        ENDIF
C
      ENDIF
C
C       jets
C
      IF(ICHOICE.EQ.1) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(ICHOICE.EQ.2) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(ICHOICE.EQ.3) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(ICHOICE.EQ.4) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
      LJETS=GZJETS()
C
C        loop over all jets to remove possible electron jet
      IF(LJETS.NE.0) THEN
        IF(LPELCMX.GT.0) THEN
          DO WHILE (LJETS.NE.0) 
            IF((ABS(Q(LJETS+9)-ELC_ETA).LT..1).AND.
     &        (ABS(Q(LJETS+8)-ELC_PHI).LT..1)) 
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
C
C         calculate jet pair mass of 2 highest Et jets
        LJET2=LQ(LJETS)
        IF(LJET2.GT.0) THEN
          CALL VADD(Q(LJETS+2),Q(LJET2+2),P,4)
          JPAIR_MASS=P(4)**2-P(1)**2-P(2)**2-P(3)**2
          IF(JPAIR_MASS.GT.0.) THEN
            JPAIR_MASS=SQRT(JPAIR_MASS)
            IF(Q(LJET2+6).GT.ETMIN) CALL HFILL(10,JPAIR_MASS,0.,1.)
          ENDIF
        ENDIF
C
C        loop over all jets
        NJETS=0
        DO WHILE (LJETS.GT.0) 
          ET=Q(LJETS+6)
          IF(ET.GT.ETMIN) THEN
            NJETS=NJETS+1
            IF(NJETS.LT.5) CALL HFILL(10+NJETS,ET,0.,1.)
          ENDIF
          LJETS=LQ(LJETS)  ! pointer to next jet
        ENDDO
        AN=NJETS
        CALL HFILL(19,AN,0.,1.)
C
      ENDIF
C
C        missing ET
C
      LPNUT=GZPNUT(2)     ! pick missing ET bank with ICD correction
      IF(LPNUT.GT.0) THEN
        ET=Q(LPNUT+7)
        CALL HFILL(31,ET,0.,1.)
      ENDIF
      LPNUT=GZPNUT(3)     ! pick missing ET bank with MUON correction
      IF(LPNUT.GT.0) THEN
        ET=Q(LPNUT+7)
        CALL HFILL(32,ET,0.,1.)
      ENDIF
C
C       muons
C
      NMUONS=0
      LPMUO=GZPMUO(0)
      MUON_ET=4.
C
      IF(LPMUO.NE.0) THEN
C
C         loop through muon banks 
C         drop low angle muons and pick maximum
        DO WHILE (LPMUO.GT.0)
          ET=Q(LPMUO+14)
          ETA=Q(LPMUO+16)
          PHI=Q(LPMUO+17)
          CALL HFILL(20,ETA,PHI,1.)
          IF ( ABS(ETA).GT.2.0 ) THEN
            CALL MZDROP(IXCOM,LPMUO,' ')
          ELSE
            NMUONS=NMUONS+1
            CALL HFILL(22,ET,0.,1.)
            IF(ET.GT.MUON_ET) THEN
              MUON_ET=ET
              MUON_ETA=ETA
              MUON_PHI=PHI
            ENDIF
          ENDIF
          LZTRK=LQ(LPMUO-4)        ! reference link to associated central track
          LPMUO=LQ(LPMUO)          ! pointer to next muon
        ENDDO
        IF(MUON_ET.GT.4.) THEN
          CALL HFILL(23,MUON_ET,0.,1.)
          CALL HFILL(21,MUON_ETA,MUON_PHI,1.)
        ENDIF
C
      ENDIF
C
C       photons
C
      NPHOT=0
      LPPHO=GZPPHO()
      PHOT_ET=10.
C
      IF(LPPHO.NE.0) THEN
C
C         loop through photon banks and pick maximum
        DO WHILE (LPPHO.GT.0)
          NPHOT=NPHOT+1
          ET=Q(LPPHO+7)
          CALL HFILL(41,ET,0.,1.)
          IF(ET.GT.PHOT_ET) THEN
            PHOT_ET=ET
          ENDIF
          LPPHO=LQ(LPPHO)          ! pointer to next photon
        ENDDO
        IF(PHOT_ET.GT.10.) CALL HFILL(42,PHOT_ET,0.,1.)
C
      ENDIF
C
C      loop over all tau candidates
C
      LPTAU=GZPTAU()
      DO WHILE (LPTAU.GT.0)
        ET=Q(LPTAU+7)
        PHI=Q(LPTAU+9)
        ETA=Q(LPTAU+10)
        RRMS=Q(LPTAU+11)
        CALL HFILL(121,ET,0.,1.)
        CALL HFILL(122,RRMS,0.,1.)
        IF(AN.GT.2) CALL HFILL(123,ET,0.,1.)
        LPTAU=LQ(LPTAU)
      ENDDO
      CALL RESET_CAPH
      GOTO 999
C
C
      ENTRY DST_DIAL
C
      DST_DIAL=.TRUE.
      ICHOICE=1
      CALL GETPAR1('Choose Cone jets (1,2,3) or NN jets (4) [1]>',
     &  'I',ICHOICE)
      CALL GETPAR(1,'ETMIN for jets [15.]>','R',ETMIN)
C
  999 RETURN
      END
