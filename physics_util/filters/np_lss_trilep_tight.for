      FUNCTION NP_LSS_TRILEP_TIGHT()

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select tri- lepton candidates for
C-                         SUSY analysis. (Wino/Zino)
C-
C-
C-               Defines leptons as follows:
C-
C-                  electrons: PELC, Pt ge 5, ISOLATION AND H-MATRIX CUTS
C-
C-                  muons    : PMUO, pt ge 5, SOME QUALITY AND COSMIC REJECTION.
C-
C-
C-               SELECTION is:
C-
C-                    1) SUM (electron + muon) ge 3
C-
C-
C-   Inputs  :
C-   Outputs : Function is True if event passes selection.
C-   Controls:
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INCLUDE 'D0$LINKS:IZJTSH.LINK'
C
      LOGICAL GOOD_MUON1,LCOSMIC
      LOGICAL FIRST, cal_MIP
      LOGICAL LGOOD3,LGOOD2MU,LGOOD2
      LOGICAL NP_LSS_TRILEP_TIGHT
      LOGICAL Oct_xing
C
      INTEGER IER
      INTEGER I, J, K, L, IUNIT, IERR
      INTEGER NMUON,LPMUO,GZPMUO,NELEC,LPELC,GZPELC,NPHO,LPPHO,GZPPHO
      INTEGER NGMUO
      INTEGER LPNUT2,GZPNUT,LPNUT3,LHMTE,LHMTP
      INTEGER NGOODELEC,NGOODPHO
      INTEGER JBIT,COSMIC
      INTEGER IFW4
C
      REAL P_GMUO(5,20)
      REAL E_PT_CUT,MU_PT_CUT,G_PT_CUT,EM_ISO_CUT
      REAL MU_COL_CUT,MET_CUT,EM_HCHSQ_CUT,HCHSQ
      REAL X_ISO,ETMISS,ETMISS3,PTXX,ANG
      REAL CAL, Pt, Eta, Phi, Tzero
      REAL MTC_FRACT_CUT, MTC_HFRACT_CUT, MU_ETA_CUT
      REAL MU_TOF_CUT,FRACT,HFRACT
C
      DATA FIRST/.TRUE./

C----------------------------------------------------------------------

      NP_LSS_TRILEP_TIGHT=.FALSE.
C
      IF ( FIRST ) THEN
        FIRST=.FALSE.
C
        CALL INRCP ('NP_LSS_TIGHT_2_RCP', IER)
        IF (IER.EQ.0) THEN
          CALL EZPICK ('NP_LSS_TIGHT_2_RCP')
          CALL EZGET ('E_PT_CUT', E_PT_CUT, IER)
          CALL EZGET ('G_PT_CUT', G_PT_CUT, IER)
          CALL EZGET ('MU_PT_CUT', MU_PT_CUT, IER)
          CALL EZGET ('MU_ETA_CUT',MU_ETA_CUT,IER)
          CALL EZGET ('MU_COL_CUT', MU_COL_CUT, IER)
          CALL EZGET ('MU_TOF_CUT',MU_TOF_CUT,IER)
          CALL EZGET ('EM_ISO_CUT', EM_ISO_CUT, IER)
          CALL EZGET ('EM_HCHSQ_CUT', EM_HCHSQ_CUT, IER)
          call EZGET ('MTC_FRACT_CUT',MTC_FRACT_CUT,IER)
          call EZGET ('MTC_HFRACT_CUT',MTC_HFRACT_CUT,IER)
          CALL EZGET ('MET_CUT', MET_CUT, IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG ('No NP_LSS_TIGHT_2_RCP', 'NP_LSS_TRILEP_TIGHT',
     &        'Could not find NP_LSS_TIGHT_2_RCP', 'F')
        ENDIF                           ! if ier .eq. 0
C

      ENDIF                             ! if first

 
      NMUON =0
      NGMUO=0
      LPMUO=GZPMUO(0)
      DO WHILE(LPMUO.NE.0)
        NMUON=NMUON+1                     ! NUMBER OF MUONS
        Cal_MIP = .FALSE.
        Oct_xing = .FALSE.
        GOOD_MUON1 = .FALSE.
        

C  Our version of the filter cuts - taken directly from PMUO:

        IFW4     = IQ(LPMUO+9)    !    IFW4 quality flag
C        D_theta  = Q(LPMUO+39)    !    CD trk match
C        D_phi    = Q(LPMUO+38)    !    CD trk match
C        CAL      = Q(LPMUO+34)    !    Calorimeter E (cells hit + 2NN)
        Pt       = Q(LPMUO+14)    !    muon Pt
        Eta      = Q(LPMUO+16)    !    muon eta
        Phi      = Q(LPMUO+17)    !    muon phi
        Tzero    = Q(LPMUO+24)    !    floating T-zero offset
        fract    = Q(LPMUO+93)
        hfract    = Q(LPMUO+94)

C        IF(IQ(LPMUO+6).GE.1) THEN   !   At least 1 CD trk in road
C          IF( D_theta/Conv .LE. 30. .AND. D_phi/Conv .LE. 25. )
C     >       CD_Match = .TRUE.
C        ENDIF

        IF( JBIT(IQ(LPMUO+44),9).GT.0 ) Oct_xing = .TRUE.
 
        IF(FRACT.GE.MTC_FRACT_CUT.AND.HFRACT.GE.MTC_HFRACT_CUT ) 
     & Cal_MIP = .TRUE.

        IF( IFW4 .LE. 1 .AND. Cal_MIP .AND. .NOT. Oct_xing  
     >      .AND. Pt .GE. MU_PT_CUT .AND. ABS(eta) .LE. MU_ETA_CUT
     >      .AND. Tzero .LE. MU_TOF_CUT ) GOOD_MUON1 = .TRUE.


        IF ( GOOD_MUON1)  THEN
          NGMUO = NGMUO + 1
          IF ( NGMUO .GT. 20 )NGMUO=20
          P_GMUO(1,NGMUO) = Q(LPMUO+10)
          P_GMUO(2,NGMUO) = Q(LPMUO+11)
          P_GMUO(3,NGMUO) = Q(LPMUO+12)
          P_GMUO(4,NGMUO) = Q(LPMUO+13)
          P_GMUO(5,NGMUO) = Q(LPMUO+24)
        ENDIF

        LPMUO = LQ(LPMUO)

      ENDDO
C
C... CHECK FOR BACK-TO-BACK MUONS:
C
      COSMIC=0
C
      IF(NGMUO.GT.1) THEN
        DO I=1,NGMUO-1
          DO J=I+1,NGMUO
            ANG=(P_GMUO(1,I)*P_GMUO(1,J)+P_GMUO(2,I)*P_GMUO(2,J)+
     >        P_GMUO(3,I)*P_GMUO(3,J))/(P_GMUO(4,I)*P_GMUO(4,J))
            IF(ANG.LT.-1.)ANG=-1.
            IF(ANG.GT.1.)ANG=1.
            IF(ACOS(ANG).GT.MU_COL_CUT) THEN
             IF( P_GMUO(5,I) .GT. 100. 
     >          .AND. P_GMUO(5,J) .GT. 100. ) COSMIC = COSMIC + 1
            ENDIF   
          ENDDO
        ENDDO
      ENDIF

C
      NGMUO=NGMUO-COSMIC
C
C...  ELECTRONS

      NELEC=0
      NGOODELEC=0
      LPELC=GZPELC()
      DO WHILE(LPELC.NE.0)
        NELEC=NELEC+1

        PTXX=Q(LPELC+7)

        IF(Q(LPELC+17).NE.0) THEN
          X_ISO=(Q(LPELC+16)-Q(LPELC+17))/Q(LPELC+17)
        ELSE
          X_ISO=-1.
        ENDIF

        LHMTE=LQ(LPELC-1)
        HCHSQ=Q(LHMTE+7) ! H-MATRIX CHISQ

        IF(PTXX.GT.E_PT_CUT.AND.
     &     X_ISO.LE.EM_ISO_CUT.AND.        ! CHANGED 4/12/93
     &     HCHSQ.LE.EM_HCHSQ_CUT) THEN

          NGOODELEC=NGOODELEC+1

        ENDIF

        LPELC = LQ(LPELC)

      ENDDO


C...  PHOTONS

C      NPHO=0
C      NGOODPHO=0
C      LPPHO = GZPPHO()
C      DO WHILE(LPPHO.NE.0)
C        NPHO=NPHO+1
C        PTXX=Q(LPPHO+7)

CCC        IF(Q(LPPHO+16).NE.0.) THEN
CCC          X_ISO=Q(LPPHO+6)/Q(LPPHO+16)
CCC        ELSE
CCC          X_ISO=1.
CCC        ENDIF

C        IF(Q(LPPHO+17).NE.0) THEN
C          X_ISO=(Q(LPPHO+16)-Q(LPPHO+17))/Q(LPPHO+17)
C        ELSE
C          X_ISO=-1.
C        ENDIF

C        LHMTP=LQ(LPPHO-1)
C        HCHSQ=Q(LHMTP+7)

C        IF(PTXX.GT.G_PT_CUT.AND.
C     &     X_ISO.LE.EM_ISO_CUT.AND.
C     &     HCHSQ.LE.EM_HCHSQ_CUT) THEN

C          NGOODPHO=NGOODPHO+1

C        ENDIF

C        LPPHO = LQ(LPPHO)

C      ENDDO

      LGOOD3 = (NGOODELEC+NGMUO).GE.3
C
C      LGOOD2MU = NGMUO.GE.2
C      LGOOD2 = (NGOODELEC+NGMUO+NGOODPHO).GE.2 .AND.
C     &  (ETMISS.GE.MET_CUT.OR.ETMISS3.GE.MET_CUT)
C
      NP_LSS_TRILEP_TIGHT=(LGOOD3) 
C
  999 RETURN
      END
