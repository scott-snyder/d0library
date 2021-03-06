C----------------------------------------------------------------------
C-
C-   Created  23-JAN-1993   Rajendran Raja
C-   contains Kinematic quantities describing fit.
C-   Including the top mass, there are 15 quantities
C-
C----------------------------------------------------------------------
      INTEGER NFIT
      PARAMETER( NFIT = 15 )
      COMMON /KINE_FIT/ ELMASS,MUMASS,NUMASS,BMASS,
     &  WMASS,TOP_MASS,HADMASS,RWT,DWT,
     &  TOP_LEPTON(4),CTHETA_TL,PHI_TL,CTHETA_WL,PHI_WL,
     &  TOP_HADRON(4),CTHETA_TH,PHI_TH,CTHETA_WH,PHI_WH,
     &  TOP_LEPTON_F(4),CTHETA_TL_F,PHI_TL_F,CTHETA_WL_F,PHI_WL_F,
     &  TOP_HADRON_F(4),CTHETA_TH_F,PHI_TH_F,CTHETA_WH_F,PHI_WH_F,
     &  PAR_NAMES(NFIT),PAR_INIV(NFIT),
     &  PAR_STEPS(NFIT),PAR_LOLIM(NFIT),PAR_HILIM(NFIT),
     &  FIT_NAMES(NFIT),FIT_VAL(NFIT),FIT_ERR(NFIT),
     &  FIT_LOLIM(NFIT),FIT_HILIM(NFIT)
C
      DOUBLE PRECISION    ELMASS,MUMASS,NUMASS,HADMASS
      DOUBLE PRECISION    BMASS,WMASS,TOP_MASS
      DOUBLE PRECISION    RWT,DWT
      DOUBLE PRECISION    TOP_LEPTON,CTHETA_TL,PHI_TL,CTHETA_WL,PHI_WL
      DOUBLE PRECISION    TOP_HADRON,CTHETA_TH,PHI_TH,CTHETA_WH,PHI_WH
      DOUBLE PRECISION    TOP_MASS_F
      DOUBLE PRECISION    TOP_LEPTON_F,CTHETA_TL_F,PHI_TL_F,
     &  CTHETA_WL_F,PHI_WL_F
      DOUBLE PRECISION    TOP_HADRON_F,CTHETA_TH_F,PHI_TH_F,
     &  CTHETA_WH_F,PHI_WH_F
      CHARACTER*32 PAR_NAMES
      DOUBLE PRECISION    PAR_STEPS,PAR_INIV,PAR_LOLIM,PAR_HILIM
      CHARACTER*32 FIT_NAMES
      DOUBLE PRECISION    FIT_VAL,FIT_ERR,FIT_LOLIM,FIT_HILIM
C
C ELMASS = ELECTRON MASS
C MUMASS = MUON MASS
C NUMASS = NEUTRINO MASS
C BMASS    = MASS OF B QUARK
C WMASS    = Mass of W boson
C TOP_MASS = MASS OF TOP QUARK
C HADMASS = MASS OF HADRONIC JET
C RWT = 1/SIGMA DSIGMA/DLIPS WEIGHT 
C DWT= DALITZE GOLDSTEIN WEIGHT
C
C  Initial values of the model
C  TOP_LEPTON = 4 VECTOR OF TOP DECAYING SEMI-LEPTONICALLY
C  CTHETA_TL = COS THETA OF W IN TOP_LEPTON REST FRAME
C  PHI_TL   = PHI OF W IN TOP_LEPTON REST FRAME
C  CTHETA_WL = COS THETA OF LEPTON IN W RESTFRAME
C  PHI_WL = PHI OF LEPTON IN W RESTFRAME
C
C  TOP_HADRON = 4 VECTOR OF TO DECAYING HADRONICALLY
C  CTHETA_TH= COS THETA OF W IN TOP_HADRON RESTFRAME
C  PHI_TH = PHI OF W IN TOP_HADRON RESTFRAME
C  CTHETA_WH = COS THETA OF high et jet in W restframe
C  PHI_WH = Phi of High Et jet in W restframe
C
C  Quantities with _F appendix refer to current values of the model during
C  fit.
C
C  PAR_NAMES = PARAMETER DESCRIPTOR
C  PAR_INIV = PARAMETER INITIAL VALUES
C  PAR_LOLIM = PARAMETER LOWER LIMITS
C  PAR_HILIM = PARAMETER UPPER LIMITS
C
C parameters for fit
C Top_mass
C TOP_LEPTON(3) LEPONICALLY DECAYING TOP 3 VECTOR IN X,Y,Z CO-ORDS
C CTHETA_TL,PHI_TL = COS THETA AND PHI OF W IN TOP RESTFRAME
C CTHETA_WL,PHI_WL = COS THETA AND PHI OF LEPTON IN W RESTFRAME
C TOP_HADRON(3) HADRONICALLY DECAYING TOP 3VECTOR IN X Y Z CO-ORDS
C CTHETA_TH,PHI_TH = COS THETA AND PHI OF W IN TOP RESTFRAME
C CTHETA_WH,PHI_WH = COS THETA AND PHI OF LAB HIGH ET JET IN W RESTFRAME
C
