C----------------------------------------------------------------------
C-
C-   Created  10-JAN-1994   Rajendran Raja
C-   Quantities to be fitted and measured.
C-
C----------------------------------------------------------------------
      INTEGER NMEAS
      PARAMETER( NMEAS = 17 )  
      COMMON /FIT_QUAN_2C/ REST(2),BLEP_JET(3),LEPTON(3),
     &  BHAD_JET(3),HAD_JET1(3),HAD_JET2(3),EMAT(NMEAS,NMEAS),
     &  HMAT(NMEAS,NMEAS), WORK(NMEAS,NMEAS),
     &  REST_F(2),BLEP_JET_F(3),LEPTON_F(3),
     &  BHAD_JET_F(3),HAD_JET1_F(3),HAD_JET2_F(3),
     &  STRFN(NMEAS),
     &  CHISQC,PROBC,FEDM,ERRDEF,NPARI,NPARX,ISTAT
C
      DOUBLE PRECISION    REST,BLEP_JET,LEPTON,BHAD_JET,
     &  HAD_JET1,HAD_JET2
      DOUBLE PRECISION EMAT,HMAT,WORK
      DOUBLE PRECISION    REST_F,BLEP_JET_F,LEPTON_F,BHAD_JET_F,
     &  HAD_JET1_F,HAD_JET2_F
      DOUBLE PRECISION    STRFN
      DOUBLE PRECISION    XMEAS(NMEAS), XPRED(NMEAS)
      EQUIVALENCE (XMEAS,REST) , (XPRED,REST_F)
      DOUBLE PRECISION    CHISQC,PROBC
      DOUBLE PRECISION    FEDM,ERRDEF
      INTEGER NPARI,NPARX,ISTAT
C
C This common block contains the quantities that are to be fitted
C derived from the measured quantities.
C REST = rest of the event 2 Vector. = -Kt of the ttbar system
C BLEP_JET = 3 vector of B jet associated with the Leptonic W.
C Quantities in these 3 vectors are Energy, Eta(psedo rapidity) and Phi.
C This makes for  particularly simple error matrices.
C LEPTON = 3 vector of Lepton (also as defined above)
C BHAD_JET = B jet 3 vector of vector associated with hadronic top.
C HAD_JET1 = 3 vector of higher Et jet resulting from  W decay
C HAD_JET2 = 3 vector of lower Et jet resulting from W decay.
C EMAT = error matrix for measurements
C HMAT = Inverse of EMAT
C WORK = Work area for inversion
C _F INDICATES CORRESPONDING FITTED QUANTITIES.
C REST_F = rest of the event 2 Vector. = -Kt of the ttbar system
C BLEP_JET_F = 3 vector of B jet associated with the Leptonic W.
C Quantities in these 3 vectors are Energy, Eta(pseudo rapidity) and Phi.
C This makes for  particularly simple error matrices.
C LEPTON_F = 3 vector of Lepton (also as defined above)
C BHAD_JET_F = B jet 3 vector of vector associated with hadronic top.
C HAD_JET1_F = 3 vector of higher Et jet resulting from  W decay
C HAD_JET2_F = 3 vector of lower Et jet resulting from W decay.
C STRFN(NMEAS) = Stetch functions of measurements. Should be Unit Gaussians
C CHISQC,PROBC = CHISQUARED AND PROBABILITY OF FIT.
C FEDM=estimated vertical distance from minimum
C ERRDEF=Value of Up parameter defining uncertainties
C NPARI=Number of currently variable parameters
C NPARX=Total number of parameters defined
C ISTAT = variable indicating how good the covariance matrix is
C       = 0 Not calculated at all
C       = 1 Diagonal approximation only, not accurate
C       = 2 Full matrix but forced positive definite
C       = 3 Full accurate covariance matrix
