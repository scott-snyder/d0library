C----------------------------------------------------------------------
C-
C-   Created  23-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER MXFSR
      PARAMETER( MXFSR = 10 )
      COMMON /KINEQ/ RUNC,EVENTC,CONFIG,ICONFIG,IPHI,NCONFIG,ILAM,
     &  LEPTON_TYPE(2),LEPTON_TYPE_S(2),ETA_PHI_LEPTON(2,2),
     &  NPHI,NLAMBDA,idum,SOL(2),DO_ITERATE,DO_EXACT,
     &  LEPTON1(4),LEPTON2(4),JET1(4),JET2(4),JET3(4),
     &  LEPTON1_S(4),LEPTON2_S(4),
     &  JET1_S(4),JET2_S(4),JET3_S(4),
     &  PNUT(2),PNUT_S(2),PNUT1(2),PNUT2(2),W1(4,2),W2(4,2),
     &  WMASS,TOP1(5,2),TOP2(5,2),PHILO,PHIHI,DELPHI,PHI,
     &  LAMBDA_LO,LAMBDA_HI,DEL_LAMBDA,LAMBDA,
     &  DEL_LAMBDA_ITERATE,ITOT_EV,IACC_EV,ISOL_EV,
     &  IFSRA,IFSRT,TOP_FSR(4,MXFSR),ATOP_FSR(4,MXFSR)
C
      DOUBLE PRECISION  LEPTON1,LEPTON2,JET1,JET2,JET3,
     &  LEPTON1_S,LEPTON2_S,JET1_S,JET2_S,JET3_S,
     &  PNUT,PNUT_S,PNUT1,PNUT2,W1,W2,WMASS,TOP1,TOP2,
     &  PHILO,PHIHI,DELPHI,PHI,
     &  LAMBDA_LO,LAMBDA_HI,DEL_LAMBDA,LAMBDA,DEL_LAMBDA_ITERATE
C
C
      INTEGER LEPTON_TYPE
      INTEGER LEPTON_TYPE_S
      INTEGER IDUM   
C
      INTEGER ICONFIG,IPHI,NCONFIG,ILAM
      INTEGER NPHI,NLAMBDA,RUNC,EVENTC,CONFIG
      INTEGER ITOT_EV,IACC_EV,ISOL_EV
C
      LOGICAL SOL,DO_ITERATE,DO_EXACT
      REAL    ETA_PHI_LEPTON
      INTEGER IFSRA,IFSRT
      REAL    TOP_FSR,ATOP_FSR
C
C RUNC ,EVENTC = RUN/EVENT NUMBER 
C CONFIG = KINEMATIC CONFIGURATION NUMBER =1 MEANS OBSERVED QUANTITIES
C LEPTON1 = 4 VECTOR OF 1ST LEPTON (HIGHEST ET)
C LEPTON2 = 4 VECTOR OF 2ND LEPTON
C LEPTON_TYPE 1=ELECTRON, 2=MUON
C JET1,JET2,JET3 = 4 VECTORS OF 1ST THREE HIGHEST ET JETS
C PNUT = TOTAL MISSING ET VECTOR FOR EVENT
C LEPTON1_S IS SAVED VALUE OF LEPTON1 (IN GENERATE CONFIGS)
C W1(4,2) = 1ST W SOLUTION (CAN HAVE TWO SOLUTIONS)
C W2(4,2) = 2ND W SOLUTION
C SOL(1) = TRUE , IF 1ST W HAS SOLUTIONS..
C WMASS = Mass of W
C TOP1 = 4 VECTOR OF 1ST TOP AND MASS. 2 SOLUTIONS FOR 2 W SOLUTIONS
C TOP2 = 4 VECTOR OF SECOND TOP AND MASS. 2 SOLUTIONS FOR 2 W SOLUTIONS
C PHILO = LOWEST VALUE OF PHI FOR 1ST NEUTRINO VECTOR FOR SCAN
C PHIHI = HIGHEST VALUE FOR PHI FOR 1ST NEUTRINO VECTOR (IN DEGREES)
C DELPHI = INCREMENT IN PHI
C PHI = CURRENT VALUE OF PHI 
C NPHI = NUMBER OF PHI VECTORS
C LAMBDA_LO = LOWEST VALUE OF NEUTRINO ET ALONG THIS DIRECTION
C LAMBDA_HI = HIGHEST VALUE OF NEUTRINO ET
C DEL_LAMBDA = INCREMENT IN LAMBDA
C NLAMBDA =M NUMBER OF STEPS IN LAMBDA TO TAKE
C LAMBDA = CURRENT VALUE OF LAMBDA (NEUTRINO ET)
C DO_ITERATE .IF TRUE WILL ITERATE FOR SOLUTION ALONG PHI LINE
C DO_EXACT  . IF TRUE WILL SOLVE FOR SOLUTION EXACTLY FOR A GIVEN TOPMASS
C ETA_PHI_LEPTON = ETA AND PHI OF LEPTONS SELECTED. FOR BTAG
C ITOT_EV = TOTAL NUMBER OF EVENTS INTO TOP_MASS_ANAL
C IACC_EV = NUMBER OF EVENTS PASSED BY TOP_MASS_GET_EVENT
C ISOL_EV = TOTAL NUMBER OF EVENTS FOR WHICH A MASS SOLUTION RESULTS
C IFSRT = NUMBER OF FINAL STATE RADIATION GLUONS FROM TOP
C IFSRA = NUMBER OF FINAL STATE RADIATION GLUONS FROM ANTI-TOP
C TOP_FSR ,ATOP_FSR = GLUON 4 VECTORS FROM FINAL STATE RADIATION