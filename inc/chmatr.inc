C----------------------------------------------------------------------
C-
C-   Created   1-JUN-1989   Rajendran Raja
C-   Updated  13-AUG-1989   Rajendran Raja
C-   Updated  28-AUG-1989   Rajendran Raja   .Got rid of eta,phi,lyr indexing.
C-
C----------------------------------------------------------------------
      INTEGER NET,NPH,NLYRH,NDIMH,NDIML,NDIMP,NDIMVL,NDIMVF,NDIMPR
      INTEGER NDIMVFR
      INTEGER NETLO,NPHLO,NETTOT,NPHTOT,NPOSN,IPOSN,IPREDF,IPREDL
C
      PARAMETER( NET = 1 )              
      PARAMETER( NPH = 1 )              
      PARAMETER( NETLO = -1 )           
      PARAMETER( NPHLO = -1 )           
      PARAMETER( NETTOT = 2*NET+1 )     
      PARAMETER( NPHTOT = 2*NPH+1 )     
      PARAMETER( NLYRH = 10 )           
      PARAMETER( NPOSN = 3 )            
      PARAMETER( NDIMH = NETTOT*NPHTOT*NLYRH+NPOSN )
      PARAMETER( NDIML = NLYRH-3 )        
      PARAMETER( NDIMP = 2 )            
      PARAMETER( NDIMPR = NETTOT*NPHTOT*NDIMP+NPOSN )  
                                        
      PARAMETER( NDIMVF = NLYRH-NDIMP ) 
      PARAMETER( NDIMVFR = NETTOT*NPHTOT*NDIMVF )
                                        
      PARAMETER( NDIMVL = NDIML-NDIMP ) 
      PARAMETER( IPOSN = NETTOT*NPHTOT*NLYRH+1 )        
      PARAMETER( IPREDF = NDIMVFR+1 )      
                                        
      PARAMETER( IPREDL = NDIMVL+1)     
                                        
C
C EMAT is the error matrix.
C HMAT is the full H- matrix.
C HMAT_VIS is the H- Matrix of the visible layers.
C AVR contains the average quantities.
C QUAN contains the quantities for the current event
C EMATRL,HMATRL,HMATRL_VIS,AVERL,QUANTL are corresponding quantities for the
C Longitudinal Error matrix,H matrix.
C NEVMTR is the total number of events contributing to HMATR.
C BEAMEN is the energy at which the H matrix was evaluated.
C ETAC,PHIC are the Eta phi indices of the tower with highest
C Energy. This maps to 0,0 in the Hmatrix space.
C
C The layers are as follows. 1st 7 layers = EM layers. LAYER 8 = FH
C LAYER 9 is Crack energy. Layer 10 = rest of dead material energy.
C These make 90 elements (9 eta phi for every layer).
C Elements 91,92,93 are the position co-ordinates of the impact of the
C electron track wrt the Center tower EM3.
C
C The depths for the Longitudinal Matrix are as follows.
C Depth 1-5 are the EM1-4 and 1st FH. Depth 6 is the Crack
C Depth 7 rest of dead material
C
C HMAT_INV is the inverse of the H sub matrix for full matrix prediction.
C PRED are the predicted energies for FH, crack and cryostat.
C DELPRED are the deviation of the predicted energy from the average
C energy.
C
C HMATRL_INV is the inverse of the longitudinal H sub-matrix of layers
C to predict.
C PREDL are the predicted energies in the FH, Crack and Cryostat
C DELPREDL is the deviation of the predicted energy from the average.
C
C
C HM_PHILIM(2) are the low and high Phi limits for evaluating H matrix.
C
C CHSQL,PROBL are the Chisuquared and probability for the Longitudinal H matrix.
C CHSQF,PROBF are the Chisuquared and probability for the Longitudinal H matrix.
C
      COMMON /CHMATR/EMAT(NDIMH,NDIMH),HMAT(NDIMH,NDIMH),
     &  HMAT_VIS(NDIMVFR,NDIMVFR),AVR(NDIMH),
     &  QUAN(NDIMH),HMAT_INV(NDIMPR,NDIMPR),PRED(NDIMPR),
     &  DELPRED(NDIMPR),
     &  EMATRL(NDIML,NDIML),HMATRL(NDIML,NDIML),
     &  HMATRL_VIS(NDIMVL,NDIMVL),AVERL(NDIML),QUANTL(NDIML),
     &  HMATRL_INV(NDIMP,NDIMP),PREDL(NDIMP),DELPREDL(NDIMP),
     &  NEVMTR,BEAMEN,ETAC,PHIC,HM_PHILIM,
     &  CHSQL,PROBL,CHSQF,PROBF
C
      DOUBLE PRECISION EMAT,HMAT,HMAT_VIS,AVR,QUAN,HMAT_INV
      DOUBLE PRECISION EMATRL,HMATRL,HMATRL_VIS,AVERL,QUANTL,HMATRL_INV
      DOUBLE PRECISION    PRED,DELPRED
      DOUBLE PRECISION    PREDL,DELPREDL
C
      INTEGER NEVMTR,ETAC,PHIC
      REAL    BEAMEN,HM_PHILIM(2)
      REAL    CHSQL,PROBL,CHSQF,PROBF
