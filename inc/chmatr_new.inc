C----------------------------------------------------------------------
C-
C-   Created   5-APR-1992   Rajendran Raja
C-   for use with the new H matrix package
C-
C----------------------------------------------------------------------
      INTEGER NET,NPH,NETLO,NPHLO,NETTOT,NPHTOT,NRAPM
C
      PARAMETER( NET = 1 )              
      PARAMETER( NPH = 1 )              
      PARAMETER( NETLO = -1 )           
      PARAMETER( NPHLO = -1 )           
      PARAMETER( NETTOT = 2*NET+1 )     
      PARAMETER( NPHTOT = 2*NPH+1 )     
C
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
C CHSQF,PROBF are the Chisuquared and probability for the full H matrix.
C
      PARAMETER( NRAPM = 37 )  
      COMMON /CHMATR_NEW/ETAC,PHIC,CHSQF,PROBF,PRED_CENTER(3),
     &  SHOWER_CENTER(3),XBAR3(3),DBAR3(3),WEIGHT_CUT,
     &  ZVPRED,EDEAD_PRED,ENER_CRACK_PRED,ENER_CRYO_PRED,
     &  ADD_CRYO,ADD_CRACK,ZETA_CUT(NRAPM),ET_MAX,
     &  RAPIDITY_MAP(NRAPM),
     &  USE_DEAD_ENERGY ,USE_POSITION_INFO,USE_ENERGY_RATIO,
     &  EVIS,EDEAD,ENER_CRYO,ENER_CRACK,Z_VERTEX_SPREAD,DELTA_PHI,
     &  DELTA_R,DELTA_Z,PHI_CENTROID,R_CENTROID,Z_CENTROID,
     &  NCACH,PCATE,NRCATE,NRCAEH,SIGN_ETA
C
      INTEGER ETAC,PHIC     
      REAL    CHSQF,PROBF  
      REAL    PRED_CENTER   
      REAL    SHOWER_CENTER 
      REAL    XBAR3         
      REAL    DBAR3         
      REAL    WEIGHT_CUT    
      REAL    ZVPRED        
      REAL    EDEAD_PRED    
      REAL    ENER_CRACK_PRED   
      REAL    ENER_CRYO_PRED    
      LOGICAL ADD_CRYO      
      LOGICAL ADD_CRACK     
      REAL    ZETA_CUT      
      REAL    ET_MAX        
      CHARACTER*32 RAPIDITY_MAP 
      LOGICAL USE_DEAD_ENERGY   
      LOGICAL USE_POSITION_INFO 
      LOGICAL USE_ENERGY_RATIO  
      REAL    EVIS              
      REAL    EDEAD             
      REAL    ENER_CRYO             
      REAL    ENER_CRACK            
      REAL    Z_VERTEX_SPREAD   
      REAL    DELTA_PHI         
      REAL    DELTA_R           
      REAL    DELTA_Z           
      REAL    PHI_CENTROID      
      REAL    R_CENTROID        
      REAL    Z_CENTROID        
      INTEGER NCACH,PCATE,NRCATE,NRCAEH 
      INTEGER SIGN_ETA          
