C----------------------------------------------------------------------
C-
C-   Common block for TOP_LIMIT program
C-   Created  22-JUN-1993   Richard Partridge
C-
C----------------------------------------------------------------------
C
      INTEGER MAX_CHAN                
      PARAMETER (MAX_CHAN=100)
C
      INTEGER TOP_MASS
      INTEGER LMSG                    
      INTEGER NCHAN                   
      INTEGER N(MAX_CHAN)             
      INTEGER NTOT                    
      INTEGER ITER                    
      INTEGER ICL                     
      INTEGER NGEN                    
C
      REAL BG(MAX_CHAN)               
      REAL DBG(MAX_CHAN)              
      REAL BR(MAX_CHAN)               
      REAL EFF(MAX_CHAN)              
      REAL DEFF(MAX_CHAN)             
      REAL LUM(MAX_CHAN)              
      REAL DLUM                       
      REAL CL                         
C
      REAL*8 FACTORIAL                
      REAL*8 GAMMA                    
      REAL*8 EPS                      
      REAL*8 SIGMA                    
      REAL*8 DSIGMA                   
      REAL*8 BETA                     
      REAL*8 BETA0                    
      REAL*8 DBETA                    
      REAL*8 BETA_MIN                 
      REAL*8 BETA_MAX                 
      REAL*8 ALPHA                    
      REAL*8 ALPHA0                   
      REAL*8 DALPHA                   
      REAL*8 ALPHA_MIN                
      REAL*8 ALPHA_MAX                
      REAL*8 SIG_MIN(0:1000)          
      REAL*8 SIG_MAX(0:1000)          
      REAL*8 SIG_INT(0:1000)          
C
      CHARACTER*10 NAMES(MAX_CHAN)    
C
      COMMON /TOP_LIMDATA/ LMSG,NCHAN,N,NTOT,ITER,ICL,NGEN,BG,DBG,BR,
     &  EFF,DEFF,LUM,DLUM,CL,FACTORIAL,GAMMA,EPS,SIGMA,DSIGMA,
     &  BETA,BETA0,DBETA,BETA_MIN,BETA_MAX,ALPHA,ALPHA0,DALPHA,
     &  ALPHA_MIN,ALPHA_MAX,SIG_MIN,SIG_MAX,SIG_INT
