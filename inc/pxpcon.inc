C
C  Include file for common block PXPCON - Muon Trigger Pixel and A-Phi
C     counters, and MDT tubes.
C
C
C  ***** PIXEL (FORWARD) COUNTER INFORMATION
C
C  Volumes names for pixel counters
      INTEGER MLYV,MABC,MNS
      PARAMETER (MLYV=4)                 
                                         
      PARAMETER (MABC=3)                 
      PARAMETER (MNS=2)                  
      CHARACTER*4 NAMPXL(MLYV,MABC,MNS)  
      CHARACTER*4 NAMPXG(MLYV,MABC,MNS)  
C
C  Geometrical parameters for pixel counters (assumes north-south
C  symmetry).
      REAL ZPIXL(MABC)     
                           
      REAL XYMIN(MABC)     
      REAL XYMAX(4,MABC)   
      INTEGER NPHII(MABC)  
      INTEGER NPHIO(MABC)  
      REAL DRFIRS(MABC)    
      INTEGER NRBOU,NTRBOU
      PARAMETER (NRBOU=15)      
      PARAMETER (NTRBOU=10)     
      REAL RBOUND(NRBOU,NTRBOU,MABC) 
      INTEGER NRPIXELS(NTRBOU)  
C
C
C  ***** MDT (FORWARD) TUBE INFORMATION
C
C  Volumes names for MDT tubes
      INTEGER MLYVIA,MABCIA,MNSIA
      PARAMETER (MLYVIA=4)               
      PARAMETER (MABCIA=3)               
      PARAMETER (MNSIA=2)                
      CHARACTER*4 NAMIAL(MLYVIA,MABCIA,MNSIA)  
      CHARACTER*4 NAMIAG(MLYVIA,MABCIA,MNSIA)  
C
C  Geometrical parameters for pixel counters (assumes north-south
C  symmetry).
      REAL ZIAROC(MABCIA)  
      REAL XIAMIN(MABCIA)  
      REAL XIAMAX(4,MABCIA)
                           
      INTEGER IADEPS       
      REAL DWIATU          
      REAL DWIAWA          
      REAL DIASTA(4)       
      REAL DIAPOS(3)       
      REAL DIAGAP          
      REAL YFLOOR          
C
C
C  *****  A-PHI (BARREL) COUNTER INFORMATION
C
      INTEGER IAPHVERS                   
C                                        
C                                        
C
C  Volumes names for A-phi (barrel) counters
      INTEGER MPHLMX
      PARAMETER (MPHLMX=5)               
      CHARACTER*4 NAMPHS(MPHLMX)         
C
      REAL XYPHI(MPHLMX)                 
      REAL ZAPHI(MPHLMX)                 
      INTEGER NPHIS(MPHLMX)              
      INTEGER NZEES(MPHLMX)              
C
      REAL XWDINR                        
      REAL XWDOTR                        
      INTEGER NCINR                      
C
      INTEGER NCHOLE
      PARAMETER (NCHOLE=12)
      REAL DXHOLE(NCHOLE)                
      REAL DZHOLE(NCHOLE)                
      REAL XPHOLE(NCHOLE)                
      REAL YPHOLE(NCHOLE)                
      REAL ZPHOLE(NCHOLE)                
C
      INTEGER NAPHXEDG
      PARAMETER (NAPHXEDG=10)            
      REAL APHXEDG(NAPHXEDG)             
C
C  *****  C-LAYER COUNTER INFORMATION
C
C  Volumes names for C-LAYER counters
      INTEGER MCLLMX
      PARAMETER (MCLLMX=9)               
      CHARACTER*4 NAMCLY                 
C
      REAL XYDISC(MCLLMX)                
      REAL ZDISC(MCLLMX)                 
      REAL DLENC(MCLLMX)                 
      REAL DWIDC(MCLLMX)                 
      REAL XYPOFC(MCLLMX)                
      REAL ATHC                          
      INTEGER NZSEGC(MCLLMX)             
      INTEGER NXYSEGC(MCLLMX)            
C 
C  The common block
      INTEGER IDEBPXP
      COMMON/PXPCON/
     &  NAMPXL,NAMPXG,
     &    ZPIXL,XYMIN,XYMAX,NPHII,NPHIO,DRFIRS,RBOUND,NRPIXELS,
     &  NAMIAL,NAMIAG,
     &    ZIAROC,XIAMIN,XIAMAX,IADEPS,DWIATU,DWIAWA,DIASTA,
     &    DIAPOS,DIAGAP,
     &  NAMPHS,IAPHVERS,
     &    XYPHI,ZAPHI,NPHIS,NZEES,APHXEDG,
     &  NCINR,
     &    XWDINR,XWDOTR,
     $    XPHOLE,YPHOLE,ZPHOLE,DXHOLE,DZHOLE,
     &  NAMCLY,
     &    XYDISC,ZDISC,DLENC,DWIDC,XYPOFC,ATHC,NZSEGC,NXYSEGC,
     &  YFLOOR,IDEBPXP
C

