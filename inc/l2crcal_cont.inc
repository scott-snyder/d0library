C----------------------------------------------------------------------
C-
C-   Created  31-MAY-1990   Richard V. Astur
C-   L2CRCAL_CONT: L2 COSMIC RAY CALORIMETER TOOL CONTROL COMMON
C-
C----------------------------------------------------------------------
C--- Init Okay?
      INTEGER L2CRCAL_PARAM             
      INTEGER L2CRCAL_PARAM_MAX         
      PARAMETER( L2CRCAL_PARAM_MAX = 5)
      LOGICAL L2CRCAL_INIT_OK           
      INTEGER L2CRCAL_EVTNUM            
      INTEGER IZL2CRCAL_RCP
      PARAMETER( IZL2CRCAL_RCP = 3)     
      INTEGER MUON_TRACK_MODE(5)        
      REAL DPHI_CUT(5)                  
c                                       
      INTEGER IMUHIT_CONE(5)            

C--- THIS Portion handles muon tracks
      INTEGER NTRAK
      INTEGER NTRAK_MAX
      PARAMETER (NTRAK_MAX = 10)
      INTEGER MUFLAG( NTRAK_MAX )
      REAL XPART(NTRAK_MAX),YPART(NTRAK_MAX),ZPART(NTRAK_MAX)
      REAL XCOS(NTRAK_MAX),YCOS(NTRAK_MAX),ZCOS(NTRAK_MAX)
C---This portion handles CC cells that were hit
      INTEGER NMCELL,NMCELL_MAX
      PARAMETER(NMCELL_MAX = 36)
      INTEGER IMUHIT_ETA(NMCELL_MAX)    
      INTEGER IMUHIT_PHI(NMCELL_MAX)
      INTEGER IMUHIT_LYR(NMCELL_MAX)
      REAL    MUHIT_ETOT
      REAL    MUHIT_E(NMCELL_MAX)       
C
      INTEGER NCIETA                    
      INTEGER NCLAYER                   
      PARAMETER (NCIETA = 12)
      PARAMETER (NCLAYER= 8)
      REAL HI,LO
      PARAMETER (HI = 1.6E38)
      PARAMETER (LO = -1.6E38)
      REAL RBEG( 2*NCLAYER + 2)         
C RBEG is the smallest Y value of this depth assuming the cell was
C centered on the y axis.
      REAL ZBEG(2*NCIETA + 2 , 2*NCLAYER + 2) 
      REAL ZEND(2*NCIETA + 2 , 2*NCLAYER + 2)
C boundary: ZBEG is the lower z boundary for this cell.
      INTEGER IDLAYER( 2*NCLAYER + 2)   
C                                       
      INTEGER IDIETA( 2*NCIETA + 2)      
C                                       
      LOGICAL OUT_OF_CC( 2*NCIETA + 2 , 2*NCLAYER + 2)  
      COMMON /L2CRCAL_CONT/NMCELL,IMUHIT_ETA,IMUHIT_PHI,IMUHIT_LYR,
     &  MUHIT_E,
     & NTRAK,XPART,YPART,ZPART,XCOS, YCOS, ZCOS,MUFLAG,
     & ZBEG,ZEND,RBEG,IDLAYER,IDIETA,DPHI_CUT,
     &  OUT_OF_CC,L2CRCAL_INIT_OK,IMUHIT_CONE,MUHIT_ETOT,
     &  MUON_TRACK_MODE,L2CRCAL_PARAM


