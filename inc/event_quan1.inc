C----------------------------------------------------------------------
C-
C-   Updated  24-JUN-1993   Rajendran Raja   
C-
C----------------------------------------------------------------------
      INTEGER NVERT_MAX     
      PARAMETER( NVERT_MAX = 3 )        
      INTEGER NJETS_MAX
      PARAMETER( NJETS_MAX = 20 )       
      INTEGER MAX_LEPHOT
      PARAMETER( MAX_LEPHOT = 5 )       
                                        
      INTEGER NV
      PARAMETER( NV = 9 )               
      INTEGER NJ
      PARAMETER( NJ = 31)              
      INTEGER NE
      PARAMETER( NE = 29 )              
      INTEGER NP
      PARAMETER( NP = 29 )              
      INTEGER NM
      PARAMETER( NM = 23 )              
      INTEGER NN
      PARAMETER( NN = 2 )              
C
      COMMON /EVENT_QUAN1/ P9_VERTEX,NVER,P25_JETS,
     &  P2_NEUT,P24_ELECTRON,P18_PHOTON,P23_MUON,
     &  NJETS,ELECTRON,PHOTON,MUON,
     &  NELEC,NPHO,NMUO
      REAL    P9_VERTEX(NV,NVERT_MAX)
      INTEGER NVER   
      REAL    P25_JETS(NJ,NJETS_MAX)
      INTEGER NJETS
      REAL    P2_NEUT(NN)
      REAL    P24_ELECTRON(NE,MAX_LEPHOT),P18_PHOTON(NP,MAX_LEPHOT),
     &  P23_MUON(NM,MAX_LEPHOT)
      LOGICAL ELECTRON,PHOTON,MUON
      INTEGER NELEC,NPHO,NMUO
