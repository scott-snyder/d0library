C----------------------------------------------------------------------
C-
C-   Created  30-MAY-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER NDPTH
      PARAMETER( NDPTH = 5 )            
      INTEGER NDEAD
      PARAMETER( NDEAD = 2 )                    
      INTEGER NHM
      PARAMETER( NHM =  NDPTH+NDEAD)
      REAL ENDPTH,PEDPTH,ETOT,ET,ETRANS,EMAX
      INTEGER ETAMX,PHIMX
C
C ****  Endpth,pedpth are the energies and percentage energies
C ****  in the 5 depths. 
C
      COMMON/CEMPRF/ENDPTH(NDPTH),PEDPTH(NDPTH),
     &  ETOT,ET,ETRANS,EMAX(NDPTH),ETAMX(NDPTH),PHIMX(NDPTH)
