C----------------------------------------------------------------------
C-
C-      Cuts for the level 2 Missing Et tool
C-
C-   Created  29-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      INTEGER NPAR_MAX                  
      PARAMETER( NPAR_MAX = 32 )
      REAL ETSUM_MIN(NPAR_MAX)         
      REAL ETOT_MAX(NPAR_MAX)         
      INTEGER NPAR                      
      COMMON /L2ETSUM_CUTS/ NPAR,ETSUM_MIN,ETOT_MAX
