C----------------------------------------------------------------------
C-
C-      Cuts for the level 2 Missing Et tool
C-
C-   Created  29-JUN-1991   James T. Linnemann
C-   Updated  11-JUL-1992  U. Heintz: add missing Et significance cut
C-
C----------------------------------------------------------------------
      INTEGER NPAR_MAX                  
      PARAMETER( NPAR_MAX = 32 )
      REAL ETMISS_MIN(NPAR_MAX)         
      REAL ETMISS_SIG_MIN(NPAR_MAX)     
      INTEGER NPAR                      
      COMMON /L2ETMISS_CUTS/ NPAR,ETMISS_MIN,ETMISS_SIG_MIN
