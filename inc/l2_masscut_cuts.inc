C----------------------------------------------------------------------
C-
C-  l2_masscut_cuts common block
C-   Created  23-AUG-1993   Kathy Fatyga
C-
C----------------------------------------------------------------------
      LOGICAL CURRENT,PASSED
      INTEGER NMAX
      PARAMETER( CURRENT = .TRUE.  )  
      PARAMETER( PASSED = .TRUE. )    
      PARAMETER(NMAX = 40) 
      REAL ETA1_MIN,ETA1_MAX,ETA2_MIN,ETA2_MAX,ETABOOST_MIN,ETABOOST_MAX
      REAL MASS_MIN,MASS_MAX
      LOGICAL LEADING
      COMMON /L2_MASSCUT_CUTS/ LEADING,ETA1_MIN,ETA1_MAX,ETA2_MIN,
     &  ETA2_MAX,ETABOOST_MIN,ETABOOST_MAX,MASS_MAX,MASS_MIN
