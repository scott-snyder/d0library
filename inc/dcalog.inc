C----------------------------------------------------------------------
C-
C-   Created  ??           ??
C-   Modified 6-AUG-1991   K. Wyatt Merritt
C-   Logical variables for GEANT program control for the calorimeter,
C-   derived from D0LOG variables SHWG and SCAL
C-   Also contains cuts for shower parametrizations
C-
C----------------------------------------------------------------------
      LOGICAL NO_SHLB,RAJA_SHLB,JW_SHLB,CALTOW,LINN_PARAM,
     &        TREE_PRUNE,LOAD1,LOAD2,TB87,PLATE_GEOM,PAD_READOUT,
     &        FORCE_SHLB
      REAL LOECUT,HIECUT,X0STEP,DCUSER(10)
      REAL    LOECUT_DEF,HIECUT_DEF,X0STEP_DEF
      INTEGER DCUSEI(10)
      REAL EDRPMAX,WDRPMAX
C
      COMMON /DCALOG/NO_SHLB,RAJA_SHLB,JW_SHLB,CALTOW,LINN_PARAM,
     &        TREE_PRUNE,LOAD1,LOAD2,TB87,PLATE_GEOM,PAD_READOUT,
     &        LOECUT,HIECUT,X0STEP,DCUSER,DCUSEI,LOECUT_DEF,
     &        HIECUT_DEF,X0STEP_DEF,
     &        EDRPMAX,WDRPMAX,FORCE_SHLB
