C----------------------------------------------------------------------
C-
C-   Created  23-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER NMASS
      PARAMETER( NMASS = 10 )
      COMMON/SIG_EFF/TM_EFF,
     &               EE_SBR,EMU_SBR,EJETS_SBR,EJETS_TAG_SBR,
     &               MUMU_SBR,MUJETS_SBR,MUJETS_TAG_SBR,
     &               EE_ERR,EMU_ERR,EJETS_ERR,EJETS_TAG_ERR,
     &               MUMU_ERR,MUJETS_ERR,MUJETS_TAG_ERR,
     &               MPOL,NEVENTS_DISC,LUMIN,ELUMIN

      INTEGER MPOL
      INTEGER NEVENTS_DISC
      REAL    LUMIN,ELUMIN
      REAL    TM_EFF(NMASS)
      REAL    EE_SBR(NMASS),EMU_SBR(NMASS),EJETS_SBR(NMASS),
     &  EJETS_TAG_SBR(NMASS),MUMU_SBR(NMASS),MUJETS_SBR(NMASS),
     &  MUJETS_TAG_SBR(NMASS)
      REAL    EE_ERR(NMASS),EMU_ERR(NMASS),EJETS_ERR(NMASS),
     &  EJETS_TAG_ERR(NMASS),MUMU_ERR(NMASS),MUJETS_ERR(NMASS),
     &  MUJETS_TAG_ERR(NMASS)
C
