C----------------------------------------------------------------------
C-
C-   Created   9-NOV-1990   H T DIEHL
C-   Modified 23-NOV-1991   (HTD) Params conform to D0 standard
C-
C----------------------------------------------------------------------
      INTEGER NUM_MUONS,NUMBER_OF_SETS
      REAL PTMIN,ABS_ETA_MAX
      LOGICAL COSMIC_REJECT, REFIT_T0, 
     $        SCINT_ON_TRACK, CAL_ON_TRACK, CD_ON_TRACK
      COMMON /MUON_L2_PARAMS/NUM_MUONS(128),PTMIN(128),ABS_ETA_MAX(128),
     $        NUMBER_OF_SETS,COSMIC_REJECT(128),REFIT_T0(128),
     $        SCINT_ON_TRACK(128),CAL_ON_TRACK(128),CD_ON_TRACK(128)
