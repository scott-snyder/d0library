C----------------------------------------------------------------------
C-
C-                      TRG_SIMUL_FUTURE_USE
C-                      ====================
C-
C-   Contains some provided by the Level 1 Calorimeter Trigger quantities 
C-   which are not yet used.
C-
C-   Created  26-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              TRG_SIMUL_FUTURE_USE to L1C_EVENT_FUTURE_USE.
C- 
C-
C----------------------------------------------------------------------
C
C     9-bit Trigger Tower Z-corrected total transverse energy
C     -------------------------------------------------------
C
      INTEGER TOT_9BIT(POS_ETA:NEG_ETA,
     +                 ETA_MIN:ETA_MAX,
     +                 PHI_MIN:PHI_MAX)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_EVENT_FUTURE_USE/ TOT_9BIT
C
