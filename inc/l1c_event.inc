C----------------------------------------------------------------------
C-
C-                      TRG_SIMUL_EVENT
C-                      ===============
C-
C-    Describes the Level 1 Calorimeter Trigger input event.
C-
C-    Created   5-JAN-1990   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek   
C-                              Changed name of common block from
C-                              TRG_SIMUL_EVENT to L1C_EVENT.
C-
C----------------------------------------------------------------------
C
C     Level 0 informations : bin number ([-15,15]) and validation :
C     -------------------------------------------------------------
C
      INTEGER LEVEL_0_NZ
      LOGICAL LEVEL_0_OK
C
C     Trigger tower FADC values :
C     ---------------------------
C
      INTEGER FADC_BYTE(POS_ETA:NEG_ETA,
     +                  ETA_MIN:ETA_MAX,
     +                  PHI_MIN:PHI_MAX,
     +                  EM_TOWER:HD_TOWER)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON /L1C_EVENT/ LEVEL_0_NZ, LEVEL_0_OK, FADC_BYTE
C
