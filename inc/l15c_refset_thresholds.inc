C----------------------------------------------------------------------
C-
C-                   L1.5 TRIGGER TOWER THRESHOLDS
C-                      ====================
C-
C-   Created   6-JAN-1994   Zhengzhi Zhang
C----------------------------------------------------------------------
C
C     EMT_THRSHLD : trigger tower EMt thresholds:
C     ------------------------------------------------------------
C
      INTEGER L15_EMT_THRSHLD   (L15CT_NUM_MIN:L15CT_NUM_MAX,
     +                       L15TM_NUM_MIN:L15TM_NUM_MAX,
     +                       POS_ETA:NEG_ETA,
     +                       ETA_MIN:ETA_MAX,
     +                       PHI_MIN:PHI_MAX)
C
C     TOT_THRSHLD : trigger tower TOT Et thresholds:
C     -----------------------------------------------------------
C
      INTEGER L15_TOT_THRSHLD   (L15CT_NUM_MIN:L15CT_NUM_MAX,
     +                       L15TM_NUM_MIN:L15TM_NUM_MAX,
     +                       POS_ETA:NEG_ETA,
     +                       ETA_MIN:ETA_MAX,
     +                       PHI_MIN:PHI_MAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L15C_REFSET_THRESHOLDS/ l15_EMT_THRSHLD, L15_TOT_THRSHLD
C
