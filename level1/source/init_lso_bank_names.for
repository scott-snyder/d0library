      SUBROUTINE INIT_LSO_BANK_NAMES()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the array mapping bank ID numbers to
C-      character strings.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:LSM_ZEB.PARAMS'
      INCLUDE 'D0$INC:LSM_ZEB_CHAR.INC'
      INCLUDE 'D0$INC:LSM_ZEB.INC'
C
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
      IF (FIRST .EQV. .FALSE.) GOTO 999
      FIRST = .FALSE.

      LSO_BANK_NAME(BANK_LS0B) = 'LS0B'
      LSO_ARRAY_SIZE(BANK_LS0B) = (L0_BIN_MAX - L0_BIN_MIN +1)
     &                           *(Z_HIGH - Z_LOW + 1)
      LSO_BANK_NAME(BANK_LSFT) = 'LSFT'
      LSO_ARRAY_SIZE(BANK_LSFT) = 1
      LSO_BANK_NAME(BANK_LSST) = 'LSST'
      LSO_ARRAY_SIZE(BANK_LSST) = 1
      LSO_BANK_NAME(BANK_LSLI) = 'LSLI'
      LSO_ARRAY_SIZE(BANK_LSLI) = (PY_QUANT - EM_ET_QUANT + 1)
     &                           *(PAGE_NUM_MAX - PAGE_NUM_MIN +1)
      LSO_BANK_NAME(BANK_LSLN) = 'LSLN'
      LSO_ARRAY_SIZE(BANK_LSLN) = (PY_QUANT - EM_ET_QUANT + 1)
     &                           *(L0_BIN_MAX - L0_BIN_MIN + 1)
      LSO_BANK_NAME(BANK_LSGA) = 'LSGA'
      LSO_ARRAY_SIZE(BANK_LSGA) = 1
      LSO_BANK_NAME(BANK_LSGE) = 'LSGE'
      LSO_ARRAY_SIZE(BANK_LSGE) = (TOT_L2_QUANT - EM_ET_QUANT + 1)
      LSO_BANK_NAME(BANK_LSEN) = 'LSEN'
      LSO_ARRAY_SIZE(BANK_LSEN) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(HD_TOWER - EM_TOWER + 1)
      LSO_BANK_NAME(BANK_LSEE) = 'LSEE'
      LSO_ARRAY_SIZE(BANK_LSEE) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(HD_TOWER - EM_TOWER + 1)
      LSO_BANK_NAME(BANK_LSAI) = 'LSAI'
      LSO_ARRAY_SIZE(BANK_LSAI) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(HD_TOWER - EM_TOWER + 1)
      LSO_BANK_NAME(BANK_LSAC) = 'LSAC'
      LSO_ARRAY_SIZE(BANK_LSAC) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(TOT_TOWER - EM_TOWER + 1)
      LSO_BANK_NAME(BANK_LSDB) = 'LSDB'
      LSO_ARRAY_SIZE(BANK_LSDB) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(HD_TOWER - EM_TOWER + 1)
      LSO_BANK_NAME(BANK_LSAZ) = 'LSAZ'
      LSO_ARRAY_SIZE(BANK_LSAZ) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(HD_TOWER - EM_TOWER + 1)
      LSO_BANK_NAME(BANK_LSLR) = 'LSLR'
      LSO_ARRAY_SIZE(BANK_LSLR) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PY_QUANT - EM_ET_QUANT + 1)
      LSO_BANK_NAME(BANK_LSLZ) = 'LSLZ'
      LSO_ARRAY_SIZE(BANK_LSLZ) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(PY_QUANT - EM_ET_QUANT + 1)
      LSO_BANK_NAME(BANK_LSPC) = 'LSPC'
      LSO_ARRAY_SIZE(BANK_LSPC) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(PY_PROM - EM_PROM + 1)
     &                           *(PAGE_INDEX_MAX - PAGE_INDEX_MIN +1)
      LSO_BANK_NAME(BANK_LSPS) = 'LSPS'
      LSO_ARRAY_SIZE(BANK_LSPS) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(PY_PROM - EM_PROM + 1)
     &                           *(PAGE_INDEX_MAX - PAGE_INDEX_MIN +1)
      LSO_BANK_NAME(BANK_LSTO) = 'LSTO'
      LSO_ARRAY_SIZE(BANK_LSTO) = (TOT_L2_QUANT - EM_ET_QUANT +1)
      LSO_BANK_NAME(BANK_LSZN) = 'LSZN'
      LSO_ARRAY_SIZE(BANK_LSZN) = (PY_QUANT - EM_ET_QUANT + 1)
     &                           *(PAGE_NUM_MAX - PAGE_NUM_MIN + 1)
      LSO_BANK_NAME(BANK_LSEC) = 'LSEC'
      LSO_ARRAY_SIZE(BANK_LSEC) = (PY_QUANT - EM_ET_QUANT + 1)
      LSO_BANK_NAME(BANK_LSRZ) = 'LSRZ'
      LSO_ARRAY_SIZE(BANK_LSRZ) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(TOT_TOWER - EM_TOWER + 1)
     &                           *(Z_COORD - R_COORD + 1)
      LSO_BANK_NAME(BANK_LSPH) = 'LSPH'
      LSO_ARRAY_SIZE(BANK_LSPH) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
      LSO_BANK_NAME(BANK_LSFF) = 'LSFF'
      LSO_ARRAY_SIZE(BANK_LSFF) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PHI_MAX - PHI_MIN + 1)
     &                           *(PY_QUANT - EM_ET_QUANT + 1)
     &                           *(PAGE_NUM_MAX - PAGE_NUM_MIN +1)
      LSO_BANK_NAME(BANK_LSTE) = 'LSTE'
      LSO_ARRAY_SIZE(BANK_LSTE) = (NEG_ETA - POS_ETA + 1)
     &                           *(ETA_RANGE_MAX - ETA_RANGE_MIN + 1)
     &                           *(PY_QUANT - EM_ET_QUANT + 1)
C----------------------------------------------------------------------
  999 RETURN
      END
