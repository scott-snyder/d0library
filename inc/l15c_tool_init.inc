C----------------------------------------------------------------------
C-
C-   Created   1-APR-1994   Zhengzhi Zhang
C-
C----------------------------------------------------------------------
      INTEGER L15CT_NUM,L15TM_NUM
      INTEGER L15TL_LOC_NUM,L15TL_GLB_NUM
C
      REAL*4 L15_FLOAT_PARAMS(L15CT_NUM_MIN:L15CT_NUM_MAX,
     +                        L15TM_NUM_MIN:L15TM_NUM_MAX,
     +                        L15TL_GLB_DSP:L15TL_LOC_DSP,
     +                        L15TL_NUM_MIN:L15TL_NUM_MAX,
     +                        L15PM_NUM_MIN:L15PM_NUM_MAX)
      INTEGER L15_INTG_PARAMS(L15CT_NUM_MIN:L15CT_NUM_MAX,
     +                        L15TM_NUM_MIN:L15TM_NUM_MAX,
     +                        L15TL_GLB_DSP:L15TL_LOC_DSP,
     +                        L15TL_NUM_MIN:L15TL_NUM_MAX,
     +                        L15PM_NUM_MIN:L15PM_NUM_MAX)
c     a flag indicating if a parameter is an integer or an float:
      INTEGER L15_TYPE_PARAMS(L15CT_NUM_MIN:L15CT_NUM_MAX,
     +                        L15TM_NUM_MIN:L15TM_NUM_MAX,
     +                        L15TL_GLB_DSP:L15TL_LOC_DSP,
     +                        L15TL_NUM_MIN:L15TL_NUM_MAX,
     +                        L15PM_NUM_MIN:L15PM_NUM_MAX)
      COMMON /L15C_TOOL_INIT/L15CT_NUM,L15TM_NUM,
     +                       L15TL_LOC_NUM,L15TL_GLB_NUM,
     +                       L15_FLOAT_PARAMS,
     +                       L15_INTG_PARAMS,
     +                       L15_TYPE_PARAMS



