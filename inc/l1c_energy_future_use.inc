C----------------------------------------------------------------------
C-
C-                      GLOBAL_ENERGY_FUTURE_USE
C-                      ========================
C-
C-   Contains unused Energy Adder Tree control parameters and outputs.
C-   It could be useful for future extensions.
C-
C-   Suffices : _MASK, _SUB, _REF, _OUT2, _CMP_RSLT have the same meaning
C-              as corresponding CAT2 and CAT3 routine arguments.
C-
C-   Created  27-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              GLOBAL_ENERGY_FUTURE_USE to
C-                              L1C_ENERGY_FUTURE_USE. 
C-
C----------------------------------------------------------------------
C
C     Scalar Adder Trees : first tier :
C     ---------------------------------
C                                                             Control parameters
C                                                             ------------------
      LOGICAL FRST_SCALAR_MASK     (CTFE_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C
      INTEGER FRST_SCALAR_SUB      (FRST_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C
      INTEGER FRST_SCALAR_REF      (4, FRST_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C                                                                        Outputs
C                                                                        -------
      INTEGER FRST_SCALAR_OUT2     (FRST_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C
      INTEGER FRST_SCALAR_CMP_RSLT (4, FRST_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C
C     Scalar Adder Trees : second tier :
C     ----------------------------------
C                                                             Control parameters
C                                                             ------------------
      LOGICAL SCND_SCALAR_MASK     (FRST_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C
      INTEGER SCND_SCALAR_SUB      (SCND_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C
      INTEGER SCND_SCALAR_REF      (4, SCND_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C                                                                        Outputs
C                                                                        -------
      INTEGER SCND_SCALAR_OUT2     (SCND_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C
      INTEGER SCND_SCALAR_CMP_RSLT (4, SCND_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C
C     Scalar Adder Trees : third tier :
C     ---------------------------------
C                                                             Control parameters
C                                                             ------------------
      LOGICAL THRD_SCALAR_MASK     (SCND_SCALAR_MAX,
     +                              EM_ET_QUANT:HD_L2_QUANT)
C                                                                        Outputs
C                                                                        -------
      INTEGER THRD_SCALAR_OUT2     (EM_ET_QUANT:HD_L2_QUANT)
C
C     Missing Transverse Energy Adder Tree : first tier :
C     ---------------------------------------------------
C                                                             Control parameters
C                                                             ------------------
      LOGICAL FRST_MPT_MASK        (CTFE_MAX,
     +                              PX_QUANT:PY_QUANT)
C
      INTEGER FRST_MPT_SUB         (FRST_MPT_MAX,
     +                              POS_MPT:NEG_MPT,
     +                              PX_QUANT:PY_QUANT)
C
      INTEGER FRST_MPT_REF         (4, FRST_MPT_MAX,
     +                              POS_MPT:NEG_MPT,
     +                              PX_QUANT:PY_QUANT)
C                                                                        Outputs
C                                                                        -------
      INTEGER FRST_MPT_OUT2        (FRST_MPT_MAX,
     +                              POS_MPT:NEG_MPT,
     +                              PX_QUANT:PY_QUANT)
C
      INTEGER FRST_MPT_CMP_RSLT    (4, FRST_MPT_MAX,
     +                              POS_MPT:NEG_MPT,
     +                              PX_QUANT:PY_QUANT)
C
C     Missing Transverse Energy Adder Tree : second tier :
C     ----------------------------------------------------
C                                                             Control parameters
C                                                             ------------------
      LOGICAL SCND_MPT_MASK        (FRST_MPT_MAX,
     +                              POS_MPT:NEG_MPT,
     +                              PX_QUANT:PY_QUANT)
C
      INTEGER SCND_MPT_SUB         (SCND_MPT_MAX,
     +                              POS_MPT:NEG_MPT,
     +                              PX_QUANT:PY_QUANT)
C
      INTEGER SCND_MPT_REF         (4, SCND_MPT_MAX,
     +                              POS_MPT:NEG_MPT,
     +                              PX_QUANT:PY_QUANT)
C                                                                        Outputs
C                                                                        -------
      INTEGER SCND_MPT_OUT2        (SCND_MPT_MAX,
     +                              POS_MPT:NEG_MPT,
     +                              PX_QUANT:PY_QUANT)
C
      INTEGER SCND_MPT_CMP_RSLT    (4, SCND_MPT_MAX,
     +                              POS_MPT:NEG_MPT,
     +                              PX_QUANT:PY_QUANT)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_ENERGY_FUTURE_USE/ FRST_SCALAR_MASK,
     + FRST_SCALAR_SUB, FRST_SCALAR_REF, FRST_SCALAR_OUT2,
     + FRST_SCALAR_CMP_RSLT, SCND_SCALAR_MASK, SCND_SCALAR_SUB,
     + SCND_SCALAR_REF, SCND_SCALAR_OUT2, SCND_SCALAR_CMP_RSLT,
     + THRD_SCALAR_MASK, THRD_SCALAR_OUT2, FRST_MPT_MASK,
     + FRST_MPT_SUB, FRST_MPT_REF, FRST_MPT_OUT2,
     + FRST_MPT_CMP_RSLT, SCND_MPT_MASK, SCND_MPT_SUB,
     + SCND_MPT_REF, SCND_MPT_OUT2, SCND_MPT_CMP_RSLT
C
