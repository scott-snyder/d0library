C----------------------------------------------------------------------
C-
C-                      HOT_TOWER_FUTURE_USE
C-                      ====================
C-
C-   Contains unused Hot Tower counting tree control parameters and
C-   outputs. It could be useful for a future extensions.
C-
C-   Created  26-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              HOT_TOWER_FUTURE_USE to L1C_COUNT_FUTURE_USE. 
C-
C----------------------------------------------------------------------
C
C     Second tier : unused CAT2 control parameters :
C     ----------------------------------------------
C
      INTEGER SCND_HOT_SUB      (SCND_HOT_MAX,
     +                           EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
      INTEGER SCND_HOT_REF      (4,SCND_HOT_MAX,
     +                           EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
      LOGICAL SCND_HOT_MASK     (CHTCR_MAX,
     +                           EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C     Second tier : unused outputs :
C     ------------------------------
C
      INTEGER SCND_HOT_OUT2     (SCND_HOT_MAX,
     +                           EM_ET_REF_MIN:TOT_ET_REF_MAX)

      INTEGER SCND_HOT_CMP_RSLT (4,SCND_HOT_MAX,
     +                           EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C     Third tier : unused CAT2 control parameters :
C     ---------------------------------------------
C
      INTEGER THRD_HOT_SUB      (EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
      LOGICAL THRD_HOT_MASK     (SCND_HOT_MAX,
     +                           EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C     Third tier : unused outputs :
C     -----------------------------
C
      INTEGER THRD_HOT_OUT2     (EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_COUNT_FUTURE_USE/ SCND_HOT_SUB,
     +                              SCND_HOT_REF,
     +                              SCND_HOT_MASK,
     +                              SCND_HOT_OUT2,
     +                              SCND_HOT_CMP_RSLT,
     +                              THRD_HOT_SUB,
     +                              THRD_HOT_MASK,
     +                              THRD_HOT_OUT2
C
