C----------------------------------------------------------------------
C-
C-                      LV1_CALTRIG_RSLT_BACKUP
C-                      =======================
C-
C-   A copy of the Level 1 Calorimeter Trigger simulation outputs 
C-
C-   Created  11-JUL-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE 
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              LV1_CALTRIG_RSLT_BACKUP to L1C_RESULT_BACKUP. 
C-
C----------------------------------------------------------------------
C
C     Hot Tower Counting tree results :
C     ---------------------------------
C
      INTEGER BACK_HOT_TOWER_COUNT    (EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
      INTEGER BACK_HOT_TOWER_CMP_RSLT (CMP_PER_CARD,
     +                                 THRD_HOT_MAX,
     +                            EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C     Scalar Energy Adder Tree results :
C     ----------------------------------
C
      INTEGER BACK_GLOBAL_ENERGY      (GL_EMET_THRTYP:GL_TOTL2_THRTYP)
C
      INTEGER BACK_GLOBAL_ENERGY_CMP_RSLT (4, CAT3_MAX,
     +                                GL_EMET_THRTYP:GL_TOTL2_THRTYP)
C
C     Total Missing Transverse Energy :
C     ---------------------------------
C
      INTEGER BACK_TOTAL_PX, BACK_TOTAL_PY, BACK_TOTAL_MPT
C
      INTEGER BACK_TOTAL_MPT_CMP_RSLT (MPT_CMP_MAX)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_RESULT_BACKUP/       BACK_HOT_TOWER_COUNT,
     +                                 BACK_HOT_TOWER_CMP_RSLT,
     +                                 BACK_GLOBAL_ENERGY,
     +                                 BACK_GLOBAL_ENERGY_CMP_RSLT,
     +                                 BACK_TOTAL_PX, BACK_TOTAL_PY,
     +                                 BACK_TOTAL_MPT,
     +                                 BACK_TOTAL_MPT_CMP_RSLT
