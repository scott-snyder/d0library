C----------------------------------------------------------------------
C-
C-                      LEVEL1_CAL_TRIG_RESULTS
C-                      =======================
C-
C-   Level 1 Calorimeter Trigger outputs : end of tree sums and
C-   comparator results. The last ones will be used as input of the
C-   ANDOR simulation.
C-
C-   Created  26-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  10-OCT-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-                              Now supports more than 4 tower count
C-                              thresholds.
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              LEVEL1_CAL_TRIG_RESULTS to L1C_GLOBAL_RESULTS.  
C-   Updated  28-JUN-1993   Philippe Laurens - MSU L1 Trigger   
C-                              Add Large Tile global count
C-
C----------------------------------------------------------------------
C
C     Hot Tower Counting tree results :
C     ---------------------------------
C
      INTEGER HOT_TOWER_COUNT    (EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
      INTEGER HOT_TOWER_CMP_RSLT( CMP_PER_CARD,
     +                            THRD_HOT_MAX, 
     +                            EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C     Scalar Energy Adder Tree results :
C     ----------------------------------
C
      INTEGER GLOBAL_ENERGY          (GL_EMET_THRTYP:GL_TOTL2_THRTYP)
C
      INTEGER GLOBAL_ENERGY_CMP_RSLT (4, CAT3_MAX,
     +                                GL_EMET_THRTYP:GL_TOTL2_THRTYP)
C
C     Total Missing Transverse Energy :
C     ---------------------------------
C
      INTEGER TOTAL_PX, TOTAL_PY, TOTAL_MPT
C
      INTEGER TOTAL_MPT_CMP_RSLT (MPT_CMP_MAX)
C
C
C     Large Tile Counts :
C     -------------------
C
      INTEGER LT_COUNT (LT_REF_MIN:LT_REF_MAX)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_GLOBAL_RESULTS/      HOT_TOWER_COUNT,
     +                                 HOT_TOWER_CMP_RSLT,
     +                                 GLOBAL_ENERGY,
     +                                 GLOBAL_ENERGY_CMP_RSLT,
     +                                 TOTAL_PX, TOTAL_PY,
     +                                 TOTAL_MPT,
     +                                 TOTAL_MPT_CMP_RSLT,
     +                                 LT_COUNT
