C----------------------------------------------------------------------
C-
C-   Variables used by the framework portion of the Level 1 Simulator
C-
C-
C-   Created   2-AUG-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed the name of the common block from
C-                              LEVEL1_FRAMEWORK to L1FW_ANDOR_AND_MISC.  
C-   Updated   5-NOV-1991   Philippe Laurens, Steven Klocek  
C-                              Added variable to hold prescalers for each
C-                              specific trigger. 
C-   Updated  13-DEC-1991   P.Laurens, S.Klocek, Add Level 1.5 
C-   Updated  17-JAN-1992   Philippe Laurens, Steven Klocek  
C-                              Added variables for additional quantities
C-                              unpacked from the datablock. 
C-   Updated  25-NOV-1992   Philippe Laurens, Steven Klocek  
C-                              Added variables for recording
C-                              programming from additional COOR messages. 
C-                              Add L0_FASTZ_GOOD_SCALER and
C-                              L0_FASTZ_GOOD_INCREMENTED 
C-   Updated  10-JUL-1993   Philippe Laurens - MSU L1 Trigger   
C-                              extend ST_VS_RS to Large Tiles
C-                              
C----------------------------------------------------------------------
      LOGICAL ST_VS_RS(TRG_NUM_MIN:TRG_NUM_MAX,
     &                 EM_ET_REF_MIN:LT_REF_MAX)
C
      LOGICAL ST_STARTDGT(GEO_NUM_MIN:GEO_NUM_MAX,
     &                    TRG_NUM_MIN:TRG_NUM_MAX)
C
      INTEGER ST_PRESCALER(TRG_NUM_MIN:TRG_NUM_MAX)
C
      LOGICAL SPECTRIG_ANDOR_ALLOC(ANDOR_NUM_MIN:ANDOR_NUM_MAX,
     &                             TRG_NUM_MIN:TRG_NUM_MAX)
C
      LOGICAL SPECTRIG_ANDOR_POLARITY(ANDOR_NUM_MIN:ANDOR_NUM_MAX,
     &                                TRG_NUM_MIN:TRG_NUM_MAX)
C
      LOGICAL ANDOR_TERM(ANDOR_NUM_MIN:ANDOR_NUM_MAX)
C
      INTEGER LV1_ANDOR_TERM_TYPE(ANDOR_NUM_MIN:ANDOR_NUM_MAX,
     &                   AO_THRSH_TYPE_INDEX:AO_THRSH_SUB2_INDEX)
C
      LOGICAL ANDOR_TERM_ASSIGNED(ANDOR_NUM_MIN:ANDOR_NUM_MAX)
C
      LOGICAL EXIST_SPECTRIG(TRG_NUM_MIN:TRG_NUM_MAX)
C
      LOGICAL EXIST_GEOSECT(GEO_NUM_MIN:GEO_NUM_MAX)
C
      LOGICAL GS_STARTDGT(GEO_NUM_MIN:GEO_NUM_MAX)
      LOGICAL FSTD_ENABLED(TRG_NUM_MIN:TRG_NUM_MAX)
      LOGICAL FSTD_ANDOR_FIRED(TRG_NUM_MIN:TRG_NUM_MAX)
      LOGICAL ST_LEVEL1_STATE(TRG_NUM_MIN:TRG_NUM_MAX)
C
      LOGICAL ST_FSTD_VETO(TRG_NUM_MIN:TRG_NUM_MAX)
      LOGICAL OBEY_FEBUSY(TRG_NUM_MIN:TRG_NUM_MAX)
      LOGICAL OBEY_L2BUSY(TRG_NUM_MIN:TRG_NUM_MAX)
      LOGICAL FEBUSY_GS_TO_ST(GEO_NUM_MIN:GEO_NUM_MAX,
     &                        TRG_NUM_MIN:TRG_NUM_MAX)
C
      LOGICAL ST_ENABLED(TRG_NUM_MIN:TRG_NUM_MAX)
      LOGICAL ST_ALLOCATED(TRG_NUM_MIN:TRG_NUM_MAX)
C
C       Level 0 Good Scaler
C
      INTEGER L0_FASTZ_GOOD_SCALER(2)
      INTEGER L0_FASTZ_GOOD_INCREMENTED
C
C       The following variables are not driven by L1SIM, but are used when an
C       existing Data Block is unpacked
C
      LOGICAL GS_FRONT_END_BUSY(GEO_NUM_MIN:GEO_NUM_MAX)
      LOGICAL ST_FRONT_END_BUSY_DISABLE(TRG_NUM_MIN:TRG_NUM_MAX)
      LOGICAL ST_L2_DISABLE(TRG_NUM_MIN:TRG_NUM_MAX)
C
C       The following variables are used for recording run statistics related
C       to Level 1.
      INTEGER COUNT_EVENTS_PROCESSED
      INTEGER COUNT_EVENTS_PASSED
      INTEGER COUNT_EVENT_PURE_L1
      INTEGER COUNT_ST_PASSED(TRG_NUM_MIN:TRG_NUM_MAX)
C
      COMMON /L1FW_ANDOR_AND_MISC/ ST_VS_RS, ST_STARTDGT,
     &  SPECTRIG_ANDOR_ALLOC, SPECTRIG_ANDOR_POLARITY, ANDOR_TERM,
     &  LV1_ANDOR_TERM_TYPE, ANDOR_TERM_ASSIGNED, EXIST_SPECTRIG,
     &  EXIST_GEOSECT, ST_PRESCALER,
     &  COUNT_EVENTS_PROCESSED, COUNT_EVENTS_PASSED, 
     &  COUNT_EVENT_PURE_L1, COUNT_ST_PASSED,
     &  FSTD_ENABLED, FSTD_ANDOR_FIRED, ST_LEVEL1_STATE,
     &  GS_FRONT_END_BUSY, GS_STARTDGT,
     &  ST_FRONT_END_BUSY_DISABLE, ST_L2_DISABLE, OBEY_FEBUSY,
     &  OBEY_L2BUSY, FEBUSY_GS_TO_ST, ST_ENABLED, ST_FSTD_VETO,
     &  ST_ALLOCATED, L0_FASTZ_GOOD_SCALER, L0_FASTZ_GOOD_INCREMENTED
