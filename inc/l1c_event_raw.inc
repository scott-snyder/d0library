C----------------------------------------------------------------------
C-
C-                      TRG_SIMUL_RAW_EVENT
C-                      ===================
C-
C-   Created  13-MAR-1990   Sylvain Tisserant (MSU)
C-   Modified 1-Apr-91      Maris Abolins
C-                          Made ISAJET_WG ,ISAJET_CS real
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              TRG_SIMUL_RAW_EVENT to L1C_EVENT_RAW.
C-
C----------------------------------------------------------------------
C
C     Run and Event Numbers
C     =====================
C
      INTEGER RUN_NUMBER, EVENT_NUMBER
C
C     Primary Vertex Z Position
C     =========================
C
      REAL    Z_VERTEX
C
C     Isajet Information | Reaction type 
C     ================== | Weight
C                        | Cross Section
C                        | Number of primary particules
C                        | Momentum components
C                        | Momentum
C                        | Transverse Momentum
C                        | Total Energy
C                        | Azimuthal and polar angles
C                        | Missing Transverse Momentum components
C
      INTEGER ISAJET_ID,  ISAJET_N
      REAL    ISAJET_WG   ,ISAJET_CS              
      REAL    ISAJET_PX,  ISAJET_PY,   ISAJET_PZ
      REAL    ISAJET_P,   ISAJET_PT,   ISAJET_E
      REAL    ISAJET_PHI, ISAJET_THETA
      REAL    ISAJET_MPX, ISAJET_MPY,  ISAJET_MPT, ISAJET_MPT_PHI
C
C     Calorimetry
C     ===========
C
      REAL    TOTAL_CALORIMETRY, TT_CALORIMETRY (EM_TOWER:HD_TOWER)
      REAL    TT_ENERGY (POS_ETA:NEG_ETA,
     +                   ETA_MIN:ETA_MAX,
     +                   PHI_MIN:PHI_MAX,
     +                   EM_TOWER:HD_TOWER)
      INTEGER HIT_TRIGGER_TOWER(EM_TOWER:HD_TOWER)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_EVENT_RAW/       RUN_NUMBER, EVENT_NUMBER,
     +                             Z_VERTEX,
     +                             ISAJET_ID,  ISAJET_WG, ISAJET_CS,
     +                             ISAJET_N,
     +                             ISAJET_PX,  ISAJET_PY, ISAJET_PZ,
     +                             ISAJET_P,   ISAJET_PT, ISAJET_E,
     +                             ISAJET_PHI, ISAJET_THETA,
     +                             ISAJET_MPX, ISAJET_MPY,
     +                             ISAJET_MPT, ISAJET_MPT_PHI,
     +                             TOTAL_CALORIMETRY, TT_CALORIMETRY,
     +                             TT_ENERGY,
     +                             HIT_TRIGGER_TOWER
C
