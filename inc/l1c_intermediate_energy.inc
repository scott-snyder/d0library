C----------------------------------------------------------------------
C-
C-                      GLOBAL_ENERGY_SUMMATIONS
C-                      ========================
C-
C-   Contains the intermediate results of the energy summation trees
C-   (scalar and missing Pt).
C-   Final results are stored the LVL1_CAL_TRIG_RESULTS common.
C-
C-   Created  27-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              GLOBAL_ENERGY_SUMMATIONS to
C-                              L1C_INTERMEDIATE_ENERGY.
C-   Updated   6-JUL-1993   Philippe Laurens - MSU L1 Trigger   
C-                              Add Large Tile energy 
C-
C----------------------------------------------------------------------
C
C     First performed by CTFE cards summations :
C     ------------------------------------------
C
      INTEGER CTFE_SUM4       (CTFE_MAX,
     +                         EM_ET_QUANT:PY_QUANT)
C
C     Scalar Adder Trees : intermediate results (1st and 2nd tiers) :
C     ---------------------------------------------------------------
C
      INTEGER FRST_SCALAR_SUM (FRST_SCALAR_MAX,
     +                         EM_ET_QUANT:HD_L2_QUANT)
C
      INTEGER SCND_SCALAR_SUM (SCND_SCALAR_MAX,
     +                         EM_ET_QUANT:HD_L2_QUANT)
C
C     Missing Transverse Energy Adder Tree : intermediate results :
C     -------------------------------------------------------------
C
      INTEGER FRST_MPT_SUM    (FRST_MPT_MAX,
     +                         POS_MPT:NEG_MPT,
     +                         PX_QUANT:PY_QUANT)
C
      INTEGER SCND_MPT_SUM    (SCND_MPT_MAX,
     +                         POS_MPT:NEG_MPT,
     +                         PX_QUANT:PY_QUANT)
C
C
C     energy deposited in each Large Tile (including offset)
C     -----------------------------------
C
      INTEGER LT_ENERGY( POS_ETA:NEG_ETA,
     &                   LT_ETA_MIN:LT_ETA_MAX,
     &                   LT_PHI_MIN:LT_PHI_MAX)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_INTERMEDIATE_ENERGY/  CTFE_SUM4,
     +                                  FRST_SCALAR_SUM,
     +                                  SCND_SCALAR_SUM,
     +                                  FRST_MPT_SUM,
     +                                  SCND_MPT_SUM,
     +                                  LT_ENERGY
C
