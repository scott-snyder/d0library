C----------------------------------------------------------------------
C-
C-                      HOT_TOWER_REFERENCES
C-                      ====================
C-
C-   Hot Tower references : Trigger Tower CTFE reference sets and end
C-   of adder tree Hot Tower Count references.
C-
C-   Created  21-NOV-1989   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              HOT_TOWER_REFERENCES to
C-                              L1C_REFSET_AND_COUNT_THRESHOLD. 
C-   Updated  28-JUN-1993   Philippe Laurens - MSU L1 Trigger   
C-                              Add Large Tile Reference Sets
C-                              and Large Tile offsets (zero energy response)
C-
C----------------------------------------------------------------------
C
C     EMT_THRSHLD : trigger tower EMt CTFE comparator references :
C     ------------------------------------------------------------
C
      INTEGER EMT_THRSHLD   (POS_ETA:NEG_ETA,
     +                       ETA_MIN:ETA_MAX,
     +                       PHI_MIN:PHI_MAX,
     +                       EM_ET_REF_MIN:EM_ET_REF_MAX)
C
C     HDT_VETO : trigger tower HDt CTFE veto references :
C     ---------------------------------------------------
C
      INTEGER HDT_VETO      (POS_ETA:NEG_ETA,
     +                       ETA_MIN:ETA_MAX,
     +                       PHI_MIN:PHI_MAX,
     +                       EM_ET_REF_MIN:EM_ET_REF_MAX)
C
C     TOT_THRSHLD : trigger tower Et CTFE comparator references :
C     -----------------------------------------------------------
C
      INTEGER TOT_THRSHLD   (POS_ETA:NEG_ETA,
     +                       ETA_MIN:ETA_MAX,
     +                       PHI_MIN:PHI_MAX,
     +                       TOT_ET_REF_MIN:TOT_ET_REF_MAX)
C
C     End of adder tree Hot Tower Count thresholds :
C     ----------------------------------------------
C
      INTEGER HOT_COUNT_REF (TOWER_CNT_THRSH_MIN:TOWER_CNT_THRSH_MAX, 
     &                       EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C     The Large Tile Reference Sets
C     -----------------------------
C
      INTEGER LT_THRSHLD( POS_ETA:NEG_ETA,
     &                    LT_ETA_MIN:LT_ETA_MAX,
     &                    LT_PHI_MIN:LT_PHI_MAX,
     &                    LT_REF_MIN:LT_REF_MAX )
C
C     Offset in each Large Tile
C     -------------------------
C
      INTEGER LT_ZERESP( POS_ETA:NEG_ETA,
     &                   LT_ETA_MIN:LT_ETA_MAX,
     &                   LT_PHI_MIN:LT_PHI_MAX)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_REFSET_AND_COUNT_THRESHOLD/ EMT_THRSHLD, HDT_VETO,
     +                              TOT_THRSHLD,
     +                              HOT_COUNT_REF,
     +                              LT_THRSHLD,
     +                              LT_ZERESP
C
