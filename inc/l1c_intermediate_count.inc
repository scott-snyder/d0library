C----------------------------------------------------------------------
C-
C-                      HOT_TOWER_COUNTING
C-                      ==================
C-
C-   Intermediate Hot Tower counting results.
C-
C-   Created  21-NOV-1989   Sylvain Tisserant (MSU)
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              HOT_TOWER_COUNTING to L1C_INTERMEDIATE_COUNT.  
C-
C----------------------------------------------------------------------
C
C     8 sub-sections of 32 bits per CHTCR cards : provided by CTFE
C     ------------------------------------------------------------
C
      LOGICAL CHTCR_INPUT    (32,CHTCR_MAX,
     +                        EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C     Hot Tower counts for each CHTCR sub-section : provided by CHTCR
C     ---------------------------------------------------------------
C
      INTEGER CHTCR_COUNT    (CHTCR_MAX,
     +                        EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C     Hot Tower counts after the 2nd tier
C     -----------------------------------
C
      INTEGER SCND_HOT_COUNT (SCND_HOT_MAX,
     +                        EM_ET_REF_MIN:TOT_ET_REF_MAX)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
      COMMON /L1C_INTERMEDIATE_COUNT/ CHTCR_INPUT,
     +                            CHTCR_COUNT,
     +                            SCND_HOT_COUNT
C
