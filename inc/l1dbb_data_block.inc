C----------------------------------------------------------------------
C-
C-                      LEVEL1_TRIGGER_DATA_BLOCK
C-                      =========================
C-
C-   Created   8-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  19-MAR-1990   Sylvain Tisserant (MSU) : Only INTEGER declarations
C-   Updated  24-MAY-1990   Sylvain Tisserant (MSU) : New format.
C-   Updated  14-JUN-1990   Sylvain Tisserant (MSU) : Back new format.
C-                          (Cf : D0 Note 967)
C-   Updated   9-OCT-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-                          Moved parameter definitions to .PARAMS file
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              LEVEL1_TRIGGER_DATA_BLOCK to L1DBB_DATA_BLOCK.  
C-   Updated   7-JAN-1992   Philippe Laurens, Steven Klocek  
C-                              Added crate header and trailer, word count
C-
C----------------------------------------------------------------------
C
C     Level 1 Trigger Data Block
C     ==========================
C
      INTEGER LVL1_DATA_BLOCK (DATA_BLOCK_MAX)
C
C----------------------------------------------------------------------
C
C     Crate header and trailer
C     ========================
C
      INTEGER L1_CRATE_HEADER(TRGR_HEADER_LENGTH)
      INTEGER L1_CRATE_TRAILER(TRGR_TRAILER_LENGTH)
      INTEGER L1_WORD_COUNT
      INTEGER CHECKSUM
      EQUIVALENCE (CHECKSUM, L1_CRATE_TRAILER(TRGR_TRAILER_LENGTH))
C-------------------------------------------------------------------------------
C
C     Jet List Length and Algorithm Revision Number
C     =============================================
C
      INTEGER    JET_LENGTH, REVISION_NUMBER
C
C-------------------------------------------------------------------------------
C
C     EQUIVALENCES
C     ============
C
      INTEGER      JET_PATTERN (ETA_MIN:ETA_MAX,
     +                          POS_ETA:NEG_ETA,
     +                          EM_ET_REF_MIN:TOT_ET_REF_MAX)
      EQUIVALENCE (JET_PATTERN, LVL1_DATA_BLOCK(I_JET_PATTERN))
C
      INTEGER      REF_MASK (EM_ET_REF_MIN:TOT_ET_REF_MAX)
      EQUIVALENCE (REF_MASK, LVL1_DATA_BLOCK(I_JET_FIRED))
C
C-------------------------------------------------------------------------------
C
      COMMON /L1DBB_DATA_BLOCK/          L1_CRATE_HEADER,
     &                                   L1_WORD_COUNT,
     &                                   LVL1_DATA_BLOCK,
     &                                   L1_CRATE_TRAILER,
     +                                   JET_LENGTH, REVISION_NUMBER
C
