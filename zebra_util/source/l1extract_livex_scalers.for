      SUBROUTINE L1EXTRACT_LIVEX_SCALERS(L1_BLOCK,
     &  GLOBAL_LIVEX_SCALER, LIVEX_PER_BUNCH,
     &  LIVEX_FASTZ_GOOD_PER_BUNCH, LIVEX_L0SINGLE_PER_BUNCH,
     &  LIVEX_L0CENTER_PER_BUNCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract from the Level 1 Datablock the Live Crossing
C-                         Scalers. This data is only relevent for real data
C-                         and when sptrg #30 has been programmed with the
C-                         proper beam quality andor terms and other
C-                         requirements.
C-                         
C-                         Each scaler is 40 bits in size and is returned
C-                         in the following format:
C-                         INTEGER SCALER(2)
C-
C-                         SCALER(1) holds the low  24 bits
C-                         SCALER(2) holds the high 16 bits
C-                         (unused bits are padded with zero)
C-      
C-   Inputs  : L1_BLOCK 
C-
C-             First word of the Level-1 Crate header in the TRGR bank.
C-          -> Use IQ( LTRGR_LEVEL1 )
C-             Where LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             cf. header of D0$ZEBRA_UTIL$SOURCE:GZFIND_CRATE.FOR for details
C-
C-           +------------------------------------------------------------+
C-           | You must pass the variable IQ(LTRGR_LEVEL1)   DIRECTLY     |
C-           | and NOT A COPY of it.                                      |
C-           |------------------------------------------------------------|
C-           | YES :   L1EXTRACT_LIVEX_SCALERS ( IQ(LTRGR_LEVEL1),... )   |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_LIVEX_SCALERS ( L1_BLOCK, ... )          |
C-           +------------------------------------------------------------+
C- 
C-   Outputs : 
C-
C-             GLOBAL_LIVEX_SCALER      The global live crossing signal scaler.
C-
C-             LIVEX_PER_BUNCH          The live crossing signal projected onto
C-                                        the six bunches in the collider.
C-                                      
C-             LIVEX_FASTZ_GOOD_PER_BUNCH The signal (Live Crossing * Level 0
C-                                          Fast Z Good) projected onto the six
C-                                          bunches in the collider.
C-                                      
C-             LIVEX_L0SINGLE_PER_BUNCH   The signal (Live Crossing * Level 0
C-                                          single vertex) projected onto the
C-                                          six bunches in the collider.
C-                                      
C-             LIVEX_L0CENTER_PER_BUNCH   The signal (Live Crossing * Level 0
C-                                          single vertex and center) projected
C-                                          onto the six bunches in the
C-                                          collider.
C-             
C-   Controls: none
C-
C-   Created   4-SEP-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER L1_BLOCK(0:*)
      INTEGER GLOBAL_LIVEX_SCALER(2) 
      INTEGER LIVEX_PER_BUNCH(2, 6)
      INTEGER LIVEX_FASTZ_GOOD_PER_BUNCH(2, 6)
      INTEGER LIVEX_L0SINGLE_PER_BUNCH(2, 6)
      INTEGER LIVEX_L0CENTER_PER_BUNCH(2, 6)
C
      INTEGER EXPOS_SCALER(2, 0:31)
C
      INTEGER BUNCH
C
C       Arrays to describe the mapping from per bunch quantities to foreign
C         scaler numbers. For more information on foreign scalers, see D0 Note
C         967 Rev B.
C
      INTEGER FOREIGN_LIVEX(6)
      INTEGER FOREIGN_LIVEX_L0(6)
      INTEGER FOREIGN_LIVEX_L0S(6)
      INTEGER FOREIGN_LIVEX_L0CENTER(6)
C
      INTEGER GLOBAL_LIVEX_SPECTRIG_NUM
      PARAMETER (GLOBAL_LIVEX_SPECTRIG_NUM = 30)
C
      DATA FOREIGN_LIVEX_L0CENTER / 25, 21, 17, 13, 9, 5 /
      DATA FOREIGN_LIVEX_L0 / 26, 22, 18, 14, 10, 6 /
      DATA FOREIGN_LIVEX_L0S / 27, 23, 19, 15, 11, 7 /
      DATA FOREIGN_LIVEX / 28, 24, 20, 16, 12, 8 /
C
      DO BUNCH = 1, 6
        CALL L1UTIL_GET_FOREIGN_SCALER( L1_BLOCK, FOREIGN_LIVEX(BUNCH),
     &                                 LIVEX_PER_BUNCH(1, BUNCH) )
        CALL L1UTIL_GET_FOREIGN_SCALER( L1_BLOCK,
     &                                 FOREIGN_LIVEX_L0(BUNCH),
     &                                 LIVEX_FASTZ_GOOD_PER_BUNCH(1,
     &                                   BUNCH) )
        CALL L1UTIL_GET_FOREIGN_SCALER( L1_BLOCK,
     &                             FOREIGN_LIVEX_L0S(BUNCH),
     &                             LIVEX_L0SINGLE_PER_BUNCH(1, BUNCH) )
        CALL L1UTIL_GET_FOREIGN_SCALER( L1_BLOCK,
     &                             FOREIGN_LIVEX_L0CENTER(BUNCH),
     &                             LIVEX_L0CENTER_PER_BUNCH(1, BUNCH) )
      END DO
C
      CALL L1EXTRACT_SPTRG_EXPOS_SCALERS(L1_BLOCK, EXPOS_SCALER)
      GLOBAL_LIVEX_SCALER(1)=EXPOS_SCALER(1, GLOBAL_LIVEX_SPECTRIG_NUM)
      GLOBAL_LIVEX_SCALER(2)=EXPOS_SCALER(2, GLOBAL_LIVEX_SPECTRIG_NUM)
C----------------------------------------------------------------------
  999 RETURN
      END
