C----------------------------------------------------------------------
C-
C-      /SCCCH_MOTHER/
C-
C-      CCCH Mother Volume parameters
C-
C-      The mother volume will be two 'PCONs' with inner radius given
C-      by the FH/CH boundary, outer radius given by the cryostat
C-      outer cold wall inner radius, lengths given by the CH module
C-      lengths, and filled with liquid argon.
C-
C-
C-   Created  28-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCCH_MOTHER_INNER_RADIUS     
      REAL CCCH_MOTHER_OUTER_RADIUS     
      REAL CCCH_MOTHER_INNER_LENGTH     
      REAL CCCH_MOTHER_OUTER_LENGTH     
      INTEGER CCCH_MOTHER_MATERIAL      
      COMMON /SCCCH_MOTHER/ CCCH_MOTHER_INNER_RADIUS,
     &                     CCCH_MOTHER_OUTER_RADIUS,
     &                     CCCH_MOTHER_INNER_LENGTH,
     &                     CCCH_MOTHER_OUTER_LENGTH,
     &                     CCCH_MOTHER_MATERIAL
