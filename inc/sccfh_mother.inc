C----------------------------------------------------------------------
C-
C-      /SCCFH_MOTHER/
C-
C-      CCFH Mother Volume parameters
C-
C-      The mother volume will be a 'TUBE' with inner radius given
C-      by the EM/FH boundary, outer radius given by the FH/CH
C-      boundary, length given by the FH module lengths, and filled
C-      with liquid argon.
C-
C-
C-   Created  28-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCFH_MOTHER_INNER_RADIUS     
      REAL CCFH_MOTHER_OUTER_RADIUS     
      REAL CCFH_MOTHER_LENGTH           
      INTEGER CCFH_MOTHER_MATERIAL      
      COMMON /SCCFH_MOTHER/ CCFH_MOTHER_INNER_RADIUS,
     &                     CCFH_MOTHER_OUTER_RADIUS,
     &                     CCFH_MOTHER_LENGTH,
     &                     CCFH_MOTHER_MATERIAL
