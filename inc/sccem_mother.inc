C----------------------------------------------------------------------
C-
C-      /SCCEM_MOTHER/
C-
C-      CCEM Mother Volume parameters
C-
C-      The mother volume will be a 'TUBE' with inner radius given
C-      by the outer radius of the cryostat inner cold wall, outer
C-      radius given by the EM/FH boundary, length given by the
C-      EM module lengths, and filled with liquid argon.
C-
C-
C-   Created  28-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCEM_MOTHER_INNER_RADIUS     
      REAL CCEM_MOTHER_OUTER_RADIUS     
      REAL CCEM_MOTHER_LENGTH           
      INTEGER CCEM_MOTHER_MATERIAL      
      COMMON /SCCEM_MOTHER/ CCEM_MOTHER_INNER_RADIUS,
     &                     CCEM_MOTHER_OUTER_RADIUS,
     &                     CCEM_MOTHER_LENGTH,
     &                     CCEM_MOTHER_MATERIAL
