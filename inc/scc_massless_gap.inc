C----------------------------------------------------------------------
C-
C-      /SCC_MASSLESS_GAP/
C-
C-      CC Massless Gap Volume parameters
C-
C-      The volume will be a 'TUBE' filled with liquid argon.
C-
C-
C-   Created  28-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CC_MASSLESS_GAP_INNER_RADIUS         
      REAL CC_MASSLESS_GAP_OUTER_RADIUS         
      REAL CC_MASSLESS_GAP_LENGTH               
      REAL CC_MASSLESS_GAP_POSITION             
      INTEGER CC_MASSLESS_GAP_MATERIAL          
      COMMON /SCC_MASSLESS_GAP/ CC_MASSLESS_GAP_INNER_RADIUS,
     &                         CC_MASSLESS_GAP_OUTER_RADIUS,
     &                         CC_MASSLESS_GAP_LENGTH,
     &                         CC_MASSLESS_GAP_POSITION,
     &                         CC_MASSLESS_GAP_MATERIAL
