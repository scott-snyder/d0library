C----------------------------------------------------------------------
C-
C-      /SCCCH_FLOOR/
C-
C-      CCCH 'Floor volume' geometrical properties
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCCH_FLOOR_INNER_RADIUS      
      REAL CCCH_FLOOR_OUTER_RADIUS      
      REAL CCCH_FLOOR_INNER_WIDTH       
      REAL CCCH_FLOOR_OUTER_WIDTH       
      REAL CCCH_FLOOR_INNER_LENGTH      
      REAL CCCH_FLOOR_OUTER_LENGTH      
      REAL CCCH_FLOOR_VOLUME            
      REAL CCCH_FLOOR_VOLUME_INSIDE(5)  
      REAL CCCH_FLOOR_VOLUME_EXCESS(5)  
      INTEGER CCCH_FLOOR_MATERIAL       
      COMMON /SCCCH_FLOOR/ CCCH_FLOOR_INNER_RADIUS,
     &                    CCCH_FLOOR_OUTER_RADIUS,
     &                    CCCH_FLOOR_INNER_WIDTH, 
     &                    CCCH_FLOOR_OUTER_WIDTH,
     &                    CCCH_FLOOR_INNER_LENGTH, 
     &                    CCCH_FLOOR_OUTER_LENGTH,
     &                    CCCH_FLOOR_VOLUME,
     &                    CCCH_FLOOR_VOLUME_INSIDE,
     &                    CCCH_FLOOR_VOLUME_EXCESS,
     &                    CCCH_FLOOR_MATERIAL
