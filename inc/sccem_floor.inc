C----------------------------------------------------------------------
C-
C-      /SCCEM_FLOOR/
C-
C-      CCEM 'Floor volume' geometrical properties
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCEM_FLOOR_INNER_RADIUS      
      REAL CCEM_FLOOR_OUTER_RADIUS      
      REAL CCEM_FLOOR_INNER_WIDTH       
      REAL CCEM_FLOOR_OUTER_WIDTH       
      REAL CCEM_FLOOR_INNER_LENGTH      
      REAL CCEM_FLOOR_OUTER_LENGTH      
      REAL CCEM_FLOOR_VOLUME            
      REAL CCEM_FLOOR_VOLUME_INSIDE(5)  
      REAL CCEM_FLOOR_VOLUME_EXCESS(5)  
      INTEGER CCEM_FLOOR_MATERIAL       
      COMMON /SCCEM_FLOOR/ CCEM_FLOOR_INNER_RADIUS,
     &                    CCEM_FLOOR_OUTER_RADIUS,
     &                    CCEM_FLOOR_INNER_WIDTH, 
     &                    CCEM_FLOOR_OUTER_WIDTH,
     &                    CCEM_FLOOR_INNER_LENGTH, 
     &                    CCEM_FLOOR_OUTER_LENGTH,
     &                    CCEM_FLOOR_VOLUME,
     &                    CCEM_FLOOR_VOLUME_INSIDE,
     &                    CCEM_FLOOR_VOLUME_EXCESS,
     &                    CCEM_FLOOR_MATERIAL
