C----------------------------------------------------------------------
C-
C-      /SCCFH_FLOOR/
C-
C-      CCFH 'Floor volume' geometrical properties
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCFH_FLOOR_INNER_RADIUS      
      REAL CCFH_FLOOR_OUTER_RADIUS      
      REAL CCFH_FLOOR_INNER_WIDTH       
      REAL CCFH_FLOOR_OUTER_WIDTH       
      REAL CCFH_FLOOR_INNER_LENGTH      
      REAL CCFH_FLOOR_OUTER_LENGTH      
      REAL CCFH_FLOOR_VOLUME            
      REAL CCFH_FLOOR_VOLUME_INSIDE(5)  
      REAL CCFH_FLOOR_VOLUME_EXCESS(5)  
      INTEGER CCFH_FLOOR_MATERIAL       
      COMMON /SCCFH_FLOOR/ CCFH_FLOOR_INNER_RADIUS,
     &                    CCFH_FLOOR_OUTER_RADIUS,
     &                    CCFH_FLOOR_INNER_WIDTH, 
     &                    CCFH_FLOOR_OUTER_WIDTH,
     &                    CCFH_FLOOR_INNER_LENGTH, 
     &                    CCFH_FLOOR_OUTER_LENGTH,
     &                    CCFH_FLOOR_VOLUME,
     &                    CCFH_FLOOR_VOLUME_INSIDE,
     &                    CCFH_FLOOR_VOLUME_EXCESS,
     &                    CCFH_FLOOR_MATERIAL
