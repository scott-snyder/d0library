C----------------------------------------------------------------------
C-
C-      /SCCEM_MODULE/
C-
C-      CCEM 'Module volume' geometrical properties
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      INTEGER CCEM_NUMBER_MODULES       
      REAL CCEM_MODULE_INNER_RADIUS     
      REAL CCEM_MODULE_OUTER_RADIUS     
      REAL CCEM_MODULE_INNER_WIDTH      
      REAL CCEM_MODULE_OUTER_WIDTH      
      REAL CCEM_MODULE_LENGTH           
      REAL CCEM_MODULE_VOLUME           
      INTEGER CCEM_MODULE_MATERIAL      
      COMMON /SCCEM_MODULE/ CCEM_NUMBER_MODULES,
     &                     CCEM_MODULE_INNER_RADIUS,
     &                     CCEM_MODULE_OUTER_RADIUS,
     &                     CCEM_MODULE_INNER_WIDTH, 
     &                     CCEM_MODULE_OUTER_WIDTH,
     &                     CCEM_MODULE_LENGTH,
     &                     CCEM_MODULE_VOLUME,
     &                     CCEM_MODULE_MATERIAL
