C----------------------------------------------------------------------
C-
C-      /SCCCH_MODULE/
C-
C-      CCCH 'Module volume' geometrical properties
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      INTEGER CCCH_NUMBER_MODULES       
      REAL CCCH_MODULE_INNER_RADIUS     
      REAL CCCH_MODULE_OUTER_RADIUS     
      REAL CCCH_MODULE_INNER_WIDTH      
      REAL CCCH_MODULE_OUTER_WIDTH      
      REAL CCCH_MODULE_INNER_LENGTH     
      REAL CCCH_MODULE_OUTER_LENGTH     
      REAL CCCH_MODULE_VOLUME           
      INTEGER CCCH_MODULE_MATERIAL      
      COMMON /SCCCH_MODULE/ CCCH_NUMBER_MODULES,
     &                     CCCH_MODULE_INNER_RADIUS,
     &                     CCCH_MODULE_OUTER_RADIUS,
     &                     CCCH_MODULE_INNER_WIDTH, 
     &                     CCCH_MODULE_OUTER_WIDTH,
     &                     CCCH_MODULE_INNER_LENGTH,
     &                     CCCH_MODULE_OUTER_LENGTH,
     &                     CCCH_MODULE_VOLUME,
     &                     CCCH_MODULE_MATERIAL
