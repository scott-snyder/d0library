C----------------------------------------------------------------------
C-
C-      /SCCFH_MODULE/
C-
C-      CCFH 'Module volume' geometrical properties
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      INTEGER CCFH_NUMBER_MODULES       
      REAL CCFH_MODULE_INNER_RADIUS     
      REAL CCFH_MODULE_OUTER_RADIUS     
      REAL CCFH_MODULE_INNER_WIDTH      
      REAL CCFH_MODULE_OUTER_WIDTH      
      REAL CCFH_MODULE_LENGTH           
      REAL CCFH_MODULE_VOLUME           
      INTEGER CCFH_MODULE_MATERIAL      
      COMMON /SCCFH_MODULE/ CCFH_NUMBER_MODULES,
     &                     CCFH_MODULE_INNER_RADIUS,
     &                     CCFH_MODULE_OUTER_RADIUS,
     &                     CCFH_MODULE_INNER_WIDTH, 
     &                     CCFH_MODULE_OUTER_WIDTH,
     &                     CCFH_MODULE_LENGTH,
     &                     CCFH_MODULE_VOLUME,
     &                     CCFH_MODULE_MATERIAL
