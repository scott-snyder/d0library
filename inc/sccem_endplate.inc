C----------------------------------------------------------------------
C-
C-      /SCCEM_ENDPLATE/
C-
C-      CCEM 'Endplate volume' geometrical properties
C-
C-   Created   7-NOV-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCEM_ENDPLATE_INNER_WIDTH    
      REAL CCEM_ENDPLATE_OUTER_WIDTH    
      REAL CCEM_ENDPLATE_HEIGHT         
      REAL CCEM_ENDPLATE_THICKNESS      
      REAL CCEM_ENDPLATE_POSITION       
      REAL CCEM_ENDPLATE_VOLUME         
      INTEGER CCEM_ENDPLATE_MATERIAL    
      COMMON /SCCEM_ENDPLATE/ CCEM_ENDPLATE_INNER_WIDTH,
     &                       CCEM_ENDPLATE_OUTER_WIDTH,
     &                       CCEM_ENDPLATE_HEIGHT,
     &                       CCEM_ENDPLATE_THICKNESS,
     &                       CCEM_ENDPLATE_POSITION,
     &                       CCEM_ENDPLATE_VOLUME,
     &                       CCEM_ENDPLATE_MATERIAL
