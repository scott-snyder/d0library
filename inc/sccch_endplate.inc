C----------------------------------------------------------------------
C-
C-      /SCCCH_ENDPLATE/
C-
C-      CCCH 'Endplate volume' geometrical properties
C-
C-   Created   7-NOV-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCCH_ENDPLATE_INNER_WIDTH    
      REAL CCCH_ENDPLATE_OUTER_WIDTH    
      REAL CCCH_ENDPLATE_HEIGHT         
      REAL CCCH_ENDPLATE_THICKNESS      
      REAL CCCH_ENDPLATE_TILT           
      REAL CCCH_ENDPLATE_POSITION       
      REAL CCCH_ENDPLATE_VOLUME         
      INTEGER CCCH_ENDPLATE_MATERIAL    
      COMMON /SCCCH_ENDPLATE/ CCCH_ENDPLATE_INNER_WIDTH,
     &                       CCCH_ENDPLATE_OUTER_WIDTH,
     &                       CCCH_ENDPLATE_HEIGHT,
     &                       CCCH_ENDPLATE_THICKNESS,
     &                       CCCH_ENDPLATE_TILT,
     &                       CCCH_ENDPLATE_POSITION,
     &                       CCCH_ENDPLATE_VOLUME,
     &                       CCCH_ENDPLATE_MATERIAL
