C----------------------------------------------------------------------
C-
C-      /MATERIAL_CHAR/
C-      /MATERIAL/
C-
C-      GEANT SRCP Material descriptors
C-
C-   Created  10-OCT-1989   Stuart Fuess
C-
C----------------------------------------------------------------------
      INTEGER MAX_COMPONENTS
      PARAMETER ( MAX_COMPONENTS = 5 )
      CHARACTER*32 MATERIAL_SRCP_LABEL
      CHARACTER*32 MATERIAL_LABEL
      INTEGER MATERIAL_NAME(3)
      INTEGER MATERIAL_CODE
      INTEGER NUMBER_COMPONENTS
      INTEGER COMPONENT_CODE(MAX_COMPONENTS)
      REAL COMPONENT_FRACTION(MAX_COMPONENTS)
      COMMON /MATERIAL_CHAR/ MATERIAL_SRCP_LABEL,
     &                       MATERIAL_LABEL
      COMMON /MATERIAL/ MATERIAL_NAME,
     &                  MATERIAL_CODE,
     &                  NUMBER_COMPONENTS,
     &                  COMPONENT_CODE,
     &                  COMPONENT_FRACTION
