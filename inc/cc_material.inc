C DEC/CMS REPLACEMENT HISTORY, Element CC_MATERIAL.INC
C *1     8-DEC-1988 14:43:45 FUESS "Initial entry of CC .INC files"
C DEC/CMS REPLACEMENT HISTORY, Element CC_MATERIAL.INC
C----------------------------------------------------------------------
C-
C-      /CC_MATERIAL_CHAR/
C-      /CC_MATERIAL/
C-
C-      Central Calorimeter GEANT SRCP Material descriptors
C-
C-   Created  28-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      INTEGER MAX_COMPONENTS
      PARAMETER ( MAX_COMPONENTS = 5 )
      CHARACTER*32 MATERIAL_LABEL
      INTEGER MATERIAL_NAME(3)
      INTEGER MATERIAL_CODE
      INTEGER NUMBER_COMPONENTS
      INTEGER COMPONENT_CODE(MAX_COMPONENTS)
      REAL COMPONENT_FRACTION(MAX_COMPONENTS)
      COMMON /CC_MATERIAL_CHAR/ MATERIAL_LABEL
      COMMON /CC_MATERIAL/ MATERIAL_NAME,
     &                     MATERIAL_CODE,
     &                     NUMBER_COMPONENTS,
     &                     COMPONENT_CODE,
     &                     COMPONENT_FRACTION
