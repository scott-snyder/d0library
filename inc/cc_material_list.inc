C DEC/CMS REPLACEMENT HISTORY, Element CC_MATERIAL_LIST.INC
C *1     8-DEC-1988 14:44:18 FUESS "Initial entry of CC .INC files"
C DEC/CMS REPLACEMENT HISTORY, Element CC_MATERIAL_LIST.INC
C----------------------------------------------------------------------
C-
C-      COMMON /CC_MATERIAL_LIST_CHAR/
C-      COMMON /CC_MATERIAL_LIST/ 
C-
C-      Central Calorimeter Materials
C-
C-   Created  30-NOV-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      INTEGER CC_MAX_MATERIALS
      INTEGER CC_MAX_COMPONENTS
      PARAMETER ( CC_MAX_MATERIALS = 20 )
      PARAMETER ( CC_MAX_COMPONENTS = 5 )
      CHARACTER*32 CC_MATERIAL_LABEL(CC_MAX_MATERIALS)
      INTEGER CC_NUMBER_MATERIALS
      INTEGER CC_MATERIAL_NAME(3,CC_MAX_MATERIALS)
      INTEGER CC_MATERIAL_CODE(CC_MAX_MATERIALS)
      INTEGER CC_NUMBER_COMPONENTS(CC_MAX_MATERIALS)
      INTEGER CC_COMPONENT_CODE(CC_MAX_COMPONENTS,
     &                            CC_MAX_MATERIALS)
      REAL CC_COMPONENT_FRACTION(CC_MAX_COMPONENTS,
     &                             CC_MAX_MATERIALS)
      COMMON /CC_MATERIAL_LIST_CHAR/ CC_MATERIAL_LABEL
      COMMON /CC_MATERIAL_LIST/ CC_MATERIAL_NAME,
     &                          CC_MATERIAL_CODE,
     &                          CC_NUMBER_COMPONENTS,
     &                          CC_COMPONENT_CODE,
     &                          CC_COMPONENT_FRACTION
