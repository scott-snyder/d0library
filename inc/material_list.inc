C----------------------------------------------------------------------
C-
C-      COMMON /MATERIAL_LIST_CHAR/
C-      COMMON /MATERIAL_LIST/ 
C-
C-      Materials for GEANT
C-      Used in STP geometry programs
C-
C-   Created  10-Oct-1989   Stuart Fuess 
C-   Updated  27-MAR-1990   Stuart Fuess      Increase dimension size to 50
C-   Updated  22-JUL-1991   Marcel Demarteau  Increase dimension size to 200
C----------------------------------------------------------------------
      INTEGER MAX_MATERIALS
      INTEGER MAX_COMPONENTS_LIST
      PARAMETER ( MAX_MATERIALS = 200 )
      PARAMETER ( MAX_COMPONENTS_LIST = 5 )
      CHARACTER*32 MATERIAL_LIST_SRCP_LABEL
      CHARACTER*32 MATERIAL_LABEL_LIST(MAX_MATERIALS)
      INTEGER NUMBER_MATERIALS
      INTEGER MATERIAL_NAME_LIST(3,MAX_MATERIALS)
      INTEGER MATERIAL_CODE_LIST(MAX_MATERIALS)
      INTEGER NUMBER_COMPONENTS_LIST(MAX_MATERIALS)
      INTEGER COMPONENT_CODE_LIST(MAX_COMPONENTS_LIST,
     &                            MAX_MATERIALS)
      REAL COMPONENT_FRACTION_LIST(MAX_COMPONENTS_LIST,
     &                             MAX_MATERIALS)
      COMMON /MATERIAL_LIST_CHAR/  MATERIAL_LIST_SRCP_LABEL,
     &                             MATERIAL_LABEL_LIST
      COMMON /MATERIAL_LIST/ NUMBER_MATERIALS,
     &                       MATERIAL_NAME_LIST,
     &                       MATERIAL_CODE_LIST,
     &                       NUMBER_COMPONENTS_LIST,
     &                       COMPONENT_CODE_LIST,
     &                       COMPONENT_FRACTION_LIST
