C----------------------------------------------------------------------
C-
C-      /SRCP_VOLUME_CHAR/
C-      /SRCP_VOLUME/
C-
C-      GEANT SRCP Volume descriptors
C-
C-   Created  11-OCT-1989   Stuart Fuess
C-
C----------------------------------------------------------------------
      INTEGER MAX_PARAMS
      PARAMETER ( MAX_PARAMS = 20 )
      CHARACTER*32 VOLUME_LABEL
      INTEGER VOLUME_NAME
      INTEGER VOLUME_SHAPE
      INTEGER VOLUME_MATERIAL_CODE
      INTEGER VOLUME_MOTHER
      INTEGER POSITIONING
      INTEGER ROTATION_MATRIX
      INTEGER COPY_NUMBER
      REAL X_POSITION
      REAL Y_POSITION
      REAL Z_POSITION
      INTEGER NUMBER_PARAMS
      REAL PARAM(MAX_PARAMS)
      COMMON /SRCP_VOLUME_CHAR/ VOLUME_LABEL
      COMMON /SRCP_VOLUME/ VOLUME_NAME,
     &                     VOLUME_SHAPE, 
     &                     VOLUME_MATERIAL_CODE, 
     &                     VOLUME_MOTHER, 
     &                     POSITIONING,
     &                     ROTATION_MATRIX,
     &                     COPY_NUMBER, 
     &                     X_POSITION, 
     &                     Y_POSITION, 
     &                     Z_POSITION, 
     &                     NUMBER_PARAMS, 
     &                     PARAM
