C DEC/CMS REPLACEMENT HISTORY, Element CC_SRCP_VOLUME.INC
C *1     8-DEC-1988 14:45:30 FUESS "Initial entry of CC .INC files"
C DEC/CMS REPLACEMENT HISTORY, Element CC_SRCP_VOLUME.INC
C----------------------------------------------------------------------
C-
C-      /CC_SRCP_VOLUME_CHAR/
C-      /CC_SRCP_VOLUME/
C-
C-      Central Calorimeter GEANT SRCP Volume descriptors
C-
C-   Created  28-OCT-1988   Stuart Fuess
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
      COMMON /CC_SRCP_VOLUME_CHAR/ VOLUME_LABEL
      COMMON /CC_SRCP_VOLUME/ VOLUME_NAME,
     &                        VOLUME_SHAPE, 
     &                        VOLUME_MATERIAL_CODE, 
     &                        VOLUME_MOTHER, 
     &                        POSITIONING,
     &                        ROTATION_MATRIX,
     &                        COPY_NUMBER, 
     &                        X_POSITION, 
     &                        Y_POSITION, 
     &                        Z_POSITION, 
     &                        NUMBER_PARAMS, 
     &                        PARAM
