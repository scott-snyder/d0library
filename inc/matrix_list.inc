C----------------------------------------------------------------------
C-
C-      /MATRIX_LIST/
C-      
C-   Created  10-OCT-1989   Stuart Fuess
C-
C-      Central Calorimeter GEANT Rotation Matrix list
C-
C-      Matrix values are: THETA1       Polar angle for axis I
C-                         PHI1         Azimuthal angle for axis I
C-                         THETA2       Polar angle for axis II
C-                         PHI2         Azimuthal angle for axis II
C-                         THETA3       Polar angle for axis III
C-                         PHI3         Azimuthal angle for axis III
C-      See GEANT manual for more complete description
C-
C----------------------------------------------------------------------
      INTEGER MAX_MATRICES
      PARAMETER ( MAX_MATRICES = 100 )
      CHARACTER*32 MATRIX_LIST_SRCP_LABEL
      INTEGER NUMBER_MATRICES                   
      INTEGER ID_MATRIX_LIST(MAX_MATRICES)      
      REAL VAL_MATRIX_LIST(6,MAX_MATRICES)      
      COMMON /MATRIX_LIST_CHAR/ MATRIX_LIST_SRCP_LABEL
      COMMON /MATRIX_LIST/ NUMBER_MATRICES,
     &                     ID_MATRIX_LIST,
     &                     VAL_MATRIX_LIST
