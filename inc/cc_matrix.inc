C----------------------------------------------------------------------
C-
C-   Created  21-NOV-1988   Stuart Fuess
C-
C-      Central Calorimeter GEANT Rotation Matrix
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
      INTEGER ID_MATRIX                 
      REAL VAL_MATRIX(6)                
      COMMON /CC_MATRIX/ ID_MATRIX,
     &                   VAL_MATRIX
