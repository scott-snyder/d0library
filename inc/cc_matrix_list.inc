C DEC/CMS REPLACEMENT HISTORY, Element CC_MATRIX_LIST.INC
C *1     8-DEC-1988 14:44:51 FUESS "Initial entry of CC .INC files"
C DEC/CMS REPLACEMENT HISTORY, Element CC_MATRIX_LIST.INC
C----------------------------------------------------------------------
C-
C-   Created  21-NOV-1988   Stuart Fuess
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
      INTEGER CC_MAX_MATRICES
      PARAMETER ( CC_MAX_MATRICES = 100 )
      INTEGER CC_NUMBER_MATRICES                
      INTEGER CC_ID_MATRIX(CC_MAX_MATRICES)     
      REAL CC_VAL_MATRIX(6,CC_MAX_MATRICES)     
      COMMON /CC_MATRIX_LIST/ CC_NUMBER_MATRICES,
     &                        CC_ID_MATRIX,
     &                        CC_VAL_MATRIX
