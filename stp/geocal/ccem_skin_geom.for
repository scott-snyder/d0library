      SUBROUTINE CCEM_SKIN_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute CCEM Skin geometry for GEANT
C-                         for use in plate level description
C-
C-            ______________________________
C-            |                            |
C-            | --                      -- |
C-            | ||                      || |         CCEM
C-            | ||                      || <----- Module volume
C-            | ||                      || |       filled with
C-            | ||                      || |      liquid argon
C-             | ||                    || |
C-             | ||                    || |
C-             | ||                 <----------- Plate volumes
C-             | ||                    || |       + endplates
C-              | ||                  || |         not shown
C-              | ||                  || |
C-              | ||                  |<----- Skin sideplates
C-               | ||                || |
C-               | --                -- |
C-               |______________________|
C-
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  14-DEC-1989   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCEM_MODULE.INC'
      INCLUDE 'D0$INC:MATRIX.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
C  Reals
      REAL    WIDTH
      REAL    LENGTH
      REAL    THICKNESS
      REAL    ANGLE
C  Characters
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
      REAL RAD
      PARAMETER (RAD=0.017453293)
C----------------------------------------------------------------------
C  Select the CC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'CENTRAL' )
C----------------------------------------------------------------------
C  Get the skin plate dimensions
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_SIDE_SKIN_WIDTH', WIDTH, IER )
      CALL EZGET ( 'CCEM_SIDE_SKIN_LENGTH', LENGTH, IER )
      CALL EZGET ( 'CCEM_SIDE_SKIN_THICKNESS', THICKNESS, IER )
C----------------------------------------------------------------------
C  Get the volume label for the left side skin
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_LEFT_SKIN_VOLUME_LABEL', 1, 
     &               VOLUME_LABEL, LEN, IER )
C----------------------------------------------------------------------
C  Get the volume name for the left side skin
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_LEFT_SKIN_VOLUME_NAME', VOLUME_NAME, IER )
C----------------------------------------------------------------------
C  Set volume shape
C----------------------------------------------------------------------
      CALL UCTOH ( 'BOX', VOLUME_SHAPE, 4, 3 )
C----------------------------------------------------------------------
C  Get material codes for skin
C----------------------------------------------------------------------
      CALL EZGET ( 'STAINLESS_STEEL_CODE', VOLUME_MATERIAL_CODE, IER )
C----------------------------------------------------------------------
C  Get volume mother name
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_PLATE_MODULE_VOLUME_NAME', VOLUME_MOTHER, IER)
C----------------------------------------------------------------------
C  Set positioning mode
C----------------------------------------------------------------------
      CALL UCTOH ( 'POS', POSITIONING, 4, 3 )
C----------------------------------------------------------------------
C  Get rotation matrix ID for left side skin
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_LEFT_SKIN_ROTATION_MATRIX', 
     &              ROTATION_MATRIX, IER )
C----------------------------------------------------------------------
C  Compute and store rotation matrix:
C  Set matrix ID
C----------------------------------------------------------------------
      ID_MATRIX = ROTATION_MATRIX
C----------------------------------------------------------------------
C  Determine angle of rotation
C----------------------------------------------------------------------
      ANGLE = ATAN ( 0.5 * 
     &       ( CCEM_MODULE_OUTER_WIDTH - CCEM_MODULE_INNER_WIDTH ) /
     &       ( CCEM_MODULE_OUTER_RADIUS - CCEM_MODULE_INNER_RADIUS )
     &       )/RAD
C----------------------------------------------------------------------
C  Set rotation parameters
C----------------------------------------------------------------------
      VAL_MATRIX(1) = - ANGLE
      VAL_MATRIX(2) = 0.0
      VAL_MATRIX(3) = 90.
      VAL_MATRIX(4) = 90.
      VAL_MATRIX(5) = -90. - ANGLE
      VAL_MATRIX(6) = 0.0
C----------------------------------------------------------------------
C  Store rotation matrix
C----------------------------------------------------------------------
      CALL STORE_MATRIX
C----------------------------------------------------------------------
C  Only a single copy
C----------------------------------------------------------------------
      COPY_NUMBER = 1
C----------------------------------------------------------------------
C  Determine the position of the left side skin
C----------------------------------------------------------------------
      X_POSITION = - CM_PER_INCH * ( 0.25 * CCEM_MODULE_OUTER_WIDTH +
     &                               0.25 * CCEM_MODULE_INNER_WIDTH -
     &                               0.5 * THICKNESS / COS(ANGLE*RAD))
      Y_POSITION = 0.0
      Z_POSITION = 0.0
C----------------------------------------------------------------------
C  Set the parameters of the volume
C----------------------------------------------------------------------
      NUMBER_PARAMS = 3
      PARAM(1) = 0.5 * CM_PER_INCH * WIDTH
      PARAM(2) = 0.5 * CM_PER_INCH * LENGTH
      PARAM(3) = 0.5 * CM_PER_INCH * THICKNESS
C----------------------------------------------------------------------
C  Write the left side skin volume description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Repeat as necessary for right side skin
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_RIGHT_SKIN_VOLUME_LABEL', 1, 
     &               VOLUME_LABEL, LEN, IER )
      CALL EZGET ( 'CCEM_RIGHT_SKIN_VOLUME_NAME', VOLUME_NAME, IER )
      CALL EZGET ( 'CCEM_RIGHT_SKIN_ROTATION_MATRIX', 
     &              ROTATION_MATRIX, IER )
      ID_MATRIX = ROTATION_MATRIX
      VAL_MATRIX(1) = ANGLE
      VAL_MATRIX(2) = 0.0
      VAL_MATRIX(3) = 90.
      VAL_MATRIX(4) = 90.
      VAL_MATRIX(5) = - 90. + ANGLE
      VAL_MATRIX(6) = 0.0
      CALL STORE_MATRIX
      X_POSITION = - X_POSITION
      CALL WRITE_VOLUME
      RETURN
      END
