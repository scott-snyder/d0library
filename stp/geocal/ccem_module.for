      SUBROUTINE CCEM_MODULE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the CCEM module GEANT volumes
C-
C-      The CCEM module volumes will be repetitively positioned
C-      inside the CCEM mother volume.  The CCEM floor volumes
C-      and the CCEM endplate volumes will be positioned inside
C-      of the CCEM module volumes.  Material which is outside of
C-      the floor and endplate volumes will be mixed to produce
C-      the 'crack' material with which the module volume will
C-      be filled.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-OCT-1988   Stuart Fuess
C-   Updated  13-JAN-1989   Stuart Fuess  Simplify structure 
C-   Updated   4-FEB-1990   Stuart Fuess  Use EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCEM_MODULE.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
C  Integers
      INTEGER IER
      INTEGER MODULE
      INTEGER FIRST_ID
C  Reals
      REAL RADIUS
      REAL OFFSET
      REAL ANGLE
      REAL ANGLE_SIZE
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
      REAL RAD
      PARAMETER (RAD=0.017453293)
C----------------------------------------------------------------------
C  Get module dimensions from endplate dimensions and positions
C----------------------------------------------------------------------
      CALL EZGET ('CCEM_MODULE_INNER_RADIUS',
     &             CCEM_MODULE_INNER_RADIUS,IER)
      CALL EZGET ('CCEM_MODULE_OUTER_RADIUS',
     &             CCEM_MODULE_OUTER_RADIUS,IER)
      CALL EZGET ('CCEM_MODULE_INNER_WIDTH',
     &             CCEM_MODULE_INNER_WIDTH,IER)
      CALL EZGET ('CCEM_MODULE_OUTER_WIDTH',
     &             CCEM_MODULE_OUTER_WIDTH,IER)
      CALL EZGET ('CCEM_MODULE_LENGTH',
     &             CCEM_MODULE_LENGTH,IER)
C----------------------------------------------------------------------
C  Compute module volume
C----------------------------------------------------------------------
      CCEM_MODULE_VOLUME = 0.5 * (CCEM_MODULE_INNER_WIDTH +
     &                            CCEM_MODULE_OUTER_WIDTH) *
     &                           (CCEM_MODULE_OUTER_RADIUS -
     &                            CCEM_MODULE_INNER_RADIUS) *
     &                            CCEM_MODULE_LENGTH
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for all modules
C----------------------------------------------------------------------
      CALL EZGET('CCEM_NUMBER_MODULES',CCEM_NUMBER_MODULES,IER)
      WRITE(OUT_VOL,1000) CCEM_NUMBER_MODULES
C----------------------------------------------------------------------
C  Parameters for module volume
C----------------------------------------------------------------------
      CALL EZGET('CCEM_MODULE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL UCTOH('TRD1',VOLUME_SHAPE,4,4)
      CALL EZGET('CCEM_CRACK_MATERIAL_CODE',VOLUME_MATERIAL_CODE,IER)
      CALL EZGET('CCEM_MOTHER_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH('POS',POSITIONING,4,3)
      Z_POSITION        = 0.0
      NUMBER_PARAMS     = 4
      PARAM(1)          = 0.5 * CM_PER_INCH * CCEM_MODULE_INNER_WIDTH
      PARAM(2)          = 0.5 * CM_PER_INCH * CCEM_MODULE_OUTER_WIDTH
      PARAM(3)          = 0.5 * CM_PER_INCH * CCEM_MODULE_LENGTH
      PARAM(4)          = 0.5 * CM_PER_INCH *
     &          (CCEM_MODULE_OUTER_RADIUS - CCEM_MODULE_INNER_RADIUS)
C----------------------------------------------------------------------
C  Get the rotation matrix ID for the first CCEM module
C----------------------------------------------------------------------
      CALL EZGET('CCEM_FIRST_ROTATION_MATRIX',FIRST_ID,IER)
C----------------------------------------------------------------------
C  Get the angular offset for the first CCEM module
C----------------------------------------------------------------------
      CALL EZGET('CCEM_ANGULAR_OFFSET',OFFSET,IER)
C----------------------------------------------------------------------
C  Set angular size of each module
C----------------------------------------------------------------------
      ANGLE_SIZE = 360. / CCEM_NUMBER_MODULES
C----------------------------------------------------------------------
C  Set the radius of the module mid-point
C----------------------------------------------------------------------
      RADIUS = 0.5 * (CCEM_MODULE_INNER_RADIUS + 
     &                CCEM_MODULE_OUTER_RADIUS)
C----------------------------------------------------------------------
C  Position multiple copies of the CCEM module volume
C----------------------------------------------------------------------
      ROTATION_MATRIX = FIRST_ID - 1
      DO MODULE=1,CCEM_NUMBER_MODULES
        WRITE(VOLUME_LABEL,1001) MODULE
        ROTATION_MATRIX = ROTATION_MATRIX + 1
        COPY_NUMBER = MODULE
        ANGLE = (MODULE-1) * ANGLE_SIZE + OFFSET
        X_POSITION = CM_PER_INCH * RADIUS * COS((ANGLE)*RAD)
        Y_POSITION = CM_PER_INCH * RADIUS * SIN((ANGLE)*RAD)
        CALL WRITE_VOLUME
      ENDDO
      RETURN
 1000 FORMAT('CCEM_NUMBER_MODULES',T40,I2)
 1001 FORMAT('CCEM_',I2.2,'_VOLUME')
      END
