      SUBROUTINE CCCH_MODULE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the CCCH module GEANT volumes
C-
C-      The CCCH module volumes will be repetitively positioned
C-      inside the CCCH mother volume.  The CCCH floor volumes
C-      and the CCCH endplate volumes will be positioned inside
C-      of the CCCH module volumes.  Material which is outside of
C-      the floor and endplate volumes will be mixed to produce
C-      the 'crack' material with which the module volume will
C-      be filled.  
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-OCT-1988   Stuart Fuess
C-   Updated   9-JAN-1989   Stuart Fuess  Simplify structure, 
C-                                        add Main Ring
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCCH_MODULE.INC'
      INCLUDE 'D0$INC:SCCCH_MR_BEAMPIPE.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
C  Integers
      INTEGER IER
      INTEGER MODULE
      INTEGER FIRST_ID
      INTEGER MODULE_VOLUME_NAME
      INTEGER MR_MODULE_VOLUME_NAME
      INTEGER MR_MODULE
      INTEGER COUNTER
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
      CALL EZGET ('CCCH_MODULE_INNER_RADIUS',
     &             CCCH_MODULE_INNER_RADIUS,IER)
      CALL EZGET ('CCCH_MODULE_OUTER_RADIUS',
     &             CCCH_MODULE_OUTER_RADIUS,IER)
      CALL EZGET ('CCCH_MODULE_INNER_WIDTH',
     &             CCCH_MODULE_INNER_WIDTH,IER)
      CALL EZGET ('CCCH_MODULE_OUTER_WIDTH',
     &             CCCH_MODULE_OUTER_WIDTH,IER)
      CALL EZGET ('CCCH_MODULE_INNER_LENGTH',
     &             CCCH_MODULE_INNER_LENGTH,IER)
      CALL EZGET ('CCCH_MODULE_OUTER_LENGTH',
     &             CCCH_MODULE_OUTER_LENGTH,IER)
C----------------------------------------------------------------------
C  Compute module volume
C----------------------------------------------------------------------
      CCCH_MODULE_VOLUME = 0.25 * (CCCH_MODULE_INNER_WIDTH +
     &                             CCCH_MODULE_OUTER_WIDTH) *
     &                            (CCCH_MODULE_OUTER_RADIUS -
     &                             CCCH_MODULE_INNER_RADIUS) *
     &                            (CCCH_MODULE_INNER_LENGTH +
     &                             CCCH_MODULE_OUTER_LENGTH)
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for all modules
C----------------------------------------------------------------------
      CALL EZGET('CCCH_NUMBER_MODULES',CCCH_NUMBER_MODULES,IER)
      WRITE(OUT_VOL,1000) CCCH_NUMBER_MODULES
C----------------------------------------------------------------------
C  Parameters for CCCH module volume
C----------------------------------------------------------------------
      CALL EZGET('CCCH_MODULE_VOLUME_NAME',MODULE_VOLUME_NAME,IER)
      CALL EZGET('CCCH_MR_MODULE_VOLUME_NAME',
     &            MR_MODULE_VOLUME_NAME,IER)
      CALL UCTOH('TRD2',VOLUME_SHAPE,4,4)
      CALL EZGET('CCCH_CRACK_MATERIAL_CODE',VOLUME_MATERIAL_CODE,IER)
      CALL EZGET('CCCH_MOTHER_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH('POS',POSITIONING,4,3)
      Z_POSITION    = 0.0
      NUMBER_PARAMS = 5
      PARAM(1)      = 0.5 * CM_PER_INCH * CCCH_MODULE_INNER_WIDTH
      PARAM(2)      = 0.5 * CM_PER_INCH * CCCH_MODULE_OUTER_WIDTH
      PARAM(3)      = 0.5 * CM_PER_INCH * CCCH_MODULE_INNER_LENGTH
      PARAM(4)      = 0.5 * CM_PER_INCH * CCCH_MODULE_OUTER_LENGTH
      PARAM(5)      = 0.5 * CM_PER_INCH *
     &          (CCCH_MODULE_OUTER_RADIUS - CCCH_MODULE_INNER_RADIUS)
C----------------------------------------------------------------------
C  Get the rotation matrix ID for the first CCCH module
C----------------------------------------------------------------------
      CALL EZGET('CCCH_FIRST_ROTATION_MATRIX',FIRST_ID,IER)
C----------------------------------------------------------------------
C  Get the angular offset for the first CCCH module
C----------------------------------------------------------------------
      CALL EZGET('CCCH_ANGULAR_OFFSET',OFFSET,IER)
C----------------------------------------------------------------------
C  Set angular size of each module
C----------------------------------------------------------------------
      ANGLE_SIZE = 360. / CCCH_NUMBER_MODULES
C----------------------------------------------------------------------
C  Set the radius of the module mid-point
C----------------------------------------------------------------------
      RADIUS = 0.5 * (CCCH_MODULE_INNER_RADIUS + 
     &                CCCH_MODULE_OUTER_RADIUS)
C----------------------------------------------------------------------
C  Get the module of the main ring
C----------------------------------------------------------------------
      CALL EZGET('CCCH_MR_MODULE',MR_MODULE,IER)
C----------------------------------------------------------------------
C  Position multiple copies of the normal CCCH module volumes.  There 
C  is only one module containing the main ring, and it has a special 
C  name.
C----------------------------------------------------------------------
      ROTATION_MATRIX = FIRST_ID - 1
      COUNTER = 0
      DO MODULE=1,CCCH_NUMBER_MODULES
        WRITE(VOLUME_LABEL,1001) MODULE
        IF ( MODULE  .EQ. MR_MODULE ) THEN
          VOLUME_NAME = MR_MODULE_VOLUME_NAME
          COPY_NUMBER = 1
        ELSE
          VOLUME_NAME = MODULE_VOLUME_NAME
          COUNTER = COUNTER + 1
          COPY_NUMBER = COUNTER
        ENDIF
        ROTATION_MATRIX = ROTATION_MATRIX + 1
        ANGLE = (MODULE-1) * ANGLE_SIZE + OFFSET
        X_POSITION = CM_PER_INCH * RADIUS * COS((ANGLE)*RAD)
        Y_POSITION = CM_PER_INCH * RADIUS * SIN((ANGLE)*RAD)
        CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Save special main ring beampipe CCCH module positioning
C----------------------------------------------------------------------
        IF ( MODULE .EQ. MR_MODULE ) THEN
          CCCH_MR_MODULE_X     = X_POSITION
          CCCH_MR_MODULE_Y     = Y_POSITION
          CCCH_MR_MODULE_ANGLE = ANGLE
        ENDIF
      ENDDO
      RETURN
 1000 FORMAT('CCCH_NUMBER_MODULES',T40,I2)
 1001 FORMAT('CCCH_',I2.2,'_VOLUME')
      END
