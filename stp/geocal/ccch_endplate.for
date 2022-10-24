      SUBROUTINE CCCH_ENDPLATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCCH Endplates
C-
C-      Define a volume for each endplate which is a trapezoid
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-   Updated  10-JAN-1989   Stuart Fuess  Add Main Ring 
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCCH_CRACK.INC'
      INCLUDE 'D0$INC:SCCCH_ENDPLATE.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
      REAL RAD
      PARAMETER (RAD=0.017453293)
C----------------------------------------------------------------------
C  Get the CCCH endplate dimensions
C----------------------------------------------------------------------
      CALL EZGET ('CCCH_ENDPLATE_INNER_WIDTH',
     &             CCCH_ENDPLATE_INNER_WIDTH,IER)
      CALL EZGET('CCCH_ENDPLATE_OUTER_WIDTH',
     &             CCCH_ENDPLATE_OUTER_WIDTH,IER)
      CALL EZGET ('CCCH_ENDPLATE_PARTIAL_HEIGHT',
     &             CCCH_ENDPLATE_HEIGHT,IER)
      CALL EZGET ('CCCH_ENDPLATE_THICKNESS',
     &             CCCH_ENDPLATE_THICKNESS,IER)
C----------------------------------------------------------------------
C  Get the tilt angle of the endplate
C----------------------------------------------------------------------
      CALL EZGET('CCCH_ENDPLATE_TILT',CCCH_ENDPLATE_TILT,IER)
C----------------------------------------------------------------------
C  Compute approximate volume of each endplate
C----------------------------------------------------------------------
      CCCH_ENDPLATE_VOLUME = 0.5 * (CCCH_ENDPLATE_INNER_WIDTH +
     &                              CCCH_ENDPLATE_OUTER_WIDTH) *
     &                              CCCH_ENDPLATE_HEIGHT *
     &                              CCCH_ENDPLATE_THICKNESS /
     &                              COS((CCCH_ENDPLATE_TILT)*RAD)
C----------------------------------------------------------------------
C  Get CCCH endplate position
C----------------------------------------------------------------------
      CALL EZGET ('CCCH_ENDPLATE_POSITION',
     &             CCCH_ENDPLATE_POSITION,IER)
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for the South=+ endplate
C----------------------------------------------------------------------
      CALL UCTOH('TRAP',VOLUME_SHAPE,4,4)
      CALL EZGET_i('STAINLESS_STEEL_CODE',VOLUME_MATERIAL_CODE,IER)
      CALL UCTOH('POS',POSITIONING,4,3)
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      X_POSITION        = 0.0
      Y_POSITION        = CM_PER_INCH * CCCH_ENDPLATE_POSITION
      Z_POSITION        = 0.0
      NUMBER_PARAMS     = 11
      PARAM(1)          = 0.5 * CM_PER_INCH *
     &                          CCCH_ENDPLATE_HEIGHT
      PARAM(2)          = CCCH_ENDPLATE_TILT
      PARAM(3)          = 90.0
      PARAM(4)          = 0.5 * CM_PER_INCH * CCCH_ENDPLATE_THICKNESS /
     &                          COS((CCCH_ENDPLATE_TILT)*RAD)
      PARAM(5)          = 0.5 * CM_PER_INCH * CCCH_ENDPLATE_INNER_WIDTH
      PARAM(6)          = PARAM(5)
      PARAM(7)          = 0.0
      PARAM(8)          = PARAM(4)
      PARAM(9)          = 0.5 * CM_PER_INCH * CCCH_ENDPLATE_OUTER_WIDTH
      PARAM(10)         = PARAM(9)
      PARAM(11)         = 0.0
C----------------------------------------------------------------------
C  Write the South endplate volume GEANT SRCP parameter description
C  for both normal and special main ring bypass modules
C----------------------------------------------------------------------
      VOLUME_LABEL = 'CCCH_SOUTH_ENDPLATE_VOLUME'
      CALL EZGET_i('CCCH_SOUTH_ENDPLATE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET_i('CCCH_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL WRITE_VOLUME
C
      VOLUME_LABEL = 'CCCH_MR_SOUTH_ENDPLATE'
      CALL EZGET_i('CCCH_MR_SOUTH_ENDPLATE_NAME',VOLUME_NAME,IER)
      CALL EZGET_i('CCCH_MR_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Set the parameters for the North=- endplate
C----------------------------------------------------------------------
      Y_POSITION   = - Y_POSITION
      PARAM(2) = - PARAM(2)
C----------------------------------------------------------------------
C  Write the North endplate volume GEANT SRCP parameter description
C  for both normal and special main ring bypass modules
C----------------------------------------------------------------------
      VOLUME_LABEL = 'CCCH_NORTH_ENDPLATE_VOLUME'
      CALL EZGET_i('CCCH_NORTH_ENDPLATE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET_i('CCCH_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL WRITE_VOLUME
C
      VOLUME_LABEL = 'CCCH_MR_NORTH_ENDPLATE'
      CALL EZGET_i('CCCH_MR_NORTH_ENDPLATE_NAME',VOLUME_NAME,IER)
      CALL EZGET_i('CCCH_MR_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL WRITE_VOLUME
      RETURN
      END
