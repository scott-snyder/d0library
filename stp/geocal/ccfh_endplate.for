      SUBROUTINE CCFH_ENDPLATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCFH Endplates
C-
C-      Define a volume for each endplate which is a trapezoid
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCFH_CRACK.INC'
      INCLUDE 'D0$INC:SCCFH_ENDPLATE.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Get the CCFH endplate dimensions
C----------------------------------------------------------------------
      CALL EZGET ('CCFH_ENDPLATE_INNER_WIDTH',
     &             CCFH_ENDPLATE_INNER_WIDTH,IER)
      CALL EZGET ('CCFH_ENDPLATE_OUTER_WIDTH',
     &             CCFH_ENDPLATE_OUTER_WIDTH,IER)
      CALL EZGET ('CCFH_ENDPLATE_PARTIAL_HEIGHT',
     &             CCFH_ENDPLATE_HEIGHT,IER)
      CALL EZGET ('CCFH_ENDPLATE_THICKNESS',
     &             CCFH_ENDPLATE_THICKNESS,IER)
C----------------------------------------------------------------------
C  Compute volume of each endplate
C----------------------------------------------------------------------
      CCFH_ENDPLATE_VOLUME = 0.5 * (CCFH_ENDPLATE_INNER_WIDTH +
     &                              CCFH_ENDPLATE_OUTER_WIDTH) *
     &                              CCFH_ENDPLATE_HEIGHT *
     &                              CCFH_ENDPLATE_THICKNESS
C----------------------------------------------------------------------
C  Get CCFH endplate position
C----------------------------------------------------------------------
      CALL EZGET ('CCFH_ENDPLATE_POSITION',
     &             CCFH_ENDPLATE_POSITION,IER)
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for the South=+ endplate
C----------------------------------------------------------------------
      VOLUME_LABEL      = 'CCFH_SOUTH_ENDPLATE_VOLUME'
      CALL EZGET('CCFH_SOUTH_ENDPLATE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL UCTOH('TRD1',VOLUME_SHAPE,4,4)
      CALL EZGET('STAINLESS_STEEL_CODE',VOLUME_MATERIAL_CODE,IER)
      CALL EZGET('CCFH_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH('POS',POSITIONING,4,3)
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      X_POSITION        = 0.0
      Y_POSITION        = CM_PER_INCH * CCFH_ENDPLATE_POSITION
      Z_POSITION        = 0.0
      NUMBER_PARAMS     = 4
      PARAM(1)          = 0.5 * CM_PER_INCH * CCFH_ENDPLATE_INNER_WIDTH
      PARAM(2)          = 0.5 * CM_PER_INCH * CCFH_ENDPLATE_OUTER_WIDTH
      PARAM(3)          = 0.5 * CM_PER_INCH * CCFH_ENDPLATE_THICKNESS
      PARAM(4)          = 0.5 * CM_PER_INCH * CCFH_ENDPLATE_HEIGHT
C----------------------------------------------------------------------
C  Write the South endplate volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Set the parameters for the North=- endplate
C----------------------------------------------------------------------
      VOLUME_LABEL = 'CCFH_NORTH_ENDPLATE_VOLUME'
      CALL EZGET('CCFH_NORTH_ENDPLATE_VOLUME_NAME',VOLUME_NAME,IER)
      Y_POSITION   = - Y_POSITION
C----------------------------------------------------------------------
C  Write the North endplate volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
      RETURN
      END
