      SUBROUTINE CCEM_ENDPLATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCEM Endplates
C-
C-      Define a volume for each endplate which is a trapezoid
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-   Updated   4-DEC-1989   Stuart Fuess  Remove D0$INC:SCCEM_CRACK.INC 
C-   Updated   4-FEB-1990   Stuart Fuess  Use more general common
C-                                        blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCEM_ENDPLATE.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Select the CC SRCP file
C  Used both for EZGETs in this routine and in the CCEM_ routines
C----------------------------------------------------------------------
      CALL EZPICK ( 'CENTRAL' )
C----------------------------------------------------------------------
C  Get the CCEM endplate dimensions
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_ENDPLATE_INNER_WIDTH',
     &             CCEM_ENDPLATE_INNER_WIDTH, IER )
      CALL EZGET ( 'CCEM_ENDPLATE_OUTER_WIDTH',
     &             CCEM_ENDPLATE_OUTER_WIDTH, IER )
      CALL EZGET ( 'CCEM_ENDPLATE_PARTIAL_HEIGHT',
     &             CCEM_ENDPLATE_HEIGHT, IER )
      CALL EZGET ( 'CCEM_ENDPLATE_THICKNESS',
     &             CCEM_ENDPLATE_THICKNESS, IER )
C----------------------------------------------------------------------
C  Compute volume of each endplate
C----------------------------------------------------------------------
      CCEM_ENDPLATE_VOLUME = 0.5 * (CCEM_ENDPLATE_INNER_WIDTH +
     &                              CCEM_ENDPLATE_OUTER_WIDTH) *
     &                              CCEM_ENDPLATE_HEIGHT *
     &                              CCEM_ENDPLATE_THICKNESS
C----------------------------------------------------------------------
C  Get CCEM endplate position
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_ENDPLATE_POSITION',
     &             CCEM_ENDPLATE_POSITION, IER )
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for the South=+ endplate
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_SOUTH_ENDPLATE_VOLUME_LABEL', 1,
     &               VOLUME_LABEL, LEN, IER )
      CALL EZGET_i ( 'CCEM_SOUTH_ENDPLATE_VOLUME_NAME', VOLUME_NAME,IER)
      CALL UCTOH ( 'TRD1', VOLUME_SHAPE, 4, 4 )
      CALL EZGET_i ( 'STAINLESS_STEEL_CODE', VOLUME_MATERIAL_CODE, IER )
      CALL EZGET_i ( 'CCEM_MODULE_VOLUME_NAME', VOLUME_MOTHER, IER )
      CALL UCTOH ( 'POS', POSITIONING, 4, 3 )
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      X_POSITION        = 0.0
      Y_POSITION        = CM_PER_INCH * CCEM_ENDPLATE_POSITION
      Z_POSITION        = 0.0
      NUMBER_PARAMS     = 4
      PARAM(1)          = 0.5 * CM_PER_INCH * CCEM_ENDPLATE_INNER_WIDTH
      PARAM(2)          = 0.5 * CM_PER_INCH * CCEM_ENDPLATE_OUTER_WIDTH
      PARAM(3)          = 0.5 * CM_PER_INCH * CCEM_ENDPLATE_THICKNESS
      PARAM(4)          = 0.5 * CM_PER_INCH * CCEM_ENDPLATE_HEIGHT
C----------------------------------------------------------------------
C  Write the South endplate volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Set the parameters for the North=- endplate
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_NORTH_ENDPLATE_VOLUME_LABEL', 1,
     &               VOLUME_LABEL, LEN, IER )
      CALL EZGET_i ( 'CCEM_NORTH_ENDPLATE_VOLUME_NAME', VOLUME_NAME,IER)
      Y_POSITION   = - Y_POSITION
C----------------------------------------------------------------------
C  Write the North endplate volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
      RETURN
      END
