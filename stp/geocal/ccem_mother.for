      SUBROUTINE CCEM_MOTHER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the GEANT CCEM mother volumes 
C-
C-      The mother volume will be a 'TUBE' with inner radius given 
C-      by the outer radius of the cryostat inner cold wall, outer 
C-      radius given by the EM/FH boundary, length given by the EM 
C-      module lengths, and filled with liquid argon.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-OCT-1988   Stuart Fuess
C-   Updated   4-FEB-1990   Stuart Fuess  Use EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCEM_MOTHER.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Get the mother volume inner radius
C----------------------------------------------------------------------
      CALL EZGET ('CCEM_MOTHER_INNER_RADIUS',
     &             CCEM_MOTHER_INNER_RADIUS,IER)
C----------------------------------------------------------------------
C  Get the mother volume outer radius
C----------------------------------------------------------------------
      CALL EZGET ('CCEM_MOTHER_OUTER_RADIUS',
     &             CCEM_MOTHER_OUTER_RADIUS,IER)
C----------------------------------------------------------------------
C  Get the total length of the EM section
C----------------------------------------------------------------------
      CALL EZGET ('CCEM_MOTHER_LENGTH',CCEM_MOTHER_LENGTH,IER)
C----------------------------------------------------------------------
C  Fill the mother volume with liquid argon
C----------------------------------------------------------------------
      CALL EZGET ('LIQUID_ARGON_CODE',VOLUME_MATERIAL_CODE,IER)
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for the CCEM mother volume
C----------------------------------------------------------------------
      CALL EZGETS ('CCEM_MOTHER_VOLUME_LABEL',1,VOLUME_LABEL,LEN,IER)
      CALL EZGET ('CCEM_MOTHER_VOLUME_NAME',VOLUME_NAME,IER)
      CALL UCTOH ('TUBE',VOLUME_SHAPE,4,4)
      CALL EZGET ('CAL_MOTHER_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH ('POS',POSITIONING,4,3)
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      X_POSITION        = 0.0
      Y_POSITION        = 0.0
      Z_POSITION        = 0.0
      NUMBER_PARAMS     = 3
      PARAM(1)          = CM_PER_INCH * CCEM_MOTHER_INNER_RADIUS
      PARAM(2)          = CM_PER_INCH * CCEM_MOTHER_OUTER_RADIUS
      PARAM(3)          = 0.5 * CM_PER_INCH * CCEM_MOTHER_LENGTH
C----------------------------------------------------------------------
C  Write the CCEM mother volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
      RETURN
      END
