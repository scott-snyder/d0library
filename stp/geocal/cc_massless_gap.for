      SUBROUTINE CC_MASSLESS_GAP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the GEANT CC Massless Gap Volumes
C-
C-      The Massless Gaps will be 'TUBEs' filled with Liquid Argon
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  22-NOV-1988   Stuart Fuess
C-   Updated  28-JUN-1989   Chip Stewart  (PUT IN CALL TO CC_MG_DIV) 
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCC_MASSLESS_GAP.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Get the Massless Gap inner radius
C----------------------------------------------------------------------
      CALL EZGET ('CC_MASSLESS_GAP_INNER_RADIUS',
     &             CC_MASSLESS_GAP_INNER_RADIUS,IER)
C----------------------------------------------------------------------
C  Get the Massless Gap outer radius
C----------------------------------------------------------------------
      CALL EZGET ('CC_MASSLESS_GAP_OUTER_RADIUS',
     &             CC_MASSLESS_GAP_OUTER_RADIUS,IER)
C----------------------------------------------------------------------
C  Get the length of each Massless Gap
C----------------------------------------------------------------------
      CALL EZGET ('CC_MASSLESS_GAP_LENGTH',CC_MASSLESS_GAP_LENGTH,IER)
C----------------------------------------------------------------------
C  Get the position of each Massless Gap
C----------------------------------------------------------------------
      CALL EZGET ('CC_MASSLESS_GAP_POSITION',
     &             CC_MASSLESS_GAP_POSITION,IER)
C----------------------------------------------------------------------
C  Fill the Massless Gap with liquid argon
C----------------------------------------------------------------------
      CALL EZGET ('LIQUID_ARGON_CODE',VOLUME_MATERIAL_CODE,IER)
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for the South=+ Massless Gap
C----------------------------------------------------------------------
      CALL EZGETS ('CC_SOUTH_MASSLESS_GAP_LABEL',1,VOLUME_LABEL,LEN,IER)
      CALL EZGET ('CC_SOUTH_MASSLESS_GAP_NAME',VOLUME_NAME,IER)
      CALL UCTOH ('TUBE',VOLUME_SHAPE,4,4)
      CALL EZGET ('CAL_MOTHER_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH ('POS',POSITIONING,4,3)
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      X_POSITION        = 0.0
      Y_POSITION        = 0.0
      Z_POSITION        = CM_PER_INCH * CC_MASSLESS_GAP_POSITION
      NUMBER_PARAMS     = 3
      PARAM(1)          = CM_PER_INCH * CC_MASSLESS_GAP_INNER_RADIUS
      PARAM(2)          = CM_PER_INCH * CC_MASSLESS_GAP_OUTER_RADIUS
      PARAM(3)          = 0.5 * CM_PER_INCH * CC_MASSLESS_GAP_LENGTH
C----------------------------------------------------------------------
C  Write the South Massless Gap volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for the North=- Massless Gap
C----------------------------------------------------------------------
      CALL EZGETS ('CC_NORTH_MASSLESS_GAP_LABEL',1,VOLUME_LABEL,LEN,IER)
      CALL EZGET ('CC_NORTH_MASSLESS_GAP_NAME',VOLUME_NAME,IER)
      Z_POSITION        = - Z_POSITION
C----------------------------------------------------------------------
C  Write the South Massless Gap volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Do CC Massless Gap Divisions same way as EC MGs are done
C----------------------------------------------------------------------
      CALL CC_MG_DIV
C
      RETURN
      END
