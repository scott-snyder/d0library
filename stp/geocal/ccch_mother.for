      SUBROUTINE CCCH_MOTHER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the GEANT CCCH mother volumes 
C-
C-      The mother volumes will be a 'PCON' with inner radius given by 
C-      the FH/CH boundary, outer radius given by the endplate outer
C-      radius, length given by the CH module lengths, and filled with 
C-      liquid argon.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-OCT-1988   Stuart Fuess
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCCH_MOTHER.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
C  Reals
      REAL RMIN, RMAX
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Get the mother volume inner radius
C----------------------------------------------------------------------
      CALL EZGET ('CCCH_MOTHER_INNER_RADIUS',
     &             CCCH_MOTHER_INNER_RADIUS,IER)
C----------------------------------------------------------------------
C  Get the mother volume outer radius
C----------------------------------------------------------------------
      CALL EZGET ('CCCH_MOTHER_OUTER_RADIUS',
     &             CCCH_MOTHER_OUTER_RADIUS,IER)
C----------------------------------------------------------------------
C  Get the lengths of the CH section
C----------------------------------------------------------------------
      CALL EZGET ('CCCH_MOTHER_INNER_LENGTH',
     &             CCCH_MOTHER_INNER_LENGTH,IER)
      CALL EZGET ('CCCH_MOTHER_OUTER_LENGTH',
     &             CCCH_MOTHER_OUTER_LENGTH,IER)
C----------------------------------------------------------------------
C  Fill the mother volume with liquid argon
C----------------------------------------------------------------------
      CALL EZGET ('LIQUID_ARGON_CODE',VOLUME_MATERIAL_CODE,IER)
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for the CCCH mother volume
C----------------------------------------------------------------------
      CALL EZGETS ('CCCH_MOTHER_VOLUME_LABEL',1,VOLUME_LABEL,LEN,IER)
      CALL EZGET ('CCCH_MOTHER_VOLUME_NAME',VOLUME_NAME,IER)
      CALL UCTOH ('PCON',VOLUME_SHAPE,4,4)
      CALL EZGET ('CAL_MOTHER_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH ('POS',POSITIONING,4,3)
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      X_POSITION        = 0.0
      Y_POSITION        = 0.0
      Z_POSITION        = 0.0
      NUMBER_PARAMS     = 15
      RMIN = CM_PER_INCH * CCCH_MOTHER_INNER_RADIUS
      RMAX = CM_PER_INCH * CCCH_MOTHER_OUTER_RADIUS
      PARAM(1)          = 0.0
      PARAM(2)          = 360.0
      PARAM(3)          = 4.
      PARAM(4)          = - 0.5 * CM_PER_INCH * CCCH_MOTHER_INNER_LENGTH
      PARAM(5)          = RMIN
      PARAM(6)          = RMIN
      PARAM(7)          = - 0.5 * CM_PER_INCH * CCCH_MOTHER_OUTER_LENGTH
      PARAM(8)          = RMIN
      PARAM(9)          = RMAX
      PARAM(10)         = - PARAM(7)
      PARAM(11)         = RMIN
      PARAM(12)         = RMAX
      PARAM(13)         = - PARAM(4)
      PARAM(14)         = RMIN
      PARAM(15)         = RMIN
C----------------------------------------------------------------------
C  Write the CCCH mother volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
      RETURN
      END
