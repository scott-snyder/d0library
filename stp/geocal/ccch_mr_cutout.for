      SUBROUTINE CCCH_MR_CUTOUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCCH Special module for Main Ring bypass
C-                         cutout section volume
C-
C-      Define a volume to be nested within the Floor volume of the
C-      special CCCH module containing the Main Ring beam pipe.
C-      This volume overrides the standard active Floor volume,
C-      and is filled with inactive liquid argon.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  10-JAN-1989   Stuart Fuess
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCCH_FLOOR.INC'
      INCLUDE 'D0$INC:SCCCH_MODULE.INC'
      INCLUDE 'D0$INC:SCCCH_MR_BEAMPIPE.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER ELEMENT
      INTEGER LSTRING
C  Reals
      REAL CCCH_CUTOUT_INNER_RADIUS
      REAL CCCH_CUTOUT_OUTER_RADIUS
      REAL CCCH_CUTOUT_INNER_WIDTH
      REAL CCCH_CUTOUT_OUTER_WIDTH
      REAL CCCH_CUTOUT_INNER_LENGTH
      REAL CCCH_CUTOUT_OUTER_LENGTH
      REAL CCCH_CUTOUT_HEIGHT
      REAL CCCH_CUTOUT_SKIN
      REAL ALPHA, BETA, GAMMA, H, W
C  Characters
      CHARACTER*32 NAME
C  Equivalences
      INTEGER IVAL(6)
      REAL RVAL(6)
      EQUIVALENCE ( IVAL, RVAL )
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
      REAL RAD
      PARAMETER (RAD=0.017453293)
C----------------------------------------------------------------------
C  Get the element number of the location of the cutout in the special
C  MR CCCH module
C----------------------------------------------------------------------
      CALL EZGET_i('CCCH_MR_CUTOUT_ELEMENT',ELEMENT,IER)
C----------------------------------------------------------------------
C  Get the radial position of this element, which gives the inner edge
C  of the cutout section skin.  Add the skin thickness to get the
C  inner radius of the cutout section
C----------------------------------------------------------------------
      WRITE(NAME,1001) ELEMENT
      CALL EZGET_iarr(NAME,IVAL,IER)
      CALL EZGET('CCCH_MR_CUTOUT_SKIN',CCCH_CUTOUT_SKIN,IER)
      CCCH_CUTOUT_INNER_RADIUS = RVAL(3) + CCCH_CUTOUT_SKIN
C----------------------------------------------------------------------
C  The outer radius of the cutout section is the same as the Floor
C  outer radius
C----------------------------------------------------------------------
      CCCH_CUTOUT_OUTER_RADIUS = CCCH_FLOOR_OUTER_RADIUS
C----------------------------------------------------------------------
C  Compute the cutout region height
C----------------------------------------------------------------------
      CCCH_CUTOUT_HEIGHT = CCCH_CUTOUT_OUTER_RADIUS -
     &                     CCCH_CUTOUT_INNER_RADIUS
C----------------------------------------------------------------------
C  Get the lateral size of the cutout region at the inner radius
C----------------------------------------------------------------------
      CALL EZGET('CCCH_MR_CUTOUT_WIDTH',CCCH_CUTOUT_INNER_WIDTH,IER)
C----------------------------------------------------------------------
C  Compute the lateral size of the cutout region at the outer radius
C  using the Floor volume dimensions
C----------------------------------------------------------------------
      CCCH_CUTOUT_OUTER_WIDTH = CCCH_CUTOUT_INNER_WIDTH +
     &  0.5 * CCCH_CUTOUT_HEIGHT * 
     &  (CCCH_FLOOR_OUTER_WIDTH - CCCH_FLOOR_INNER_WIDTH) /
     &  (CCCH_FLOOR_OUTER_RADIUS - CCCH_FLOOR_INNER_RADIUS)
C----------------------------------------------------------------------
C  The outer length of the cutout section is the same as the outer
C  length of the Floor volume
C----------------------------------------------------------------------
      CCCH_CUTOUT_OUTER_LENGTH = CCCH_FLOOR_OUTER_LENGTH
C----------------------------------------------------------------------
C  Compute the inner length of the cutout section
C----------------------------------------------------------------------
      CCCH_CUTOUT_INNER_LENGTH = CCCH_CUTOUT_OUTER_LENGTH +
     &  CCCH_CUTOUT_HEIGHT *
     &  (CCCH_FLOOR_INNER_LENGTH - CCCH_FLOOR_OUTER_LENGTH) /
     &  (CCCH_FLOOR_OUTER_RADIUS - CCCH_FLOOR_INNER_RADIUS)
C----------------------------------------------------------------------
C  Set the GEANT SRCP parameters for the cutout section
C----------------------------------------------------------------------
      VOLUME_LABEL = 'CCCH_MR_CUTOUT_VOLUME'
      CALL EZGET_i('CCCH_MR_CUTOUT_VOLUME_NAME',VOLUME_NAME,IER)
      CALL UCTOH('TRAP',VOLUME_SHAPE,4,4)
      CALL EZGET_i('LIQUID_ARGON_CODE',VOLUME_MATERIAL_CODE,IER)
      CALL EZGET_i('CCCH_MR_FLOOR8_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH('POS',POSITIONING,4,3)
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      ALPHA = CCCH_CUTOUT_INNER_WIDTH
      BETA  = CCCH_CUTOUT_OUTER_WIDTH
      GAMMA = CCCH_CUTOUT_HEIGHT
      H     = CCCH_FLOOR_OUTER_RADIUS - CCCH_FLOOR_INNER_RADIUS
      W     = 0.25 * (CCCH_FLOOR_INNER_WIDTH + CCCH_FLOOR_OUTER_WIDTH)
      X_POSITION        = CM_PER_INCH *
     &  (ALPHA*(0.25-0.5*H/GAMMA) - BETA*(0.75-0.5*H/GAMMA) + W)
      Y_POSITION        = 0.0
      Z_POSITION        = 0.5 * CM_PER_INCH *
     &  (CCCH_FLOOR_OUTER_RADIUS - CCCH_FLOOR_INNER_RADIUS -
     &   CCCH_CUTOUT_HEIGHT)
      NUMBER_PARAMS     = 11
      PARAM(1)          = 0.5 * CM_PER_INCH * CCCH_CUTOUT_HEIGHT
      PARAM(2)          = ATAN( 0.5 * (CCCH_CUTOUT_OUTER_WIDTH -
     &                                  CCCH_CUTOUT_INNER_WIDTH) / 
     &                           CCCH_CUTOUT_HEIGHT )/RAD
      PARAM(3)          = 0.
      PARAM(4)          = 0.5 * CM_PER_INCH * CCCH_CUTOUT_INNER_LENGTH
      PARAM(5)          = 0.5 * CM_PER_INCH * CCCH_CUTOUT_INNER_WIDTH
      PARAM(6)          = PARAM(5)
      PARAM(7)          = 0.
      PARAM(8)          = 0.5 * CM_PER_INCH * CCCH_CUTOUT_OUTER_LENGTH
      PARAM(9)          = 0.5 * CM_PER_INCH * CCCH_CUTOUT_OUTER_WIDTH
      PARAM(10)         = PARAM(9)
      PARAM(11)         = 0.
C----------------------------------------------------------------------
C  Write the volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Save positioning info (in CM) of cutout region
C----------------------------------------------------------------------
      CCCH_MR_CUTOUT_DX = X_POSITION
      CCCH_MR_CUTOUT_DZ = Z_POSITION
  999 RETURN
 1001 FORMAT('CCCH_ELEMENT_',I3.3)
      END
