      SUBROUTINE ECIH_BASIC_MODULE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set most of the GEANT geometry parameters 
C-                         for an ECIH module.  Parameters NOT set
C-                         are label, position and material information.
C-                         These remaining parameters are set, and the
C-                         volume structure stored, in the ECIH_HOMO_ 
C-                         and ECIH_PLATE_ specific module routines.
C-                         If necessary, the copy number may also be
C-                         reset.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  18-MAR-1990   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:ECIH_MODULE.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'ENDCAP' )
C----------------------------------------------------------------------
C  Set the volume shape
C----------------------------------------------------------------------
      CALL UCTOH  ( 'TUBE', VOLUME_SHAPE, 4, 4 )
C----------------------------------------------------------------------
C  Extract the volume shape parameters.  Save raw values in common
C  plus convert to CM for GEANT volume structure.
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_MODULE_INNER_RADIUS', 
     &  ECIH_MODULE_INNER_RADIUS, IER )
      CALL EZGET ( 'ECIH_MODULE_OUTER_RADIUS', 
     &  ECIH_MODULE_OUTER_RADIUS, IER )
      CALL EZGET ( 'ECIH_MODULE_LENGTH', ECIH_MODULE_LENGTH, IER )
      NUMBER_PARAMS = 3
      PARAM(1) = CM_PER_INCH * ECIH_MODULE_INNER_RADIUS
      PARAM(2) = CM_PER_INCH * ECIH_MODULE_OUTER_RADIUS
      PARAM(3) = 0.5 * CM_PER_INCH * ECIH_MODULE_LENGTH
C----------------------------------------------------------------------
C  Compute and store the volume of the volume, in raw units
C----------------------------------------------------------------------
      ECIH_MODULE_VOLUME = ECIH_MODULE_LENGTH * PI *
     &  ( ECIH_MODULE_OUTER_RADIUS **2 - ECIH_MODULE_INNER_RADIUS **2 )
  999 RETURN
      END
