      SUBROUTINE ECIH_BACK_PLATE_GEOM(Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract volume dimensions for ECIH Back 
C-                         plate and create GEANT volume structure
C-
C-   Inputs  : Z        Z position of the inner edge of this volume
C-                      relative to the Module volume (inches)
C-   Outputs : Z        Z position of the outer edge of this volume
C-                      relative to the Module volume (inches)
C-   Controls: none
C-
C-   Created  18-MAR-1990   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      REAL    Z
C  Include files
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
C  Reals
      REAL INNER_RADIUS
      REAL OUTER_RADIUS
      REAL THICKNESS
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK('ENDCAP')
C----------------------------------------------------------------------
C  Extract Back plate dimension information
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_BACK_PLATE_INNER_RADIUS', 
     &  INNER_RADIUS, IER )
      CALL EZGET ( 'ECIH_BACK_PLATE_OUTER_RADIUS', 
     &  OUTER_RADIUS, IER )
      CALL EZGET ( 'ECIH_BACK_PLATE_THICKNESS', 
     &  THICKNESS, IER )
C----------------------------------------------------------------------
C  Extract volume information
C----------------------------------------------------------------------
      CALL EZGETS ( 'ECIH_BACK_PLATE_VOLUME_LABEL', 1,
     &  VOLUME_LABEL, LEN, IER )
      CALL EZGET_i ( 'ECIH_BACK_PLATE_VOLUME_NAME', VOLUME_NAME, IER )
      CALL UCTOH ( 'TUBE', VOLUME_SHAPE, 4, 4 )
      CALL EZGET_i ( 'STAINLESS_STEEL_CODE', VOLUME_MATERIAL_CODE, IER )
      CALL EZGET_i ( 'ECIH_MODULE_VOLUME_NAME', VOLUME_MOTHER, IER )
      CALL UCTOH ( 'POS', POSITIONING, 4, 3 )
      ROTATION_MATRIX = 1
      COPY_NUMBER = 1
      X_POSITION  = 0.
      Y_POSITION  = 0.
      Z_POSITION  = CM_PER_INCH * ( Z + 0.5 * THICKNESS )
      NUMBER_PARAMS = 3
      PARAM(1) = CM_PER_INCH * INNER_RADIUS
      PARAM(2) = CM_PER_INCH * OUTER_RADIUS
      PARAM(3) = 0.5 * CM_PER_INCH * THICKNESS
C----------------------------------------------------------------------
C  Write Back plate volume
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Increment Z position
C----------------------------------------------------------------------
      Z = Z + THICKNESS
  999 RETURN
      END
