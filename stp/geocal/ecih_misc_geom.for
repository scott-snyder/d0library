      SUBROUTINE ECIH_MISC_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create miscellaneous ECIH GEANT volume
C-                         structures which will be used in both
C-                         homogeneous and plate level descriptions
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
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
C  Reals
      REAL IR
      REAL OR
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK('ENDCAP')
C----------------------------------------------------------------------
C  Create support pipe volume
C----------------------------------------------------------------------
      CALL EZGETS ( 'ECIH_SUPPORT_PIPE_VOLUME_LABEL', 1,
     &  VOLUME_LABEL, LEN, IER )
      CALL EZGET ( 'ECIH_SUPPORT_PIPE_VOLUME_NAME', VOLUME_NAME, IER )
      CALL UCTOH ( 'TUBE', VOLUME_SHAPE, 4, 4 )
      CALL EZGET ( 'STAINLESS_STEEL_CODE', VOLUME_MATERIAL_CODE, IER )
      CALL EZGET ( 'ECIH_MODULE_VOLUME_NAME', VOLUME_MOTHER, IER )
      CALL UCTOH ( 'POS', POSITIONING, 4, 3 )
      ROTATION_MATRIX = 1
      COPY_NUMBER = 1
      X_POSITION  = 0.
      Y_POSITION  = 0.
      Z_POSITION  = 0.
      CALL EZGET ( 'ECIH_SUPPORT_PIPE_INNER_RADIUS', IR, IER )
      CALL EZGET ( 'ECIH_SUPPORT_PIPE_OUTER_RADIUS', OR, IER )
      NUMBER_PARAMS = 3
      PARAM(1) = CM_PER_INCH * IR
      PARAM(2) = CM_PER_INCH * OR
      PARAM(3) = 0.5 * CM_PER_INCH * ECIH_MODULE_LENGTH
C----------------------------------------------------------------------
C  Write support pipe volume
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
  999 RETURN
      END
