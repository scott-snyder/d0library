      SUBROUTINE ECMH_BASIC_MODULE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the ECMH module GEANT volumes.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-MAR-1990   Norman A. Amos
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
      INCLUDE 'D0$INC:MATRIX.INC'
      INCLUDE 'D0$INC:IUSET.INC'
      INTEGER IER
C----------------------------------------------------------------------
C- Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK( 'ENDCAP' )
C----------------------------------------------------------------------
C- Constants.
C----------------------------------------------------------------------
      CALL UCTOH('TRD1',VOLUME_SHAPE,4,4)
      CALL UCTOH('POS',POSITIONING,4,3)
      NUMBER_PARAMS = 4
C----------------------------------------------------------------------
C- Finally, write the Rotation Matrix.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_ROTATION_MATRIX',ID_MATRIX,IER)
      ROTATION_MATRIX=ID_MATRIX
      VAL_MATRIX(1) = 90.
      VAL_MATRIX(2) = 180.
      VAL_MATRIX(3) = 0.
      VAL_MATRIX(4) = 0.
      VAL_MATRIX(5) = 90.
      VAL_MATRIX(6) = 90.
      CALL STORE_MATRIX
C
  999 RETURN
      END
