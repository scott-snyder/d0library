      SUBROUTINE CCCH_MR_BEAMPIPE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCCH Main Ring Beampipe volume
C-
C-      Define a volume to be nested within the CCCH MR cutout section
C-      volume.  The volume is a tube, filled with vacuum.  Since the
C-      ends of the CCCH MR cutout section volume are tapered, the
C-      beampipe tube volume will have some overlap.
C-
C-      Note: Most of the quantities in this routine are in Centimeters
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  12-JAN-1989   Stuart Fuess
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCCH_MR_BEAMPIPE.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
      INCLUDE 'D0$INC:MATRIX.INC'
C  Integers
      INTEGER IER
C  Reals
      REAL CCCH_MR_BEAMPIPE_X
      REAL CCCH_MR_BEAMPIPE_Y
      REAL CCCH_MR_BEAMPIPE_RADIUS
      REAL CCCH_MR_BEAMPIPE_LENGTH
      REAL X1, X2, X3, XREL
      REAL Y1, Y2, Y3, ZREL
      REAL SINA, COSA
C  Characters
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
      REAL RAD
      PARAMETER (RAD=0.017453293)
C----------------------------------------------------------------------
C  Get the position and size of the Main Ring beampipe, and convert
C  units to CM.  These coordinates are in the Master Reference System.
C----------------------------------------------------------------------
      CALL EZGET('CCCH_MR_BEAMPIPE_X',CCCH_MR_BEAMPIPE_X,IER)
      CALL EZGET('CCCH_MR_BEAMPIPE_Y',CCCH_MR_BEAMPIPE_Y,IER)
      CCCH_MR_BEAMPIPE_X = CM_PER_INCH * CCCH_MR_BEAMPIPE_X
      CCCH_MR_BEAMPIPE_Y = CM_PER_INCH * CCCH_MR_BEAMPIPE_Y
      CALL EZGET('CCCH_MR_BEAMPIPE_RADIUS',CCCH_MR_BEAMPIPE_RADIUS,IER)
      CCCH_MR_BEAMPIPE_RADIUS = CM_PER_INCH * CCCH_MR_BEAMPIPE_RADIUS
      CALL EZGET('CCCH_MR_BEAMPIPE_LENGTH',CCCH_MR_BEAMPIPE_LENGTH,IER)
      CCCH_MR_BEAMPIPE_LENGTH = CM_PER_INCH * CCCH_MR_BEAMPIPE_LENGTH
C----------------------------------------------------------------------
C  Convert the beam pipe position into the local coordinates of the
C  CCCH_MR_CUTOUT volume.  First save useful sine and cosine.
C----------------------------------------------------------------------
      SINA = SIN((CCCH_MR_MODULE_ANGLE-90.)*RAD)
      COSA = COS((CCCH_MR_MODULE_ANGLE-90.)*RAD)
C----------------------------------------------------------------------
C  Use the global position of the center of the CCCH module containing 
C  the main ring beampipe to compute the global position of the center 
C  of the Floor volume.
C----------------------------------------------------------------------
      X1 = CCCH_MR_MODULE_X - CCCH_MR_FLOOR_OFFSET * SINA
      Y1 = CCCH_MR_MODULE_Y + CCCH_MR_FLOOR_OFFSET * COSA
C----------------------------------------------------------------------
C  Compute the global position of the center of the Cutout volume
C----------------------------------------------------------------------
      X2 = X1 - CCCH_MR_CUTOUT_DX * COSA
      Y2 = Y1 - CCCH_MR_CUTOUT_DX * SINA
      X3 = X2 - CCCH_MR_CUTOUT_DZ * SINA
      Y3 = Y2 + CCCH_MR_CUTOUT_DZ * COSA
C----------------------------------------------------------------------
C  Use the known global position of the beampipe to compute its
C  position in the local system of the Cutout volume
C----------------------------------------------------------------------
      XREL = - COSA * (CCCH_MR_BEAMPIPE_X - X3) -
     &         SINA * (CCCH_MR_BEAMPIPE_Y - Y3)
      ZREL = - SINA * (CCCH_MR_BEAMPIPE_X - X3) +
     &         COSA * (CCCH_MR_BEAMPIPE_Y - Y3)
C----------------------------------------------------------------------
C  Set the rotation matrix for the beampipe
C----------------------------------------------------------------------
      CALL EZGET_i('CCCH_MR_ROTATION_MATRIX',ID_MATRIX,IER)
      VAL_MATRIX(1) = 0.
      VAL_MATRIX(2) = 0.
      VAL_MATRIX(3) = 90.
      VAL_MATRIX(4) = 0.
      VAL_MATRIX(5) = 90.
      VAL_MATRIX(6) = 90.
      CALL STORE_MATRIX
C----------------------------------------------------------------------
C  Set the remaining GEANT SRCP parameters for the beampipe volume
C----------------------------------------------------------------------
      VOLUME_LABEL = 'CCCH_MR_BEAMPIPE_VOLUME'
      CALL EZGET_i('CCCH_MR_BEAMPIPE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL UCTOH('TUBE',VOLUME_SHAPE,4,4)
      CALL EZGET_i('VACUUM_CODE',VOLUME_MATERIAL_CODE,IER)
      CALL EZGET_i('CCCH_MR_CUTOUT_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH('POS',POSITIONING,4,3)
      ROTATION_MATRIX = ID_MATRIX
      COPY_NUMBER = 1
      X_POSITION = XREL
      Y_POSITION = 0.
      Z_POSITION = ZREL
      NUMBER_PARAMS = 3
      PARAM(1) = 0.
      PARAM(2) = CCCH_MR_BEAMPIPE_RADIUS
      PARAM(3) = 0.5 * CCCH_MR_BEAMPIPE_LENGTH
C----------------------------------------------------------------------
C  Write the volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
      RETURN
      END
