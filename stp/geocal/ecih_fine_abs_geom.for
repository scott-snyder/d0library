      SUBROUTINE ECIH_FINE_ABS_GEOM(FLOOR,STEP,GAP,COPY,Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract volume dimensions for ECIH Fine
C-                         section Absorber plate and create GEANT 
C-                         volume structure
C-
C-      On the initial entry to this routine the 'constant' parameters
C-      of the volume are extracted from SRCP and stored.  On the first
C-      and subsequent entries, the GEANT volume structure is created.
C-      
C-   Inputs  : FLOOR    Floor number, to be encoded into volume label
C-             STEP     Step number, to be encoded into volume label
C-             GAP      Gap number, to be encoded into volume label
C-             COPY     Copy number
C-             Z        Z position of the inner edge of this volume
C-                      relative to the Module volume (inches)
C-   Outputs : Z        Z position of the outer edge of this volume
C-                      relative to the Module volume (inches)
C-   Controls: none
C-
C-   Created  20-MAR-1990   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER FLOOR
      INTEGER STEP
      INTEGER GAP
      INTEGER COPY
      REAL    Z
C  Include files
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
      INTEGER NAME
      INTEGER SHAPE
      INTEGER MATERIAL_CODE
      INTEGER MOTHER
      INTEGER POS
C  Reals
      REAL INNER_RADIUS
      REAL OUTER_RADIUS
      REAL THICKNESS
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C  Data statements
      INTEGER IENTRY
      DATA IENTRY / 0 /
C----------------------------------------------------------------------
C  On first entry extract Fine absorber plate dimension information
C----------------------------------------------------------------------
      IF ( IENTRY .EQ. 0 ) THEN
        IENTRY = 1
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
        CALL EZPICK('ENDCAP')
        CALL EZGET ( 'ECIH_FINE_ABS_INNER_RADIUS', 
     &    INNER_RADIUS, IER )
        CALL EZGET ( 'ECIH_FINE_ABS_OUTER_RADIUS', 
     &    OUTER_RADIUS, IER )
        CALL EZGET ( 'ECIH_FINE_ABS_THICKNESS', 
     &    THICKNESS, IER )
C----------------------------------------------------------------------
C  Extract volume information
C----------------------------------------------------------------------
        CALL EZGET_i ( 'ECIH_FINE_ABS_VOLUME_NAME', NAME, IER )
        CALL UCTOH ( 'TUBE', SHAPE, 4, 4 )
        CALL EZGET_i ( 'URANIUM_CODE', MATERIAL_CODE, IER )
        CALL EZGET_i ( 'ECIH_MODULE_VOLUME_NAME', MOTHER, IER )
        CALL UCTOH ( 'POS', POS, 4, 3 )
      ENDIF
C----------------------------------------------------------------------
C  Encode the FLOOR, STEP, and GAP number into the volume label
C----------------------------------------------------------------------
      WRITE(VOLUME_LABEL,1001) FLOOR,STEP,GAP
C----------------------------------------------------------------------
C  Copy the constant parameters into the volume description
C----------------------------------------------------------------------
      VOLUME_NAME          = NAME
      VOLUME_SHAPE         = SHAPE
      VOLUME_MATERIAL_CODE = MATERIAL_CODE
      VOLUME_MOTHER        = MOTHER
      POSITIONING          = POS
      ROTATION_MATRIX      = 1
      COPY_NUMBER   = COPY
      X_POSITION    = 0.
      Y_POSITION    = 0.
      Z_POSITION    = CM_PER_INCH * ( Z + 0.5 * THICKNESS )
      NUMBER_PARAMS = 3
      PARAM(1)      = CM_PER_INCH * INNER_RADIUS
      PARAM(2)      = CM_PER_INCH * OUTER_RADIUS
      PARAM(3)      = 0.5 * CM_PER_INCH * THICKNESS
C----------------------------------------------------------------------
C  Write Fine absorber plate volume
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Increment Z position
C----------------------------------------------------------------------
      Z = Z + THICKNESS
  999 RETURN
 1001 FORMAT('ECIH_FINE_ABS_',3I1,'_VOLUME')
      END
