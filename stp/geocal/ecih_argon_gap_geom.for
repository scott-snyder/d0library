      SUBROUTINE ECIH_ARGON_GAP_GEOM(FLOOR,STEP,GAP,SIDE,COPY,Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract volume dimensions for ECIH Argon Gap
C-                         and create GEANT volume structures
C-
C-      On the initial entry to this routine the 'constant' parameters
C-      of the volume are extracted from SRCP and stored.  On the first
C-      and subsequent entries, the GEANT volume structure is created.
C-      
C-   Inputs  : FLOOR    Floor number, to be encoded into volume label
C-             STEP     Step number, to be encoded into volume label
C-             GAP      Gap number, to be encoded into volume label
C-             SIDE     Side (of MLB) number, to be encoded into volume
C-                      label
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
      INTEGER SIDE
      INTEGER COPY
      REAL    Z
C  Include files
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
      INTEGER SHAPE
      INTEGER MATERIAL_CODE
      INTEGER MOTHER
      INTEGER POS
C  Reals
      REAL INNER_RADIUS
      REAL OUTER_RADIUS
      REAL THICKNESS
C  Characters
      CHARACTER*2 BASE_NAME
      CHARACTER*4 CHAR_NAME
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C  Data statements
      INTEGER IENTRY
      DATA IENTRY / 0 /
C----------------------------------------------------------------------
C  On first entry extract Argon gap dimension information
C----------------------------------------------------------------------
      IF ( IENTRY .EQ. 0 ) THEN
        IENTRY = 1
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
        CALL EZPICK('ENDCAP')
        CALL EZGET ( 'ECIH_ARGON_GAP_INNER_RADIUS', 
     &    INNER_RADIUS, IER )
        CALL EZGET ( 'ECIH_ARGON_GAP_OUTER_RADIUS', 
     &    OUTER_RADIUS, IER )
        CALL EZGET ( 'ECIH_ARGON_GAP_THICKNESS', 
     &    THICKNESS, IER )
C----------------------------------------------------------------------
C  Extract volume information
C----------------------------------------------------------------------
        CALL EZGETS ( 'ECIH_ARGON_GAP_VOLUME_BASE_NAME', 1, 
     &    BASE_NAME, LEN, IER )
        CALL UCTOH ( 'TUBE', SHAPE, 4, 4 )
        CALL EZGET ( 'LIQUID_ARGON_CODE', MATERIAL_CODE, IER )
        CALL EZGET ( 'ECIH_MODULE_VOLUME_NAME', MOTHER, IER )
        CALL UCTOH ( 'POS', POS, 4, 3 )
      ENDIF
C----------------------------------------------------------------------
C  Encode the FLOOR, STEP, GAP, INDEX, and SIDE  numbers into the 
C  volume label
C----------------------------------------------------------------------
      WRITE(VOLUME_LABEL,1001) FLOOR,STEP,GAP,SIDE
C----------------------------------------------------------------------
C  Encode the FLOOR number into the volume name
C----------------------------------------------------------------------
      WRITE(CHAR_NAME,1002) BASE_NAME,FLOOR,STEP
      CALL UCTOH ( CHAR_NAME, VOLUME_NAME, 4, 4 )
C----------------------------------------------------------------------
C  Copy the constant parameters into the volume description
C----------------------------------------------------------------------
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
C  Write Argon gap volume
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Increment Z position
C----------------------------------------------------------------------
      Z = Z + THICKNESS
  999 RETURN
 1001 FORMAT('ECIH_ARGON_GAP_',4I1,'_VOLUME')
 1002 FORMAT(A2,2I1)
      END
