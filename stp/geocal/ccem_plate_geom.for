      SUBROUTINE CCEM_PLATE_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Plate level CCEM geometry for GEANT
C-
C-            ______________________________
C-            | __________________________ |
C-            | -------------------------- |
C-            | -------------------------- |         CCEM
C-            | -------------------------- <----- Module volume
C-            | -------------------------- |       filled with
C-            |  ------------------------  |      liquid argon
C-             | ------------------------ |
C-             | ------------------------ |
C-             | ------------------------ <----- Plate volumes
C-             |  ----------------------  |
C-              | ---------------------- |
C-              | ---------------------- |      + endplates not shown
C-              |  --------------------  |      + sideplates not shown
C-               | -------------------- |
C-               | -------------------- |
C-               |______________________|
C-
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  30-NOV-1989   Stuart Fuess
C-   Updated   4-DEC-1989   Stuart Fuess  Fill out 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCEM_MODULE.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
      INTEGER LIQUID_ARGON_CODE
      INTEGER STAINLESS_STEEL_CODE
      INTEGER URANIUM_CODE
      INTEGER G10_CODE
      INTEGER ELEMENT
      INTEGER FIRST_ELEMENT
      INTEGER LAST_ELEMENT
      INTEGER MATERIAL_NAME
C  Reals
      REAL CENTRAL_Z
      REAL DX, DY, DZ, LO_Z, HI_Z
      REAL PREVIOUS_DX, PREVIOUS_DY, PREVIOUS_HI_Z
      REAL RCCUTW, RCCUTL
C  Characters
      CHARACTER*4  ELEMENT_NAME
      CHARACTER*4  PREVIOUS_ELEMENT_NAME
      CHARACTER*32 NAME
C  Equivalences
      INTEGER IVAL(6)
      REAL RVAL(6)
      EQUIVALENCE ( IVAL, RVAL )
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )

      integer ihss/4HSS  /
      integer ihg10/4HG10 /
      integer ihu/4HU   /
C----------------------------------------------------------------------
C  Select the CC SRCP file
C  Used both for EZGETs in this routine and in the CCEM_ routines
C----------------------------------------------------------------------
      CALL EZPICK ( 'CENTRAL' )
C----------------------------------------------------------------------
C  Get radial (Y) position of module center
C----------------------------------------------------------------------
      CENTRAL_Z = 0.5 * ( CCEM_MODULE_INNER_RADIUS +
     &                    CCEM_MODULE_OUTER_RADIUS )
C----------------------------------------------------------------------
C  Get size of cutback of resistive coat on signal boards
C----------------------------------------------------------------------
      CALL EZGET ( 'RESIST_COAT_WIDTH_CUTBACK', RCCUTW, IER )
      CALL EZGET ( 'RESIST_COAT_LENGTH_CUTBACK', RCCUTL, IER )
C----------------------------------------------------------------------
C  Set volume parameters that are the same for all elements
C----------------------------------------------------------------------
      CALL UCTOH ( 'BOX', VOLUME_SHAPE, 4, 3 )
      CALL EZGET ( 'CCEM_PLATE_MODULE_VOLUME_NAME', VOLUME_MOTHER, IER)
      CALL UCTOH ( 'POS', POSITIONING, 4, 3 )
      ROTATION_MATRIX = 1
      COPY_NUMBER     = 1
      NUMBER_PARAMS   = 3
      X_POSITION = 0.0
      Y_POSITION = 0.0
C----------------------------------------------------------------------
C  Get material codes for element materials to be encountered
C----------------------------------------------------------------------
      CALL EZGET ( 'LIQUID_ARGON_CODE', LIQUID_ARGON_CODE, IER )
      CALL EZGET ( 'STAINLESS_STEEL_CODE', STAINLESS_STEEL_CODE, IER )
      CALL EZGET ( 'URANIUM_CODE', URANIUM_CODE, IER )
      CALL EZGET ( 'G10_CODE', G10_CODE, IER )
C----------------------------------------------------------------------
C  Get first and last element numbers in CCEM module
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_FIRST_ELEMENT', FIRST_ELEMENT, IER )
      CALL EZGET ( 'CCEM_LAST_ELEMENT', LAST_ELEMENT, IER )
C----------------------------------------------------------------------
C  Initialize parameters of previous element
C----------------------------------------------------------------------
      PREVIOUS_ELEMENT_NAME = ' '
      PREVIOUS_DX = 0.0
      PREVIOUS_DY = 0.0
      PREVIOUS_HI_Z = 0.0
C----------------------------------------------------------------------
C  Loop over elements
C----------------------------------------------------------------------
      DO ELEMENT=FIRST_ELEMENT,LAST_ELEMENT
C----------------------------------------------------------------------
C  Encode element label
C----------------------------------------------------------------------
        WRITE(NAME,1001) ELEMENT
C----------------------------------------------------------------------
C  Extract element info
C       IVAL(1) = Element name
C       IVAL(2) = Material name
C       RVAL(3) = Inner radius
C       RVAL(4) = Thickness
C       RVAL(5) = Width
C       RVAL(6) = Length
C----------------------------------------------------------------------
        CALL EZGETS ( NAME,1,ELEMENT_NAME,LEN,IER )
        CALL EZGET ( NAME, IVAL, IER )
        MATERIAL_NAME = IVAL(2)
        LO_Z  = RVAL(3)
        DZ    = RVAL(4)
        DX    = RVAL(5)
        DY    = RVAL(6)
C----------------------------------------------------------------------
C  Avoid overlapping volumes created by roundoff in raw SRCP file
C----------------------------------------------------------------------
        LO_Z = MAX ( LO_Z, PREVIOUS_HI_Z)
        HI_Z = LO_Z + DZ
C----------------------------------------------------------------------
C  If previous element was a signal board, then need to create a
C  volume representing the argon gap between the signal board and the
C  current element.
C----------------------------------------------------------------------
        IF ( PREVIOUS_ELEMENT_NAME(1:2) .EQ. 'SG' ) THEN
          VOLUME_LABEL = PREVIOUS_ELEMENT_NAME(1:4) // '+_VOLUME'
          CALL UCTOH ( 'A+'//PREVIOUS_ELEMENT_NAME(3:4), 
     &                  VOLUME_NAME, 4, 4 )
          VOLUME_MATERIAL_CODE = LIQUID_ARGON_CODE
          Z_POSITION = CM_PER_INCH * 
     &                  ( 0.5 * (PREVIOUS_HI_Z + LO_Z) - CENTRAL_Z )
          PARAM(1) = 0.5 * CM_PER_INCH * ( PREVIOUS_DX - 2. * RCCUTW )
          PARAM(2) = 0.5 * CM_PER_INCH * ( PREVIOUS_DY - 2. * RCCUTL )
          PARAM(3) = 0.5 * CM_PER_INCH * ( LO_Z - PREVIOUS_HI_Z )
          CALL WRITE_VOLUME
        ENDIF
C----------------------------------------------------------------------
C  If this element is a signal board, then need to create a volume
C  representing the argon gap between the previous element and this
C  signal board.
C----------------------------------------------------------------------
        IF ( ELEMENT_NAME(1:2) .EQ. 'SG' ) THEN
          VOLUME_LABEL = ELEMENT_NAME(1:4) // '-_VOLUME'
          CALL UCTOH ( 'A-'//ELEMENT_NAME(3:4), 
     &                  VOLUME_NAME, 4, 4 )
          VOLUME_MATERIAL_CODE = LIQUID_ARGON_CODE
          Z_POSITION = CM_PER_INCH * 
     &                  ( 0.5 * (PREVIOUS_HI_Z + LO_Z) - CENTRAL_Z )
          PARAM(1) = 0.5 * CM_PER_INCH * ( DX - 2. * RCCUTW )
          PARAM(2) = 0.5 * CM_PER_INCH * ( DY - 2. * RCCUTL )
          PARAM(3) = 0.5 * CM_PER_INCH * ( LO_Z - PREVIOUS_HI_Z )
          CALL WRITE_VOLUME
        ENDIF
C----------------------------------------------------------------------
C  Create a volume representing this element
C----------------------------------------------------------------------
        VOLUME_LABEL = ELEMENT_NAME(1:4) // '_VOLUME'
        CALL UCTOH ( ELEMENT_NAME(1:4), VOLUME_NAME, 4, 4 )
        IF ( MATERIAL_NAME .EQ. iHSS   ) THEN
          VOLUME_MATERIAL_CODE = STAINLESS_STEEL_CODE
        ELSE IF ( MATERIAL_NAME .EQ. iHU    ) THEN
          VOLUME_MATERIAL_CODE = URANIUM_CODE
        ELSE IF ( MATERIAL_NAME .EQ. iHG10  ) THEN
          VOLUME_MATERIAL_CODE = G10_CODE
        ENDIF
        Z_POSITION = CM_PER_INCH * ( 0.5 * (LO_Z + HI_Z) - CENTRAL_Z )
        PARAM(1) = 0.5 * CM_PER_INCH * DX
        PARAM(2) = 0.5 * CM_PER_INCH * DY
        PARAM(3) = 0.5 * CM_PER_INCH * DZ
        CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Retain parameters of current element
C----------------------------------------------------------------------
        PREVIOUS_ELEMENT_NAME = ELEMENT_NAME
        PREVIOUS_DX = DX
        PREVIOUS_DY = DY
        PREVIOUS_HI_Z = HI_Z
      ENDDO
      RETURN
 1001 FORMAT('CCEM_ELEMENT_',I3.3)
      END
