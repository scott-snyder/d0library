      SUBROUTINE CCCH_FLOOR(FLOOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCCH Floor volume and materials
C-
C-   CCCH floor volume
C-      Each floor of a CH module will be a 'TRD1', with lateral
C-      dimensions such as to enclose the resitive coat sections of
C-      the readout boards, and inner and outer surfaces determined
C-      by the active regions of each floor. The volume is filled
C-      with a mixture representing the relative components.  The
C-      material from the module elements which lie outside of the
C-      floor volume are accumulated as contributing to the 'crack'
C-      mixture.  
C-
C-   Inputs  : FLOOR    CCCH Floor number
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-   Updated  10-JAN-1989   Stuart Fuess  Add Main Ring 
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER FLOOR
C  Include files
      INCLUDE 'D0$INC:SCCCH_CRACK.INC'
      INCLUDE 'D0$INC:SCCCH_FLOOR.INC'
      INCLUDE 'D0$INC:SCCCH_MODULE.INC'
      INCLUDE 'D0$INC:SCCCH_MR_BEAMPIPE.INC'
      INCLUDE 'D0$INC:CC_MATERIAL_CODES.INC'
      INCLUDE 'D0$INC:MATERIAL.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER FIRST_SIGNAL, LAST_SIGNAL
      INTEGER FIRST_ELEMENT, LAST_ELEMENT
      INTEGER ICCCH
      INTEGER CODE
      INTEGER N
      INTEGER LSTRING
C  Reals
      REAL RADA, RADB
      REAL WIDA, WIDB
      REAL LENA, LENB
      REAL RCCUTW, RCCUTL
      REAL OFFSET
      REAL VOL_ELEMENT, VOL_ENCLOSED
      REAL SUM
C  Characters
      CHARACTER*32 NAME
C  Equivalences
      INTEGER IVAL(6)
      REAL RVAL(6)
      EQUIVALENCE ( IVAL, RVAL )
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Determine CCCH volume dimensions:
C       Define a volume which is limited radially by the first and
C       last module element which contribute to the 'signal' of
C       the floor, and transversely by the edges of the resistive
C       coat floor on the innermost and outermost signal boards
C  Get size of cutback of resistive coat on signal boards
C----------------------------------------------------------------------
      CALL EZGET('RESIST_COAT_WIDTH_CUTBACK',RCCUTW,IER)
      CALL EZGET('RESIST_COAT_LENGTH_CUTBACK',RCCUTL,IER)
C----------------------------------------------------------------------
C  Get element number of first element in this floor
C----------------------------------------------------------------------
      WRITE(NAME,1001) FLOOR
      CALL EZGET(NAME,FIRST_ELEMENT,IER)
C----------------------------------------------------------------------
C  Get element number of first signal board in this floor
C----------------------------------------------------------------------
      WRITE(NAME,1002) FLOOR
      CALL EZGET(NAME,FIRST_SIGNAL,IER)
C----------------------------------------------------------------------
C  Get element number of last signal board in this floor
C----------------------------------------------------------------------
      WRITE(NAME,1003) FLOOR
      CALL EZGET(NAME,LAST_SIGNAL,IER)
C----------------------------------------------------------------------
C  Get element number of last element in this floor
C----------------------------------------------------------------------
      WRITE(NAME,1004) FLOOR
      CALL EZGET(NAME,LAST_ELEMENT,IER)
C----------------------------------------------------------------------
C  Extract array of info for the first element of the floor
C       IVAL(1) = Element name
C       IVAL(2) = Material name
C       RVAL(3) = Inner radius
C       RVAL(4) = Thickness
C       RVAL(5) = Width
C       RVAL(6) = Length
C  Get inner radius of first element
C----------------------------------------------------------------------
      WRITE(NAME,1005) FIRST_ELEMENT
      CALL EZGET(NAME,IVAL,IER)
      CCCH_FLOOR_INNER_RADIUS = RVAL(3)
C----------------------------------------------------------------------
C  Extract array of info for the first signal board
C  Compute radius at board center of first signal board
C  Compute width and length of resistive coat on first signal board
C----------------------------------------------------------------------
      WRITE(NAME,1005) FIRST_SIGNAL
      CALL EZGET(NAME,IVAL,IER)
      RADA = RVAL(3) + 0.5 * RVAL(4)
      WIDA = RVAL(5) - 2. * RCCUTW
      LENA = RVAL(6) - 2. * RCCUTL
C----------------------------------------------------------------------
C  Extract array of info for the last signal board
C  Compute radius at board center of last signal board
C  Compute width and length of resistive coat on last signal board
C----------------------------------------------------------------------
      WRITE(NAME,1005) LAST_SIGNAL
      CALL EZGET(NAME,IVAL,IER)
      RADB = RVAL(3) + 0.5 * RVAL(4)
      WIDB = RVAL(5) - 2. * RCCUTW
      LENB = RVAL(6) - 2. * RCCUTL
C----------------------------------------------------------------------
C  Extract array of info for one element beyond the last element of 
C  the floor.  The floor volume outer radius is taken as the inner
C  radius of this next element.
C----------------------------------------------------------------------
      WRITE(NAME,1005) LAST_ELEMENT+1
      CALL EZGET(NAME,IVAL,IER)
      CCCH_FLOOR_OUTER_RADIUS = RVAL(3)
C----------------------------------------------------------------------
C  Compute width and length at inner and outer radii
C----------------------------------------------------------------------
      CCCH_FLOOR_INNER_WIDTH = WIDA + (CCCH_FLOOR_INNER_RADIUS-RADA) / 
     &  (RADB-RADA) * (WIDB-WIDA)
      CCCH_FLOOR_OUTER_WIDTH = WIDA + (CCCH_FLOOR_OUTER_RADIUS-RADA) / 
     &  (RADB-RADA) * (WIDB-WIDA)
      CCCH_FLOOR_INNER_LENGTH = LENA + (CCCH_FLOOR_INNER_RADIUS-RADA) / 
     &  (RADB-RADA) * (LENB-LENA)
      CCCH_FLOOR_OUTER_LENGTH = LENA + (CCCH_FLOOR_OUTER_RADIUS-RADA) / 
     &  (RADB-RADA) * (LENB-LENA)
C----------------------------------------------------------------------
C  Compute volume of floor
C----------------------------------------------------------------------
      CCCH_FLOOR_VOLUME = 0.25 * (CCCH_FLOOR_INNER_WIDTH +
     &                            CCCH_FLOOR_OUTER_WIDTH) *
     &                           (CCCH_FLOOR_INNER_LENGTH +
     &                            CCCH_FLOOR_OUTER_LENGTH) *
     &                           (CCCH_FLOOR_OUTER_RADIUS -
     &                            CCCH_FLOOR_INNER_RADIUS)
C----------------------------------------------------------------------
C  Find offset of floor center from module center
C----------------------------------------------------------------------
      OFFSET = 0.5  * (CCCH_FLOOR_INNER_RADIUS +
     &                 CCCH_FLOOR_OUTER_RADIUS -
     &                 CCCH_MODULE_INNER_RADIUS -
     &                 CCCH_MODULE_OUTER_RADIUS)
C----------------------------------------------------------------------
C  Determine CCCH materials:
C       Determine the material content of the CCCH volume, plus
C       the material content from the CCCH elements which are
C       outside of the volume, and hence contribute to the crack
C       material
C  Zero sums of volumes of each type of material
C       1 = Uranium
C       2 = Stainless Steel
C       3 = Copper
C       4 = G10
C       5 = Liquid Argon (fills remaining volume)
C----------------------------------------------------------------------
      DO CODE=1,4
        CCCH_FLOOR_VOLUME_INSIDE(CODE) = 0
        CCCH_FLOOR_VOLUME_EXCESS(CODE) = 0
      ENDDO
C----------------------------------------------------------------------
C  Loop over elements within this CCCH floor
C----------------------------------------------------------------------
      DO ICCCH=FIRST_ELEMENT,LAST_ELEMENT
C----------------------------------------------------------------------
C  Extract array of info for this element
C  Get element type, which keys to material properties
C----------------------------------------------------------------------
        WRITE(NAME,1005) ICCCH
        CALL EZGET(NAME,IVAL,IER)
        CODE = 1
        DO WHILE ( IVAL(2) .NE. CODE_HOLLERITH(CODE) )
          CODE = CODE + 1
        ENDDO
C----------------------------------------------------------------------
C  Compute total volume of element
C----------------------------------------------------------------------
        VOL_ELEMENT = RVAL(4) * RVAL(5) * RVAL(6)
C----------------------------------------------------------------------
C  Compute volume inside and outside of module volume
C----------------------------------------------------------------------
        WIDA = CCCH_FLOOR_INNER_WIDTH +
     &    (RVAL(3) - CCCH_FLOOR_INNER_RADIUS) /
     &    (CCCH_FLOOR_OUTER_RADIUS - CCCH_FLOOR_INNER_RADIUS) *
     &    (CCCH_FLOOR_OUTER_WIDTH  - CCCH_FLOOR_INNER_WIDTH)
        WIDB = CCCH_FLOOR_INNER_WIDTH +
     &    (RVAL(3) + RVAL(4) - CCCH_FLOOR_INNER_RADIUS) /
     &    (CCCH_FLOOR_OUTER_RADIUS - CCCH_FLOOR_INNER_RADIUS) *
     &    (CCCH_FLOOR_OUTER_WIDTH  - CCCH_FLOOR_INNER_WIDTH)
        LENA = CCCH_FLOOR_INNER_LENGTH + 
     &    (RVAL(3) - CCCH_FLOOR_INNER_RADIUS) /
     &    (CCCH_FLOOR_OUTER_RADIUS - CCCH_FLOOR_INNER_RADIUS) *
     &    (CCCH_FLOOR_OUTER_LENGTH - CCCH_FLOOR_INNER_LENGTH)
        LENB = CCCH_FLOOR_INNER_LENGTH +
     &    (RVAL(3) + RVAL(4) - CCCH_FLOOR_INNER_RADIUS) /
     &    (CCCH_FLOOR_OUTER_RADIUS - CCCH_FLOOR_INNER_RADIUS) *
     &    (CCCH_FLOOR_OUTER_LENGTH - CCCH_FLOOR_INNER_LENGTH)
        VOL_ENCLOSED = 0.25 * (WIDA+WIDB) * (LENA+LENB) * RVAL(4)
C----------------------------------------------------------------------
C  If element volume is greater than volume enclosed within module
C  volume, then use enclosed value as 'inside' and difference as
C  'excess'.  If element volume is smaller than volume enclosed
C  within module volume, then use element volume as 'inside' with
C  no 'excess'.  Note that this is an approximation, which inexactly
C  handles the case where the module volume boundary cuts thru the
C  edge of the element boundary.
C----------------------------------------------------------------------
        IF ( VOL_ELEMENT .GT. VOL_ENCLOSED ) THEN
          CCCH_FLOOR_VOLUME_INSIDE(CODE) = 
     &          CCCH_FLOOR_VOLUME_INSIDE(CODE) + VOL_ENCLOSED
          CCCH_FLOOR_VOLUME_EXCESS(CODE) = 
     &          CCCH_FLOOR_VOLUME_EXCESS(CODE) + VOL_ELEMENT -
     &          VOL_ENCLOSED
        ELSE
          CCCH_FLOOR_VOLUME_INSIDE(CODE) = 
     &          CCCH_FLOOR_VOLUME_INSIDE(CODE) + VOL_ELEMENT
        ENDIF
      ENDDO
C----------------------------------------------------------------------
C  Get label, name, and material code to be used for this CCCH floor
C----------------------------------------------------------------------
      WRITE(MATERIAL_LABEL,1006) FLOOR
      CALL ADDSTR(MATERIAL_LABEL,'_NAME',NAME,LSTRING)
      CALL EZGET(NAME,MATERIAL_NAME,IER)
      CALL ADDSTR(MATERIAL_LABEL,'_CODE',NAME,LSTRING)
      CALL EZGET(NAME,MATERIAL_CODE,IER)
      CCCH_FLOOR_MATERIAL = MATERIAL_CODE
C----------------------------------------------------------------------
C  Initialize the number of components
C----------------------------------------------------------------------
      NUMBER_COMPONENTS = 0
C----------------------------------------------------------------------
C  Set the appropriate component codes
C  Compute the relative volumes of the different components
C----------------------------------------------------------------------
      N = 0
      SUM = 0.
      DO CODE=1,4
        IF ( CCCH_FLOOR_VOLUME_INSIDE(CODE) .GT. 0. ) THEN
          N = N + 1
          CALL ADDSTR(CODE_CHARACTER(CODE),'_CODE',NAME,LSTRING)
          CALL EZGET(NAME,COMPONENT_CODE(N),IER)
          COMPONENT_FRACTION(N) = CCCH_FLOOR_VOLUME_INSIDE(CODE) /
     &                               CCCH_FLOOR_VOLUME
          SUM = SUM + COMPONENT_FRACTION(N)
        ENDIF
      ENDDO
      N = N + 1
      CALL EZGET('LIQUID_ARGON_CODE',COMPONENT_CODE(N),IER)
      COMPONENT_FRACTION(N) = 1. - SUM
      NUMBER_COMPONENTS = N
C----------------------------------------------------------------------
C  Write the GEANT SRCP parameters for the material
C----------------------------------------------------------------------
      CALL STORE_MATERIAL
C----------------------------------------------------------------------
C  Set the GEANT SRCP volume parameters for the CCCH Floor volume
C----------------------------------------------------------------------
      CALL UCTOH('TRD2',VOLUME_SHAPE,4,4)
      VOLUME_MATERIAL_CODE = CCCH_FLOOR_MATERIAL
      CALL UCTOH('POS',POSITIONING,4,3)
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      X_POSITION        = 0.0
      Y_POSITION        = 0.0
      Z_POSITION        = CM_PER_INCH * OFFSET
      NUMBER_PARAMS     = 5
      PARAM(1)          = 0.5 * CM_PER_INCH * CCCH_FLOOR_INNER_WIDTH
      PARAM(2)          = 0.5 * CM_PER_INCH * CCCH_FLOOR_OUTER_WIDTH
      PARAM(3)          = 0.5 * CM_PER_INCH * CCCH_FLOOR_INNER_LENGTH
      PARAM(4)          = 0.5 * CM_PER_INCH * CCCH_FLOOR_OUTER_LENGTH
      PARAM(5)          = 0.5 * CM_PER_INCH *
     &          (CCCH_FLOOR_OUTER_RADIUS - CCCH_FLOOR_INNER_RADIUS)
C----------------------------------------------------------------------
C  Write the South Floor volume GEANT SRCP parameter description
C  for both normal and special main ring bypass modules
C----------------------------------------------------------------------
      WRITE(VOLUME_LABEL,1007) FLOOR
      CALL ADDSTR(VOLUME_LABEL,'_NAME',NAME,LSTRING)
      CALL EZGET(NAME,VOLUME_NAME,IER)
      CALL EZGET('CCCH_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL WRITE_VOLUME
C
      WRITE(VOLUME_LABEL,1008) FLOOR
      CALL ADDSTR(VOLUME_LABEL,'_NAME',NAME,LSTRING)
      CALL EZGET(NAME,VOLUME_NAME,IER)
      CALL EZGET('CCCH_MR_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL WRITE_VOLUME
C----------------------------------------------------------------------
C  Save radial offset (in CM) for use in main ring beam pipe 
C  positioning
C----------------------------------------------------------------------
      CCCH_MR_FLOOR_OFFSET = CM_PER_INCH * OFFSET
      RETURN
 1001 FORMAT('CCCH_FLOOR',I1,'_FIRST_ELEMENT')
 1002 FORMAT('CCCH_FLOOR',I1,'_FIRST_SIGNAL')
 1003 FORMAT('CCCH_FLOOR',I1,'_LAST_SIGNAL')
 1004 FORMAT('CCCH_FLOOR',I1,'_LAST_ELEMENT')
 1005 FORMAT('CCCH_ELEMENT_',I3.3)
 1006 FORMAT('CCCH_FLOOR',I1,'_MATERIAL')
 1007 FORMAT('CCCH_FLOOR',I1,'_VOLUME')
 1008 FORMAT('CCCH_MR_FLOOR',I1,'_VOLUME')
      END
