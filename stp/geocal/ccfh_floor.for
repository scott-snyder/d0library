      SUBROUTINE CCFH_FLOOR(FLOOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCFH Floor volume and materials
C-
C-   CCFH floor volume
C-      Each floor of an FH module will be a 'TRD1', with lateral
C-      dimensions such as to enclose the resitive coat sections of
C-      the readout boards, and inner and outer surfaces determined
C-      by the active regions of each floor. The volume is filled
C-      with a mixture representing the relative components.  The
C-      material from the module elements which lie outside of the
C-      floor volume are accumulated as contributing to the 'crack'
C-      mixture.
C-
C-   Inputs  : FLOOR    CCFH Floor number
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-OCT-1988   Stuart Fuess
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER FLOOR
C  Include files
      INCLUDE 'D0$INC:SCCFH_CRACK.INC'
      INCLUDE 'D0$INC:SCCFH_FLOOR.INC'
      INCLUDE 'D0$INC:SCCFH_MODULE.INC'
      INCLUDE 'D0$INC:CC_MATERIAL_CODES.INC'
      INCLUDE 'D0$INC:MATERIAL.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER FIRST_SIGNAL, LAST_SIGNAL
      INTEGER FIRST_ELEMENT, LAST_ELEMENT
      INTEGER ICCFH
      INTEGER CODE
      INTEGER N
      INTEGER LSTRING
C  Reals
      REAL RADA, RADB
      REAL WIDA, WIDB
      REAL LENA, LENB
      REAL RCCUTW, RCCUTL
      REAL  OFFSET
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
C  Determine CCFH volume dimensions:
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
      CALL EZGET_i(NAME,FIRST_ELEMENT,IER)
C----------------------------------------------------------------------
C  Get element number of first signal board in this floor
C----------------------------------------------------------------------
      WRITE(NAME,1002) FLOOR
      CALL EZGET_i(NAME,FIRST_SIGNAL,IER)
C----------------------------------------------------------------------
C  Get element number of last signal board in this floor
C----------------------------------------------------------------------
      WRITE(NAME,1003) FLOOR
      CALL EZGET_i(NAME,LAST_SIGNAL,IER)
C----------------------------------------------------------------------
C  Get element number of last element in this floor
C----------------------------------------------------------------------
      WRITE(NAME,1004) FLOOR
      CALL EZGET_i(NAME,LAST_ELEMENT,IER)
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
      CALL EZGET_iarr(NAME,IVAL,IER)
      CCFH_FLOOR_INNER_RADIUS = RVAL(3)
C----------------------------------------------------------------------
C  Extract array of info for the first signal board
C  Compute radius at board center of first signal board
C  Compute width and length of resistive coat on first signal board
C----------------------------------------------------------------------
      WRITE(NAME,1005) FIRST_SIGNAL
      CALL EZGET_iarr(NAME,IVAL,IER)
      RADA = RVAL(3) + 0.5 * RVAL(4)
      WIDA = RVAL(5) - 2. * RCCUTW
      LENA = RVAL(6) - 2. * RCCUTL
C----------------------------------------------------------------------
C  Extract array of info for the last signal board
C  Compute radius at board center of last signal board
C  Compute width and length of resistive coat on last signal board
C----------------------------------------------------------------------
      WRITE(NAME,1005) LAST_SIGNAL
      CALL EZGET_iarr(NAME,IVAL,IER)
      RADB = RVAL(3) + 0.5 * RVAL(4)
      WIDB = RVAL(5) - 2. * RCCUTW
      LENB = RVAL(6) - 2. * RCCUTL
C----------------------------------------------------------------------
C  Extract array of info for one element beyond the last element of 
C  the floor.  The floor volume outer radius is taken as the inner
C  radius of this next element.
C----------------------------------------------------------------------
      WRITE(NAME,1005) LAST_ELEMENT+1
      CALL EZGET_arr(NAME,IVAL,IER)
      CCFH_FLOOR_OUTER_RADIUS = RVAL(3)
C----------------------------------------------------------------------
C  Compute width and length at inner and outer radii
C----------------------------------------------------------------------
      CCFH_FLOOR_INNER_WIDTH = WIDA + (CCFH_FLOOR_INNER_RADIUS-RADA) / 
     &  (RADB-RADA) * (WIDB-WIDA)
      CCFH_FLOOR_OUTER_WIDTH = WIDA + (CCFH_FLOOR_OUTER_RADIUS-RADA) / 
     &  (RADB-RADA) * (WIDB-WIDA)
      CCFH_FLOOR_INNER_LENGTH = LENA + (CCFH_FLOOR_INNER_RADIUS-RADA) / 
     &  (RADB-RADA) * (LENB-LENA)
      CCFH_FLOOR_OUTER_LENGTH = LENA + (CCFH_FLOOR_OUTER_RADIUS-RADA) / 
     &  (RADB-RADA) * (LENB-LENA)
C----------------------------------------------------------------------
C  Compute volume of floor
C----------------------------------------------------------------------
      CCFH_FLOOR_VOLUME = 0.25 * (CCFH_FLOOR_INNER_WIDTH +
     &                            CCFH_FLOOR_OUTER_WIDTH) *
     &                           (CCFH_FLOOR_INNER_LENGTH +
     &                            CCFH_FLOOR_OUTER_LENGTH) *
     &                           (CCFH_FLOOR_OUTER_RADIUS -
     &                            CCFH_FLOOR_INNER_RADIUS)
C----------------------------------------------------------------------
C  Find offset of floor center from module center
C----------------------------------------------------------------------
      OFFSET = - 0.5  * (CCFH_MODULE_INNER_RADIUS +
     &                   CCFH_MODULE_OUTER_RADIUS -
     &                   CCFH_FLOOR_INNER_RADIUS -
     &                   CCFH_FLOOR_OUTER_RADIUS)
C----------------------------------------------------------------------
C  Determine CCFH materials:
C       Determine the material content of the CCFH volume, plus
C       the material content from the CCFH elements which are
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
        CCFH_FLOOR_VOLUME_INSIDE(CODE) = 0
        CCFH_FLOOR_VOLUME_EXCESS(CODE) = 0
      ENDDO
C----------------------------------------------------------------------
C  Loop over elements within this CCFH floor
C----------------------------------------------------------------------
      DO ICCFH=FIRST_ELEMENT,LAST_ELEMENT
C----------------------------------------------------------------------
C  Extract array of info for this element
C  Get element type, which keys to material properties
C----------------------------------------------------------------------
        WRITE(NAME,1005) ICCFH
        CALL EZGET_iarr(NAME,IVAL,IER)
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
        WIDA = CCFH_FLOOR_INNER_WIDTH +
     &    (RVAL(3) - CCFH_FLOOR_INNER_RADIUS) /
     &    (CCFH_FLOOR_OUTER_RADIUS - CCFH_FLOOR_INNER_RADIUS) *
     &    (CCFH_FLOOR_OUTER_WIDTH  - CCFH_FLOOR_INNER_WIDTH)
        WIDB = CCFH_FLOOR_INNER_WIDTH +
     &    (RVAL(3) + RVAL(4) - CCFH_FLOOR_INNER_RADIUS) /
     &    (CCFH_FLOOR_OUTER_RADIUS - CCFH_FLOOR_INNER_RADIUS) *
     &    (CCFH_FLOOR_OUTER_WIDTH  - CCFH_FLOOR_INNER_WIDTH)
        LENA = CCFH_FLOOR_INNER_LENGTH + 
     &    (RVAL(3) - CCFH_FLOOR_INNER_RADIUS) /
     &    (CCFH_FLOOR_OUTER_RADIUS - CCFH_FLOOR_INNER_RADIUS) *
     &    (CCFH_FLOOR_OUTER_LENGTH - CCFH_FLOOR_INNER_LENGTH)
        LENB = CCFH_FLOOR_INNER_LENGTH +
     &    (RVAL(3) + RVAL(4) - CCFH_FLOOR_INNER_RADIUS) /
     &    (CCFH_FLOOR_OUTER_RADIUS - CCFH_FLOOR_INNER_RADIUS) *
     &    (CCFH_FLOOR_OUTER_LENGTH - CCFH_FLOOR_INNER_LENGTH)
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
          CCFH_FLOOR_VOLUME_INSIDE(CODE) = 
     &          CCFH_FLOOR_VOLUME_INSIDE(CODE) + VOL_ENCLOSED
          CCFH_FLOOR_VOLUME_EXCESS(CODE) = 
     &          CCFH_FLOOR_VOLUME_EXCESS(CODE) + VOL_ELEMENT -
     &          VOL_ENCLOSED
        ELSE
          CCFH_FLOOR_VOLUME_INSIDE(CODE) = 
     &          CCFH_FLOOR_VOLUME_INSIDE(CODE) + VOL_ELEMENT
        ENDIF
      ENDDO
C----------------------------------------------------------------------
C  Get label, name, and material code to be used for this CCFH floor
C----------------------------------------------------------------------
      WRITE(MATERIAL_LABEL,1006) FLOOR
      CALL ADDSTR(MATERIAL_LABEL,'_NAME',NAME,LSTRING)
      CALL EZGET_iarr(NAME,MATERIAL_NAME,IER)
      CALL ADDSTR(MATERIAL_LABEL,'_CODE',NAME,LSTRING)
      CALL EZGET_i(NAME,MATERIAL_CODE,IER)
      CCFH_FLOOR_MATERIAL = MATERIAL_CODE
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
        IF ( CCFH_FLOOR_VOLUME_INSIDE(CODE) .GT. 0. ) THEN
          N = N + 1
          CALL ADDSTR(CODE_CHARACTER(CODE),'_CODE',NAME,LSTRING)
          CALL EZGET_i(NAME,COMPONENT_CODE(N),IER)
          COMPONENT_FRACTION(N) = CCFH_FLOOR_VOLUME_INSIDE(CODE) /
     &                               CCFH_FLOOR_VOLUME
          SUM = SUM + COMPONENT_FRACTION(N)
        ENDIF
      ENDDO
      N = N + 1
      CALL EZGET_i('LIQUID_ARGON_CODE',COMPONENT_CODE(N),IER)
      COMPONENT_FRACTION(N) = 1. - SUM
      NUMBER_COMPONENTS = N
C----------------------------------------------------------------------
C  Write the GEANT SRCP parameters for the material
C----------------------------------------------------------------------
      CALL STORE_MATERIAL
C----------------------------------------------------------------------
C  Set the GEANT SRCP volume parameters for the CCFH floor volume
C----------------------------------------------------------------------
      WRITE(VOLUME_LABEL,1007) FLOOR
      CALL ADDSTR(VOLUME_LABEL,'_NAME',NAME,LSTRING)
      CALL EZGET_i(NAME,VOLUME_NAME,IER)
      CALL UCTOH('TRD1',VOLUME_SHAPE,4,4)
      VOLUME_MATERIAL_CODE = CCFH_FLOOR_MATERIAL
      CALL EZGET_i('CCFH_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH('POS',POSITIONING,4,3)
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      X_POSITION        = 0.0
      Y_POSITION        = 0.0
      Z_POSITION        = CM_PER_INCH * OFFSET
      NUMBER_PARAMS     = 4
      PARAM(1)          = 0.5 * CM_PER_INCH * CCFH_FLOOR_INNER_WIDTH
      PARAM(2)          = 0.5 * CM_PER_INCH * CCFH_FLOOR_OUTER_WIDTH
      PARAM(3)          = 0.5 * CM_PER_INCH * CCFH_FLOOR_INNER_LENGTH
      PARAM(4)          = 0.5 * CM_PER_INCH *
     &          (CCFH_FLOOR_OUTER_RADIUS - CCFH_FLOOR_INNER_RADIUS)
C----------------------------------------------------------------------
C  Write the CCFH Floor volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
      RETURN
 1001 FORMAT('CCFH_FLOOR',I1,'_FIRST_ELEMENT')
 1002 FORMAT('CCFH_FLOOR',I1,'_FIRST_SIGNAL')
 1003 FORMAT('CCFH_FLOOR',I1,'_LAST_SIGNAL')
 1004 FORMAT('CCFH_FLOOR',I1,'_LAST_ELEMENT')
 1005 FORMAT('CCFH_ELEMENT_',I3.3)
 1006 FORMAT('CCFH_FLOOR',I1,'_MATERIAL')
 1007 FORMAT('CCFH_FLOOR',I1,'_VOLUME')
      END
