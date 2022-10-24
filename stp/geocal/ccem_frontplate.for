      SUBROUTINE CCEM_FRONTPLATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCEM Front Plate Volume
C-
C-   CCEM Front plate volume
C-      The Front Plate of an EM module will be a 'BOX' with 
C-      dimensions of the stainless steel front plate.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created    10-DEC-1988   Stuart Fuess   
C-   Updated   6-JAN-1989   Stuart Fuess  Standardize element fetching 
C-   Updated   4-FEB-1990   Stuart Fuess  Use EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCEM_FRONTPLATE.INC'
      INCLUDE 'D0$INC:SCCEM_MODULE.INC'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER FIRST_ELEMENT
C  Reals
      REAL RAD
      REAL  OFFSET
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
C  Determine CCEM Front Plate dimensions:
C  Extract array of info for the first element of the CCEM
C       IVAL(1) = Element name
C       IVAL(2) = Material name
C       RVAL(3) = Inner radius
C       RVAL(4) = Thickness
C       RVAL(5) = Width
C       RVAL(6) = Length
C----------------------------------------------------------------------
      CALL EZGET('CCEM_FIRST_ELEMENT',FIRST_ELEMENT,IER)
      WRITE(NAME,1001) FIRST_ELEMENT
      CALL EZGET_iarr(NAME,IVAL,IER)
      RAD = RVAL(3)
      CCEM_FRONTPLATE_THICKNESS = RVAL(4)
      CCEM_FRONTPLATE_WIDTH     = RVAL(5)
      CCEM_FRONTPLATE_LENGTH    = RVAL(6)
C----------------------------------------------------------------------
C  Compute volume of Front plate
C----------------------------------------------------------------------
      CCEM_FRONTPLATE_VOLUME = CCEM_FRONTPLATE_WIDTH *
     &                         CCEM_FRONTPLATE_LENGTH *
     &                         CCEM_FRONTPLATE_THICKNESS
C----------------------------------------------------------------------
C  Get CCEM Front plate position
C----------------------------------------------------------------------
      OFFSET = - 0.5  * (CCEM_MODULE_INNER_RADIUS +
     &                   CCEM_MODULE_OUTER_RADIUS -
     &                   CCEM_FRONTPLATE_THICKNESS) + RAD
C----------------------------------------------------------------------
C  Set the GEANT SRCP volume parameters for the Front plate volume
C----------------------------------------------------------------------
      VOLUME_LABEL = 'CCEM_FRONTPLATE_VOLUME'
      CALL EZGET('CCEM_FRONTPLATE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL UCTOH('BOX',VOLUME_SHAPE,4,3)
      CALL EZGET('STAINLESS_STEEL_CODE',VOLUME_MATERIAL_CODE,IER)
      CALL EZGET('CCEM_MODULE_VOLUME_NAME',VOLUME_MOTHER,IER)
      CALL UCTOH('POS',POSITIONING,4,3)
      ROTATION_MATRIX   = 1
      COPY_NUMBER       = 1
      X_POSITION        = 0.0
      Y_POSITION        = 0.0
      Z_POSITION        = CM_PER_INCH * OFFSET
      NUMBER_PARAMS     = 3
      PARAM(1)          = 0.5 * CM_PER_INCH * CCEM_FRONTPLATE_WIDTH
      PARAM(2)          = 0.5 * CM_PER_INCH * CCEM_FRONTPLATE_LENGTH
      PARAM(3)          = 0.5 * CM_PER_INCH * CCEM_FRONTPLATE_THICKNESS
C----------------------------------------------------------------------
C  Write the Front plate volume GEANT SRCP parameter description
C----------------------------------------------------------------------
      CALL WRITE_VOLUME
      RETURN
 1001 FORMAT('CCEM_ELEMENT_',I3.3)
      END
