      SUBROUTINE CCEM_BACKPLATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCEM Back Plate Volume
C-
C-   CCEM Back plate volume
C-      Compute the volume of the CCEM Back plate, which will be
C-      subtracted from the entire module volume in order to compute
C-      the crack volume
C-
C-      A GEANT volume is NOT established for the CCEM backplate
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created    28-DEC-1988   Stuart Fuess   
C-   Updated   4-FEB-1990   Stuart Fuess  Use EZ routines 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCEM_BACKPLATE.INC'
C  Integers
      INTEGER IER
      INTEGER LAST_ELEMENT
C  Characters
      CHARACTER*32 NAME
C  Equivalences
      INTEGER IVAL(6)
      REAL RVAL(6)
      EQUIVALENCE ( IVAL, RVAL )
C----------------------------------------------------------------------
C  Determine CCEM Back Plate dimensions:
C  Extract array of info for the last element of the CCEM
C       IVAL(1) = Element name
C       IVAL(2) = Material name
C       RVAL(3) = Inner radius
C       RVAL(4) = Thickness
C       RVAL(5) = Width
C       RVAL(6) = Length
C----------------------------------------------------------------------
      CALL EZGET('CCEM_LAST_ELEMENT',LAST_ELEMENT,IER)
      WRITE(NAME,1001) LAST_ELEMENT
      CALL EZGET(NAME,IVAL,IER)
      CCEM_BACKPLATE_THICKNESS = RVAL(4)
      CCEM_BACKPLATE_WIDTH     = RVAL(5)
      CCEM_BACKPLATE_LENGTH    = RVAL(6)
C----------------------------------------------------------------------
C  Compute volume of Back plate
C----------------------------------------------------------------------
      CCEM_BACKPLATE_VOLUME = CCEM_BACKPLATE_WIDTH *
     &                        CCEM_BACKPLATE_LENGTH *
     &                        CCEM_BACKPLATE_THICKNESS
      RETURN
 1001 FORMAT('CCEM_ELEMENT_',I3.3)
      END
