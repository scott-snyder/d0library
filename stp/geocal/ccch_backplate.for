      SUBROUTINE CCCH_BACKPLATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCCH Back Plate Volume
C-
C-   CCCH Back plate volume
C-      Compute the volume of the CCCH Back plate, which will be
C-      subtracted from the entire module volume in order to compute
C-      the crack volume
C-
C-      A GEANT volume is NOT established for the CCCH backplate
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  28-DEC-1988   Stuart Fuess   
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCCH_BACKPLATE.INC'
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
C  Determine CCCH Back Plate dimensions:
C  Extract array of info for the last element of the CCCH
C       IVAL(1) = Element name
C       IVAL(2) = Material name
C       RVAL(3) = Inner radius
C       RVAL(4) = Thickness
C       RVAL(5) = Width
C       RVAL(6) = Length
C----------------------------------------------------------------------
      CALL EZGET('CCCH_LAST_ELEMENT',LAST_ELEMENT,IER)
      WRITE(NAME,1001) LAST_ELEMENT
      CALL EZGET(NAME,IVAL,IER)
      CCCH_BACKPLATE_THICKNESS = RVAL(4)
      CCCH_BACKPLATE_WIDTH     = RVAL(5)
      CCCH_BACKPLATE_LENGTH    = RVAL(6)
C----------------------------------------------------------------------
C  Compute volume of Back plate
C----------------------------------------------------------------------
      CCCH_BACKPLATE_VOLUME = CCCH_BACKPLATE_WIDTH *
     &                        CCCH_BACKPLATE_LENGTH *
     &                        CCCH_BACKPLATE_THICKNESS
      RETURN
 1001 FORMAT('CCCH_ELEMENT_',I3.3)
      END
