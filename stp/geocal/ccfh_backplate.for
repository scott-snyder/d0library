      SUBROUTINE CCFH_BACKPLATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCFH Back Plate Volume
C-
C-   CCFH Back plate volume
C-      Compute the volume of the CCFH Back plate, which will be
C-      subtracted from the entire module volume in order to compute
C-      the crack volume
C-
C-      A GEANT volume is NOT established for the CCFH backplate
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  06-JAN-1989   Stuart Fuess   
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCFH_BACKPLATE.INC'
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
C  Determine CCFH Back Plate dimensions:
C  Extract array of info for the last element of the CCFH
C       IVAL(1) = Element name
C       IVAL(2) = Material name
C       RVAL(3) = Inner radius
C       RVAL(4) = Thickness
C       RVAL(5) = Width
C       RVAL(6) = Length
C----------------------------------------------------------------------
      CALL EZGET('CCFH_LAST_ELEMENT',LAST_ELEMENT,IER)
      WRITE(NAME,1001) LAST_ELEMENT
      CALL EZGET(NAME,IVAL,IER)
      CCFH_BACKPLATE_THICKNESS = RVAL(4)
      CCFH_BACKPLATE_WIDTH     = RVAL(5)
      CCFH_BACKPLATE_LENGTH    = RVAL(6)
C----------------------------------------------------------------------
C  Compute volume of Back plate
C----------------------------------------------------------------------
      CCFH_BACKPLATE_VOLUME = CCFH_BACKPLATE_WIDTH *
     &                        CCFH_BACKPLATE_LENGTH *
     &                        CCFH_BACKPLATE_THICKNESS
      RETURN
 1001 FORMAT('CCFH_ELEMENT_',I3.3)
      END
