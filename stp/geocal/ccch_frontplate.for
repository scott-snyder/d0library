      SUBROUTINE CCCH_FRONTPLATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCCH Front Plate Volume
C-
C-   CCCH Front plate volume
C-      Compute the volume of the CCCH Front plate, which will be
C-      subtracted from the entire module volume in order to compute
C-      the crack volume
C-
C-      A GEANT volume is NOT established for the CCCH frontplate
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
      INCLUDE 'D0$INC:SCCCH_FRONTPLATE.INC'
C  Integers
      INTEGER IER
      INTEGER FIRST_ELEMENT
C  Characters
      CHARACTER*32 NAME
C  Equivalences
      INTEGER IVAL(6)
      REAL RVAL(6)
      EQUIVALENCE ( IVAL, RVAL )
C----------------------------------------------------------------------
C  Determine CCCH Front Plate dimensions:
C  Extract array of info for the first element of the CCCH
C       IVAL(1) = Element name
C       IVAL(2) = Material name
C       RVAL(3) = Inner radius
C       RVAL(4) = Thickness
C       RVAL(5) = Width
C       RVAL(6) = Length
C----------------------------------------------------------------------
      CALL EZGET('CCCH_FIRST_ELEMENT',FIRST_ELEMENT,IER)
      WRITE(NAME,1001) FIRST_ELEMENT
      CALL EZGET(NAME,IVAL,IER)
      CCCH_FRONTPLATE_THICKNESS = RVAL(4)
      CCCH_FRONTPLATE_WIDTH     = RVAL(5)
      CCCH_FRONTPLATE_LENGTH    = RVAL(6)
C----------------------------------------------------------------------
C  Compute volume of Front plate
C----------------------------------------------------------------------
      CCCH_FRONTPLATE_VOLUME = CCCH_FRONTPLATE_WIDTH *
     &                         CCCH_FRONTPLATE_LENGTH *
     &                         CCCH_FRONTPLATE_THICKNESS
      RETURN
 1001 FORMAT('CCCH_ELEMENT_',I3.3)
      END
