C DEC/CMS REPLACEMENT HISTORY, Element SAFLD.FOR
C *1    29-MAY-1991 16:42:07 ABACHI "OLEG EROSHIN: New SAMUS routines"
C DEC/CMS REPLACEMENT HISTORY, Element SAFLD.FOR
      SUBROUTINE SAFLD ( VECT, F )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Produce magnetic field parameters
C-
C-   Inputs  : VECT - initial coordinates
C-   Outputs : F - field components
C-
C-   Created  17-OCT-1985   Hedin
C-   Updated  29-SEP-1990   A.Kiryunin  : include SAMUS toroids' field 
C-   Updated   4-MAR-1992   Daria Zieminska  read field map 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL VECT(3), F(3)
      INTEGER QUAD                     
C----------------------------------------------------------------------
      CALL VZERO(F,3)
C     
      QUAD=13
      IF (VECT(3).GT.0.) QUAD=14
      CALL GTMFLD(QUAD,VECT,F)
  999 RETURN
      END
