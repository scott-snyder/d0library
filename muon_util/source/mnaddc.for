      SUBROUTINE MNADDC( ICELL, IMOD, ISCN )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert cell id to module and scintillator id
C-
C-   Inputs  : ICELL : chamber_id*256 + cell_id
C-   Outputs : IMOD  : PDT module ID
C-             ISCN  : scintillator ID ( 1,2,3,,,8 )
C-   Controls: None
C-
C-   Created  24-FEB-1994   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ICELL, IMOD, ISCN
      INTEGER  NWIR, NCEL
C----------------------------------------------------------------------
      IMOD = ICELL/256
      NCEL = ICELL - IMOD*256
      NWIR = NCEL/4
      ISCN= NWIR/2 + 1
C
  999 RETURN
      END
