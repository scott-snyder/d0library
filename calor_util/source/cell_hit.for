      SUBROUTINE CELL_HIT (IETA, IPHI, ILYR, E, ET, EX, EY, EZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieve hit information for a given calorimeter
C-   cell.
C-
C-   Inputs  : IETA, IPHI, ILYR [I] indices of the cell in question
C-   Outputs : E    [R] the energy in the cell
C-             ET   [R] the transverse energy in the cell
C-             EX   [R] the X component of the energy
C-             EY   [R] the Y component of the energy
C-             EX   [R] the X component of the energy
C-             EZ   [R] the Z component of the energy
C-   Controls: none
C-
C-   Created  28-AUG-1992   Marc Paterno
C-   Updated   9-SEP-1992   Marc Paterno  Updated to return Ex, Ey, and Ez as
C-                                        well as the previous values
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE  'D0$INC:PTCAEP.INC'
      INTEGER  IETA, IPHI, ILYR
      REAL     E, ET, EX, EY, EZ
C----------------------------------------------------------------------
      INTEGER  LCAEH, GZCAEH, PTR, NREP
      EXTERNAL GZCAEH
C----------------------------------------------------------------------
      E = 0.
      ET = 0.
      EX = 0.
      EY = 0.
      EZ = 0.

      IF ( (IETA .EQ. 0) .OR.
     &     (IPHI .EQ. 0) .OR.
     &     (ILYR .EQ. 0)) RETURN

      LCAEH = GZCAEH()
      IF (LCAEH .LE. 0) RETURN

      NREP = IQ(LCAEH+2)

      ptr = ptcaep(ieta, iphi, ilyr)
      if ( ptr .le. 0 ) return          ! no hit in this cell

      PTR = (PTr - 1) * NREP + LCAEH

      EX = Q(PTR + 4)
      EY = Q(PTR + 5)
      EZ = Q(PTR + 6)
      E  = Q(PTR + 7)
      ET = Q(PTR + 8)

      RETURN
      END
