      SUBROUTINE PRTRGR_SIGN_EXTEND_PT(ENERGY_COUNT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sign extend a 3-byte quantity to 4 bytes.
C-
C-   Inputs  : ENERGY_COUNT     The number to extend (MODIFIED)
C-   Outputs : ENERGY_COUNT     The sign extended number
C-   Controls: none
C-
C-   Created  27-JAN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ENERGY_COUNT
C
      INTEGER SIGN_BIT, FF000000
      PARAMETER (SIGN_BIT = 23, FF000000 = -16777216)
C
C
      IF (BTEST(ENERGY_COUNT, SIGN_BIT) .EQV. .TRUE.) THEN
        ENERGY_COUNT = IOR(ENERGY_COUNT, FF000000)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
