      SUBROUTINE GTCAID_CELL (IETA, IPHI, ILYR, EHOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the indices of a cell, tell whether or not it
C-   has been removed as a hot cell, and if so the energy that has been removed.
C-
C-   Inputs  : IETA [I]   eta index of cell in question
C-             IPHI [I]   phi index of cell in question
C-             ILYR [I]   layer index of cell in question
C-   Outputs : HOT  [R]   The energy of the hot cell that has been removed;
C-                        HOT = 0.0 for a cell that is NOT a hot cell.
C-   Controls: none
C-
C-   Created   6-MAY-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IETA, IPHI, ILYR
      REAL     EHOT
C----------------------------------------------------------------------
      INTEGER  IHIT, IETA_HIT, IPHI_HIT, ILYR_HIT, SCALED_RATIO, NHOT
      INTEGER  VERS, NREP, NMAX, NCAND
      REAL     ENERGY, ET_THRESH, RATIO
      LOGICAL  OK
C----------------------------------------------------------------------
      EHOT = 0.0
C<<
      CALL GTCAID_GLOBAL (VERS, NREP, NMAX, NHOT, NCAND, ET_THRESH,
     &  RATIO, OK)
      IF ( .NOT. OK ) RETURN            ! no CAID bank
C<<
      IF ( NHOT .EQ. 0 ) RETURN         ! hot hot cell
C<<
      DO IHIT = 1, NHOT
        CALL GTCAID (IHIT, IETA_HIT, IPHI_HIT, ILYR_HIT, SCALED_RATIO,
     &    ENERGY, OK)
        IF (IETA .EQ. IETA_HIT .AND.
     &      IPHI .EQ. IPHI_HIT .AND.
     &      ILYR .EQ. ILYR_HIT ) THEN
          EHOT = ENERGY                 ! found a hot cell
          RETURN
        ENDIF
      ENDDO                             ! ihit = 1, nhot
      RETURN
      END
