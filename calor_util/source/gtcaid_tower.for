      SUBROUTINE GTCAID_TOWER (IETA, IPHI, NHOT, LYR_FLAG, EHOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the input tower IETA and IPHI indices, return
C-   information on whether or not there are any hot cells which have been
C-   removed from the tower.
C-
C-   Inputs  : IETA [I]   eta index of the tower in question
C-             IPHI [I]   phi index of the tower in question
C-   Outputs : NHOT [I]   the number of hot cells removed from this tower
C-             LYR_FLAG [L*(NLYRL)]   array of flags; LYR_FLAG(i) is .TRUE. if
C-                                 layer i (in tower IETA, IPHI) has been
C-                                 identified as a hot cell and removed.
C-             EHOT [R*(NLYRL)]  array of energies; EHOT(i) is the "hot cell"
C-                            energy in layer i (in tower IETA, IPHI) which has
C-                            been removed
C-   Controls: none
C-
C-   Created   6-MAY-1993   Marc Paterno
C-   Updated  11-NOV-1993   Marc Paterno  Corrected call to GTCAID 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER  IETA, IPHI, NHOT
      LOGICAL  LYR_FLAG(NLYRL)
      REAL     EHOT(NLYRL)
C----------------------------------------------------------------------
      INTEGER  VERS, NREP, NMAX, NCAND, IHIT, IETA_HIT, IPHI_HIT
      INTEGER  ILYR_HIT, I, NHOT_GLOBAL, SCALED_RATIO
      REAL     ET_THRESH, RATIO, ENERGY
      LOGICAL  OK
C----------------------------------------------------------------------
      NHOT = 0
      DO I = 1, NLYRL
        LYR_FLAG (I) = .FALSE.
      ENDDO
      CALL VZERO (EHOT, NLYRL)
C<<
      CALL GTCAID_GLOBAL (VERS, NREP, NMAX, NHOT_GLOBAL, NCAND,
     &  ET_THRESH, RATIO, OK)
C<<
      IF (NHOT .EQ. 0) RETURN           ! no hot cell anywhere
C<<
      DO IHIT = 1, NHOT_GLOBAL
        CALL GTCAID (IHIT, IETA_HIT, IPHI_HIT, ILYR_HIT, SCALED_RATIO,
     &    ENERGY, OK)
        IF (OK) THEN
          IF ( IETA .EQ. IETA_HIT .AND.
     &       IPHI .EQ. IPHI_HIT ) THEN
            LYR_FLAG (ILYR_HIT) = .TRUE.
            EHOT (ILYR_HIT) = ENERGY
            NHOT = NHOT + 1
          ENDIF                         ! if ieta .eq. ieta_hit .and. ...
        ENDIF                           ! if ok
      ENDDO                             ! i = 1, nhot_global
C<<
      RETURN
      END
