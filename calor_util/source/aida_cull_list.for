      SUBROUTINE AIDA_CULL_LIST (LIST, MIN_ET, CUT_RATIO,
     &           NAID, RATIO_LIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a list of pointers to hits in the CAEH bank, we
C-   go through the list and remove those NOT identified as anomalous isolated
C-   energy deposits.
C-
C-   Inputs  : LIST   [I(AIDA_LIST_LENGTH)]  ORIGINAL array of pointers to hits
C-                                           in CAEH
C-             MIN_ET [R]     minimum ET for a cell to be a hot cell
C-             CUT_RATIO  [R] cells with (ave. E_neighbor/E_cand) less than or
C-                            equal to CUT_RATIO are identified as hot
C-   Outputs : LIST   [I(AIDA_LIST_LENGTH)]  MODIFIED array of pointers to BAD
C-                                           hits in CAEH
C-             NAID   [I]     number of cells suppressed
C-             RATIO_LIST [R(AIDA_LIST_LENGTH)] list of ratios for cut cells; 0
C-                                              for uncut cells.
C-   Controls: none
C-
C-   Created  15-MAR-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER  LIST(AIDA_LIST_LENGTH), NAID
      REAL     MIN_ET, CUT_RATIO, RATIO_LIST(AIDA_LIST_LENGTH)
C----------------------------------------------------------------------
      INTEGER  I, IETA, IPHI, ILYR, LCAEH, GZCAEH, NREP, PTR
      EXTERNAL GZCAEH
      LOGICAL  CELL_IS_HOT
      EXTERNAL CELL_IS_HOT
      REAL     RATIO
C----------------------------------------------------------------------
      CALL VZERO (RATIO_LIST, AIDA_LIST_LENGTH)
C
C ****  Go through the list, zeroing all pointers that are not identified as hot
C ****  cells.  We also "count" hot cells by starting out at AIDA_LIST_LENGTH,
C ****  and removing one for every zero entry in LIST.
C
      NAID = AIDA_LIST_LENGTH
      LCAEH = GZCAEH()
      IF ( LCAEH .LE. 0 ) RETURN

      NREP = IQ(LCAEH + 2)


      DO I = 1, AIDA_LIST_LENGTH

        IF (LIST(I) .GT. 0 ) THEN
          PTR = (LIST(I)-1)*NREP + LCAEH
          IETA = IQ(PTR+12)
          IPHI = IQ(PTR+13)
          ILYR = IQ(PTR+14)

          IF ( .NOT.
     &         CELL_IS_HOT ( IETA, IPHI, ILYR,
     &         MIN_ET, CUT_RATIO, RATIO)) THEN
            LIST(I) = 0
            NAID = NAID - 1
          ELSE
            RATIO_LIST(I) = RATIO
          ENDIF                         ! if .not. cell_is_hot
        ELSE
          NAID = NAID - 1
        ENDIF                           ! if list(i) .gt. 0
      ENDDO                             ! i = 1, AIDA_list_length

      RETURN
      END
