      FUNCTION VTXCOFF_PAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get run number for this calibration.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-  ENTRY: VTXC_FIRST: return earliest run processed
C-
C-   Created  16-SEP-1992   M. Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL VTXCOFF_PAR
      INTEGER IER
      LOGICAL FIRST,SET_UNIT_GAIN
      INTEGER RUN, MINRUN, VTXC_FIRST, RUNNO
      SAVE MINRUN
      DATA FIRST /.TRUE./
      DATA MINRUN / 999999 /
C----------------------------------------------------------------------
      VTXCOFF_PAR = .TRUE.
C
      IF (FIRST) THEN
        CALL EZPICK( 'VTXCOFF_RCP' )
        CALL EZGET( 'SET_UNIT_GAIN', SET_UNIT_GAIN, IER )
        CALL EZRSET
        FIRST = .FALSE.
C
C FILL GAIN BANK, VGNL, WITH 1.
C
        IF ( SET_UNIT_GAIN ) THEN
          CALL VTX_UNIT_GAIN
        END IF
      END IF
C
C ****  Check run number; store the earliest run number to be used as a DBL3 key
C
      RUN = RUNNO()
      IF ( RUN .LT. MINRUN ) MINRUN = RUN
C
  999 RETURN
C
      ENTRY VTXC_FIRST()
C
      VTXC_FIRST = MINRUN
C
      RETURN
      END
