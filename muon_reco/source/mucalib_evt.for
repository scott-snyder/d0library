      SUBROUTINE MUCALIB_EVT(MUCALIB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : MUCALIB: defines what processes to run
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-SEP-1991   David Hedin: dummy for now
C-             7-JAN-1992   Eric James:  add t0,dt0 calib calls
C-            27-JAN-1992   Cecilia Gerber: choose modules to calibrate
C-        DH 2/92 add remaining hooks
C   DH 5/92 remove module list init to MUONLY_INI
C   DH 6/92 add calls to ALIGN hooks
C   RM 12/94 add calls to scintillator calibration routines
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MUCALIB
      LOGICAL FIRST, OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C  Book Calib Histograms for first call as requested.
C  ==================================================
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        IF (MUCALIB.EQ.0.OR.MUCALIB.EQ.1) CALL MUBOOK_TZERO
        IF (MUCALIB.EQ.0.OR.MUCALIB.EQ.1) CALL MUBOOK_TTOD
        IF (MUCALIB.EQ.0.OR.MUCALIB.EQ.2) CALL MUBOOK_DTZERO
        IF (MUCALIB.EQ.0.OR.MUCALIB.EQ.3) CALL MUBOOK_PAD
        IF (MUCALIB.EQ.0.OR.MUCALIB.EQ.4) CALL MUBOOK_ALIGN
        IF (MUCALIB.EQ.0.OR.MUCALIB.EQ.5) CALL MUBOOK_SCINT
      ENDIF
C
C  Fill Calib Histograms as requested
C  ==================================
C
      IF(MUCALIB.GE.0.AND.MUCALIB.LE.3) CALL MUHIST_CALIB(MUCALIB)
      IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.4) CALL MUHIST_ALIGN
      IF(MUCALIB.EQ.0.OR.MUCALIB.EQ.5) CALL MUHIST_SCINT(MUCALIB)
C 
  999 RETURN
      END
