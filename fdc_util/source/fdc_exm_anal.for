      FUNCTION FDC_EXM_ANAL()
C----------------------------------------------------------------------
C-
C-  Description:  Control FDC Examine2 analysis
C-
C-  Created   7-JAN-1991   Susan K. Blessing
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER LRCP,IER
C
      LOGICAL FDC_ONLY
      LOGICAL DAQ
      LOGICAL FDC_EXM_ANAL
      LOGICAL OK
      LOGICAL FLGVAL
      LOGICAL FDFHST_EXM
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
      DATA FDC_ONLY/.FALSE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
C See if optional histograms were requested
        CALL EZLOC('FDC_RCP',LRCP)
        IF (LRCP.GT.0) THEN
          CALL EZPICK('FDC_RCP')
          CALL EZGET('FDC_ONLY',FDC_ONLY,IER)
          CALL EZRSET
        END IF
        FIRST = .FALSE.
      END IF
C
C CHECK EVENT TYPE
      CALL GET_EVENT_TYPE('DAQ',DAQ)
C
      IF (.NOT.DAQ) GO TO 999
C
      OK = FDFHST_EXM()
C
      IF (FDC_ONLY) THEN
        IF (FLGVAL('FDC_HIST')) THEN
          CALL FDC_OPT_HIST
          CALL FLGSET('FDC_HIST',.FALSE.)
        END IF
      END IF
C
      FDC_EXM_ANAL = OK
C
  999 RETURN
      END
