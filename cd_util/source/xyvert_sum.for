      FUNCTION XYVERT_SUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modify job summary for XYVERT-package
C-	                   (delete temporary histograms)
C-
C-   Created  08-OCT-1992   Alexandre Zinchenko
C-   Updated  18-JAN-1993   A. Zinchenko - call HCDIR for RZ-file
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL XYVERT_SUM, SEPARATE_RUNS
      INTEGER IER, LUN, XYVERT_LUN
      DATA SEPARATE_RUNS /.FALSE./
C----------------------------------------------------------------------
      XYVERT_SUM = .TRUE.
C
      CALL EZPICK('XYVERT_RCP',IER)
      CALL EZGET('SEPARATE_RUNS',SEPARATE_RUNS,IER)
      CALL EZRSET
C
      CALL HCDIR('//AAABBB',' ')
      CALL DHDIR('XYVERT_RCP','HBOOK_DIRECTORY',IER,' ')
      CALL HDELET(1)
      CALL HDELET(2)
      CALL HDELET(3)
      CALL HDELET(4)
      IF (.NOT.SEPARATE_RUNS) THEN
        CALL HREND('AAABBB')
        LUN = XYVERT_LUN()
        CLOSE(LUN,STATUS='DELETE')
        CALL RLUNIT(600,LUN,IER)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
