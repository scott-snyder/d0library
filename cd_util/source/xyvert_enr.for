      FUNCTION XYVERT_ENR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End_Of_Run routine for XYVERT-package
C-
C-
C-   Created  05-OCT-1992   Alexandre Zinchenko
C-   Updated  02-MAR-1993   A. Zinchenko - add "SEPARATE RUNS"
C-   Updated   7-MAR-1994   Susan Blessing  Explicitly set memory directory. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL XYVERT_ENR, FIRST, SEPARATE_RUNS
      INTEGER LUN, IER, XYVERT_LUN
C
      DATA SEPARATE_RUNS/.FALSE./
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      XYVERT_ENR = .TRUE.
      IF (FIRST) THEN
        CALL EZPICK('XYVERT_RCP',IER)
        CALL EZGET('SEPARATE_RUNS',SEPARATE_RUNS,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      CALL XYZFIT
C
      IF (SEPARATE_RUNS) THEN
        CALL DHDIR('XYVERT_RCP','HBOOK_DIRECTORY',IER,' ')
        CALL HCDIR('//AAABBB',' ')
        CALL HREND('AAABBB')
        CALL DHDIR('XYVERT_RCP','HBOOK_DIRECTORY',IER,' ')
        CALL HDELET(1)
        CALL HDELET(2)
        CALL HDELET(3)
        CALL HDELET(4)
        LUN = XYVERT_LUN()
        CLOSE(LUN,STATUS='DELETE')
        CALL RLUNIT(600,LUN,IER)
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
