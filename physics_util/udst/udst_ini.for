      LOGICAL FUNCTION UDST_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : read in UDST_RCP file
C-
C-   Created   18-JUL-1993   Ulrich Heintz, Meenakshi Narain, V. Balamurali
C-   Updated  10-OCT-1995   Ulrich Heintz initialize WRITE_MICRODST flag  
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
C----------------------------------------------------------------------
      UDST_INI= .TRUE.
      CALL INRCP('UDST_RCP',IER)       ! read in RCP file
      IF(IER.NE.0)CALL ERRMSG('no UDST_RCP file','UDST_INI',' ','F')
C
      CALL EZPICK('UDST_RCP')
      CALL EZERR(IER)
      IF(IER.NE.0)CALL ERRMSG('no UDST_RCP bank','UDST_INI',' ','F')
      CALL EZRSET
C... initialize WRITE_MICRODST flag
      CALL FLGBK('WRITE_MICRODST',1)
      CALL FLGSET('WRITE_MICRODST',.TRUE.)
C----------------------------------------------------------------------
  999 RETURN
      END
