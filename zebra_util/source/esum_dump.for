      FUNCTION ESUM_DUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump ESUM Banks 
C-
C-   Returned value  : TRUE
C-
C-   Created   21-JAN-1992 Serban Protopopescu
C-   Updated   19-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ESUM_DUMP
      INTEGER DUNIT,DMPUNI
      LOGICAL FIRST,FLGVAL
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL DMPBNK('ESUM',.TRUE.)
      ENDIF
C
C ****  DO THIS EVERY EVENT
C
      DUNIT=DMPUNI()
        IF(FLGVAL('DMPUSR_ESUM')) THEN
          IF(DUNIT.GT.0) CALL PRESUM(DUNIT,0,0,'ALL',0)
          CALL FLGSET('DMPUSR_ESUM',.FALSE.)
        ENDIF

      ESUM_DUMP = .TRUE.
  999 RETURN
      END
