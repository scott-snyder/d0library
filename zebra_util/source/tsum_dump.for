      FUNCTION TSUM_DUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump TSUM Banks using dump facility
C-
C-   Returned value  : TRUE
C-
C-   Created   21-JAN-1992 Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TSUM_DUMP
      EXTERNAL PRTSUM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL DMPBNK('TSUM',.TRUE.)
      ENDIF
C
C ****  DO THIS EVERY EVENT
C
      TSUM_DUMP=.TRUE.
C
      CALL DMPANY('TSUM',PRTSUM)
  999 RETURN
      END
