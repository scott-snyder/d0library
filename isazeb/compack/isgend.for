      LOGICAL FUNCTION ISGEND()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     user hook for ISAJET end-of-run
C-
C-   Created   8-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISA_RUNNO,IRN
      LOGICAL FIRST,DONE,FLGCHK
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      CALL ISAEND
      CALL ISAFFL
      ISGEND=.TRUE.
      IF(FIRST) THEN
        IF(.NOT.FLGCHK('MORE_RUNS')) CALL FLGBK('MORE_RUNS',1)
        FIRST=.FALSE.
      ENDIF
C
C       get setup for more runs
      CALL ISA_STOP(DONE)
      CALL FLGSET('MORE_RUNS',.NOT.DONE)
      CALL ISGZNC
      IRN=ISA_RUNNO()
      CALL ISA_SETRUN(IRN+1)
  999 RETURN
      END
