      FUNCTION ISA_RUNNO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Give a run number during event generation
C-   Returned value  : run number
C-
C-   ENTRY ISA_SETRUN(IRUN) give a run number
C-   Input:
C-   IRUN = run number
C-   Created  25-JUL-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISA_RUNNO,IRN,IRUN
      INTEGER ISA_SETRUN
      SAVE IRN
      DATA IRN/1/
C----------------------------------------------------------------------
C
      ISA_RUNNO=IRN
  999 RETURN
C
      ENTRY ISA_SETRUN(IRUN)
      IRN=IRUN
      RETURN
      END
