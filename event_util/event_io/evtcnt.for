      INTEGER FUNCTION EVTCNT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Give event number
C-
C-   ENTRY EVTSET(NEVT)
C-   Input:
C-   NEVT = event number
C-
C-   Created   1-AUG-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER EVTSET,NEVT,N
      SAVE N
      DATA N/0/
C----------------------------------------------------------------------
      EVTCNT=N
      RETURN
C
      ENTRY EVTSET(NEVT)
      N=NEVT
C
  999 RETURN
      END
