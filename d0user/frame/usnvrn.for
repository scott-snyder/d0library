      INTEGER FUNCTION USNVRN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Number of events to run
C-  
C-   ENTRY STNVRN(NEVRUN)  
C-   Input: NEVRUN = value of USNVRN in subsequent calls
C-
C-   Created   8-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NEVRUN,NEVSAV,STNVRN
      SAVE NEVSAV
C----------------------------------------------------------------------
C
      DATA NEVSAV/0/
      USNVRN=NEVSAV
      RETURN
C
      ENTRY STNVRN(NEVRUN)
      NEVSAV=NEVRUN
  999 RETURN
      END
