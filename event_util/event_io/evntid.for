      SUBROUTINE EVNTID(NUMRUN,NUMEVT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-         Return the event ID of the last event, 2 numbers: 
C-         run number and event ouput number
C-   Outputs : 
C-      NUMRUN   = run number
C-      NUMEVT   = event number
C-
C-   Created  18-MAY-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMRUN,NUMEVT
      INTEGER RUNNO,EVONUM
C----------------------------------------------------------------------
      NUMRUN=RUNNO()
      NUMEVT=EVONUM()
  999 RETURN
      END
