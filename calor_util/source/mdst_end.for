      LOGICAL FUNCTION MDST_END()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Terminate MDST output. Call at end of run.
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-SEP-1991   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NWR
C----------------------------------------------------------------------
      MDST_END = .TRUE.
C
C ****  TERMINATE THE DST OUTPUT
C
      CALL EVWRITES('QCD',NWR)
      CALL EVCLWO('QCD')
      WRITE(6,888) NWR
  888 FORMAT('  ',I8,' EVENTS WRITTEN OUT ON THE QCD STREAM')

  999 RETURN
      END
