      FUNCTION VTRSUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of job processing for VTRAKS package
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-NOV-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL VTRSUM, CALLHST
      INTEGER IER
C----------------------------------------------------------------------
      CALL EZPICK('VTRAKS_RCP')
      CALL EZGET('CALLHST',CALLHST,IER)
      CALL EZRSET
C
      VTRSUM = .TRUE.
      IF ( CALLHST ) CALL VTXHST_END
C
  999 RETURN
      END
