      LOGICAL FUNCTION ZTENDR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : performs end run tasks for central tracking
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-JAN-1993   J.Fr. Glicenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK
      LOGICAL TRD_END_UPDATE
      EXTERNAL TRD_END_UPDATE
C     Closes all databases for the TRD
      OK = TRD_END_UPDATE()
C
      ZTENDR = .TRUE.
  999 RETURN
      END
