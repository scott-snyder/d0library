      LOGICAL FUNCTION TRD_END_UPDATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Ends updating databases info, required by
C-                          Lars software.
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-DEC-1992   Jean-Francois Glicenstein
C-   Updated  12-APR-1995   Lars Rasmussen, comment out any close
C-                          of dbmon databases.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
ccc      LOGICAL LOK
ccc      CALL DBMU_ENDDM(LOK)
ccc      CALL DBMU_ENDHV(LOK)
      TRD_END_UPDATE = .TRUE.
  999 RETURN
      END
