      SUBROUTINE CJET_MERGE_PRECLUSTERS (PRE_CLUSTER_I,PRE_CLUSTER_J)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine to which pre-cluster (if any)
C-   cluster I should be connected.
C-
C-   Inputs  : PRE_CLUSTER_I    [I]     Pre-cluster number
C-   Outputs : PRE_CLUSTER_J    [I]     Pre-cluster to be connected
C-   Controls: None
C-
C-   Created  12-DEC-1989   Harrison B. Prosper
C-   Updated  11-JAN-1990   Harrison B. Prosper  
C-      Simplified argument list 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRE_CLUSTER_I,PRE_CLUSTER_J
C----------------------------------------------------------------------
      PRE_CLUSTER_J = 0
  999 RETURN
      END
