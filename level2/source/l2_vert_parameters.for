      SUBROUTINE L2_VERT_PARAMETERS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get parameters in L2 node for L2_VERT
C-
C-   Inputs  : downloads from L2_VERT_INIT
C-   Outputs : constants for L2_VERT
C-   Controls: 
C-
C-   Created  19-OCT-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL L2_L0_PARAMETERS
      CALL L2_VERTEX_CDC_PARAMETERS
  999 RETURN
      END
