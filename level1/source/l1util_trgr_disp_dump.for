      LOGICAL FUNCTION L1UTIL_TRGR_DISP_DUMP ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform dump of TRGR bank when LSM and resource
C-     information is available.
C-
C-   Returned value  : Success status. Always .TRUE.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-FEB-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CALL L1_FW_AND_CT_DUMP
      L1UTIL_TRGR_DISP_DUMP = .TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
