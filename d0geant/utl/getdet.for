      SUBROUTINE GETDET(NAMDET,LSET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Gets parameters of detector set
C-   to be used in DIGI routines 
C-
C-   Inputs  : NAMDET-  SRCP name of detector set
C-             
C-   Outputs : LSET - SRCP array belonging to NAMDET
C-
C-   Controls: None
C-
C-   Created  22-JAN-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAMDET(*)
      CHARACTER*60 NMDET1
C
      INTEGER LEN3,LSET
      DIMENSION LSET(*)
C----------------------------------------------------------------------
      
      CALL SLSRCP('SRCP_REST')          ! Select REST bank
      CALL ADDSTR(NAMDET,'(1)',NMDET1,LEN3)   !Makes it into array format
      CALL GTSRCP(NMDET1,LSET(1),1)
      CALL RSSRCP                       ! Select previously selected bank
C
  999 RETURN
      END
