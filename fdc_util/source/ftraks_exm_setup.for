      FUNCTION FTRAKS_EXM_SETUP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-FEB-1990   Susan Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FTRAKS_EXM_SETUP
      LOGICAL OK
      LOGICAL FTRINI,FTRDDF
C----------------------------------------------------------------------
C
      OK = FTRINI ()                    ! Reads FTRAKS.RCP
      OK = FTRDDF () .OR. OK            ! Define banks to dump
      FTRAKS_EXM_SETUP = OK
C
  999 RETURN
      END
