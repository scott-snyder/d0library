      FUNCTION FIT_TWO_END 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-FEB-1993   Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'D0$INC:FIT_TWO.INC'
      LOGICAL FIT_TWO_END
C----------------------------------------------------------------------
      CALL HPRINT(0)
      CALL HRPUT(O,HFILE,'NT')
      CALL CLOSE_NTUPLE
      FIT_TWO_END=.TRUE.
  999 RETURN
      END
