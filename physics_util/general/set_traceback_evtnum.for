C------------------------------------------------------------------------
      LOGICAL FUNCTION SET_TRACEBACK_EVTNUM
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: 
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-May-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
C&ELSE
C&      INCLUDE 'D0$INC:ZEBCOM.INC'
C&C-----------------------------------------------------------------------
C&      CALL SEGV_SET_RUN_EVENT(IQ(LHEAD+6),IQ(LHEAD+9))
C&ENDIF
      SET_TRACEBACK_EVTNUM=.TRUE.
 999  RETURN
      END
