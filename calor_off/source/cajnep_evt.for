      FUNCTION CAJNEP_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Event interface for CAJNEP package
C-   Returned value : True
C-
C-   Created  19-NOV-1991   Dhiman Chakraborty
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CAJNEP_EVT
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST )THEN                  ! LOCAL INIT
        FIRST = .FALSE.
      ENDIF
C
      CAJNEP_EVT = .TRUE.
C
      CALL FLJNEP
C
C     Put your codes here.  You may want to use CAJNEP_RCP.
C
  999 RETURN
      END
