      FUNCTION CD_PULSER_CONTROL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Change to CD pulser control menu if 
C-                         requested.  Used by CD Examines.
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-NOV-1990   Susan K. Blessing
C-   Updated  17-MAY-1993   Susan K. Blessing  The routine ZPULSER was 
C-    changed to ZPULSER_CONTROL.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL CD_PULSER_CONTROL
      LOGICAL FLGVAL
C
C----------------------------------------------------------------------
C
      CD_PULSER_CONTROL = .TRUE.
C
C See if pulser control was requested
      IF (FLGVAL('PULSER_CONTROL')) THEN
        CALL ZPULSER_CONTROL
        CALL FLGSET('PULSER_CONTROL',.FALSE.)
      END IF
C
  999 RETURN
      END
