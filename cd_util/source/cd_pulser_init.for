      LOGICAL FUNCTION CD_PULSER_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization for pulser control for
C-                         CD Examines
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  5-NOV-1990   Susan K. Blessing   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C----------------------------------------------------------------------
C
      CD_PULSER_INIT = .TRUE.
C
      CALL FLGBK('PULSER_CONTROL',1)
C
      CALL MENSET('CDPULSER')
C
  999 RETURN
      END
