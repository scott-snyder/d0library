      FUNCTION QCDJETS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return TRUE if this event should be kept. The
C-                         event will be kept if a QCD trigger of interest
C-                         has fired. (Use QCD_GET_MASK).
C-
C-
C-   Returned value  : TRUE if the event should be kept. FALSE otherwise.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-
C-   Created  11-DEC-1992   Richard V. Astur
C-   Modified 11-SEP-1993   R. Astur "Delete QCDE references"
C-   Modified 5-FEB-1994    R. Astur "Update for 1B"
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'             ! Event zebra store
      INCLUDE 'D0$INC:QCD_BIT_NAMES.INC'      ! QCD bit names
      INCLUDE 'D0$INC:QCD_BIT.INC'            ! QCD bit common arrays
      LOGICAL QCDJETS
C: JETS variables
      INTEGER IER
      INTEGER QCD_MASK, QCD_MASK2
      LOGICAL OK, FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C: Assume we will drop this event
C
      QCDJETS = .FALSE.
C
C: Initialize for this event
C
      CALL QCD_FILTER_BIT_INIT
C
C: Get QCD masks
C
      CALL QCD_GET_MASK( QCD_MASK )
      CALL QCD_GET_MASK2( QCD_MASK2 )
C
C: Bit 14 is always set, so clear it
C
      QCD_MASK  = IBCLR( QCD_MASK, 14)
      QCD_MASK2 = IBCLR( QCD_MASK2, 14 )
C
C: Keep this event if any QCD bits fired
C
      QCDJETS = ( ( QCD_MASK .NE. 0 ) .OR. ( QCD_MASK2 .NE. 0 ))
      RETURN
      END
