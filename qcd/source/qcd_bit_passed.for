      FUNCTION QCD_BIT_PASSED( QCD_NAME, QCD_MASKDUM )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Logical function returns TRUE if the QCD
C-      filter bit (see D0$INC:QCD_BIT_NAMES.INC) name QCD_NAME passed
C-      this event.
C-
C-   Returned value  : TRUE if event passed the specified filter
C- 
C-   Inputs  : QCD_NAME    [C] : Name of filter bit (e.g. 'JET_END_LOW' )
C-             QCD_MASKDUM [I] : dummy - get mask ourselves
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-JAN-1993   Richard V. Astur
C-   Updated  17-FEB-1994   R. Astur "Add QCD_MASK2, no longer need to
C-                                    pass in QCD_MASK. Find itself"
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) QCD_NAME
      INTEGER QCD_MASKDUM, QCD_BIT
      INTEGER MASK1, MASK2
      LOGICAL QCD_BIT_PASSED
C----------------------------------------------------------------------
      QCD_BIT_PASSED = .FALSE.            ! Assume it did not pass.
C
C: Get masks
C
      CALL QCD_GET_MASK( MASK1 )
      CALL QCD_GET_MASK2( MASK2 )
C
C: Get QCD bit number for this name
C
      CALL QCD_NAME_TO_BIT( QCD_NAME, QCD_BIT )
      IF ( QCD_BIT .GE. 0 .AND. QCD_BIT .LE. 31) THEN
        QCD_BIT_PASSED = BTEST( MASK1, QCD_BIT )
      ELSEIF( QCD_BIT .GE. 32 .AND. QCD_BIT .LE. 63 ) THEN
        QCD_BIT_PASSED = BTEST( MASK2, QCD_BIT-32 )
      ELSE
        CALL ERRMSG('ILLEGAL BIT','QCD_BIT_PASSED',
     &    'ILLEGAL BIT RETURNED', 'W' )
      ENDIF

  999 RETURN
      END
