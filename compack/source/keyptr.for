      FUNCTION KEYPTR ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return next available slot in command queue.
C-
C-   Returned value  : Queue Pointer, or -1 if queue is FULL
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  29-JUN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER KEYPTR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
C
C ****  Update queue pointer
C
      QUEUE_FULL = ((QUEUE_PTR+1) .EQ. EVENT_PTR) .OR.
     &             ((QUEUE_PTR .EQ. MAX_PTR) .AND. (EVENT_PTR .EQ. 1))
C
      IF ( QUEUE_FULL ) THEN
        KEYPTR = -1
      ELSE
        QUEUE_PTR = QUEUE_PTR + 1
        IF ( QUEUE_PTR .GT. MAX_PTR ) THEN        ! Circular Queue
          QUEUE_PTR = 1
        ENDIF
        KEYPTR = QUEUE_PTR
      ENDIF
C
  999 RETURN
      END
