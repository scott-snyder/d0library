      FUNCTION PU_PUSH_QUEUE(NUM,ELEMENTS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pushes an element(s) into a queue
C-
C-   Inputs  : NUM         [I]: Number of elements to push into the queue
C-             ELEMENTS[C*(*)]: Element(s) to push in the queue
C-             
C-   Outputs : None
C-
C-   Modified  9-OCT-1992   Nobuaki Oshima
C-        Added new ENTRY PU_RESET_QUEUE
C-   Created  30-JUN-1992   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PU_PUSH_QUEUE
C      
      INTEGER NUM
      CHARACTER*(*) ELEMENTS(*)
C
      INTEGER MAXQUE
      PARAMETER( MAXQUE = 200 )
      CHARACTER*80 EVENT_QUEUE(MAXQUE)
C
      INTEGER QUE_PTR,I,J,K,L
C
      SAVE EVENT_QUEUE
      DATA QUE_PTR/0/
C----------------------------------------------------------------------
      LOGICAL PU_POP_QUEUE, PU_RESET_QUEUE
      CHARACTER*(*) POPED_ELEMENT
C----------------------------------------------------------------------
C
C ****  Push the element(s) into the queue
C
      I = 0
      DO WHILE ( (QUE_PTR .LT. MAXQUE) .AND. (I .LT. NUM) )
        I = I + 1
        QUE_PTR = QUE_PTR + 1
        CALL SWORDS(ELEMENTS(I),J,K,L)
        EVENT_QUEUE(QUE_PTR) = ELEMENTS(I)(J:K)
      ENDDO
C
C ****  Prompt a Message if the queue limit reached
C
      IF ( I .LT. NUM ) THEN
        CALL ERRMSG('PIXIE_UTIL','PU_PUSH_QUEUE',
     &    'EVENT_QUEUE LIMIT REACHED','W')
      ENDIF
      RETURN
C
      ENTRY PU_POP_QUEUE(POPED_ELEMENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pops an element of the top of the queue
C-   If the stack is empty PU_POP_QUEUE will return the value .FALSE. 
C-
C-   Inputs  : None
C-   
C-   Outputs : ELEMENT [C*(*)]: Element of the top of the queue.
C-                              
C-   Modified  9-OCT-1992   Nobuaki Oshima 
C-       Fixed a bug( ELEMENTS array was replaced by EVENT_QUEUE array).
C-   Created  30-JUN-1992   Lupe Howell
C-
C----------------------------------------------------------------------
      IF ( QUE_PTR .NE. 0 ) THEN
        CALL SWORDS( EVENT_QUEUE(QUE_PTR), I, J, K )
        POPED_ELEMENT = EVENT_QUEUE(QUE_PTR)(I:J)
        QUE_PTR = QUE_PTR - 1
        PU_POP_QUEUE = .TRUE.
C
C ****  If the queue is empty return with false 
C
      ELSE
        PU_POP_QUEUE = .FALSE.
      ENDIF
      RETURN
C-
C--- Reset a counter QUE_PTR after EXIT from Hardware Rotation
C-
      ENTRY PU_RESET_QUEUE
C-
      QUE_PTR = 0
C
  999 RETURN
      END
