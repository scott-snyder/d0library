C----------------------------------------------------------------------
C-
C-   Created  25-JUL-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INTEGER TIMER_ID
      PARAMETER( TIMER_ID = 11111 )
C
      INTEGER MAX_PTR
      PARAMETER( MAX_PTR  = 255)
C
      LOGICAL EVENT_MODE, TIMER_MODE, TIMER_ON, QUEUE_EMPTY, QUEUE_FULL
      INTEGER EVENT_FLAG, KEYBDID, DISPLID, QUEUE_PTR, EVENT_PTR
      INTEGER BINARY_TIME(2), KEY_MODE
C
      INTEGER KEYCODE_Q(MAX_PTR),IOCODE_Q(MAX_PTR)
C
      COMMON /KEYCOM/ EVENT_MODE,
     &                TIMER_MODE,
     &                TIMER_ON,
     &                QUEUE_EMPTY,
     &                QUEUE_FULL,
     &                EVENT_FLAG,
     &                KEY_MODE,
     &                BINARY_TIME,
     &                KEYBDID,
     &                DISPLID,
     &                KEYCODE_Q,
     &                IOCODE_Q,
     &                EVENT_PTR,
     &                QUEUE_PTR
C
      CHARACTER*40 TIMER_COMMAND, COMMAND_Q(MAX_PTR)
      COMMON /KEYCMD/ TIMER_COMMAND, COMMAND_Q
