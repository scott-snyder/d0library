      FUNCTION LIBKEY(KBID,DISPID,TCODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read a keystroke from the terminal. Use
C-   MENUEF(SWITCH) to activate/deactivate the MENU event flag.
C-   This flag can be set either by a key-stroke or by an external process.
C-   Use MSETEF(COMMAND) to set the flag and MCLREF to clear it. The command
C-   specified in the call to MSETEF will be returned by MENUDO.
C-
C-   Inputs  : KBID     [I]     Keyboard ID
C-             DISPID   [I]     ID of display with cursor
C-   Outputs : TCODE    [I]     Code of key struck
C-   Controls: None
C-
C-   Created  22-SEP-1988   Jan S. Hoftun
C-   Updated  25-JUL-1990   Harrison B. Prosper
C-      Added event_flag interruption of MENUDO
C-      and timout
C-   Updated  29-JUN-1991   Harrison B. Prosper  
C-      Add queueing 
C-   Updated  25-OCT-1991   Harrison B. Prosper   
C-   Updated  31-OCT-1991   Herbert Greenlee   
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER LIBKEY
      INTEGER KBID, DISPID, TCODE
      INTEGER SMG$READ_KEYSTROKE
      INTEGER ISTAT
      INTEGER*2 TCODE2
C
C&IF VAXVMS
      INTEGER I
      INTEGER SYS$CLREF
      INTEGER SYS$WAITFR
      INTEGER LIB$GET_EF
C
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:SMGCOM.INC'
      INCLUDE 'D0$INC:KEYCOM.INC'
      INCLUDE '($SSDEF)'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( EVENT_MODE  ) THEN
        IF ( FIRST ) THEN
          FIRST = .FALSE.
C
C **** Get local event flag
C
          ISTAT = LIB$GET_EF(EVENT_FLAG)
          IF ( .NOT. ISTAT ) CALL MSGSCR(ISTAT,' ')
          ISTAT = SYS$CLREF (%VAL(EVENT_FLAG))
          IF ( .NOT. ISTAT ) CALL MSGSCR(ISTAT,' ')
C
          EVENT_PTR = 0                 ! Initialize event pointer
          QUEUE_PTR = 0                 ! Initialize next event pointer
        ENDIF
C
C ****  Reset timer if in timer mode
C
        IF ( TIMER_MODE ) THEN
          CALL LIBSTM                     ! Re-activate timer
        ENDIF
C
        KEYBDID = KBID                  ! Keyboard ID
        DISPLID = DISPID                ! Display ID
C
C ****  If the event queue is empty then wait for an event
C
        QUEUE_EMPTY = EVENT_PTR .EQ. QUEUE_PTR
        IF ( QUEUE_EMPTY ) THEN
          ISTAT = SYS$WAITFR (%VAL(EVENT_FLAG))
          IF ( .NOT. ISTAT ) CALL MSGSCR(ISTAT,' ')
        ENDIF
        ISTAT = SYS$CLREF (%VAL(EVENT_FLAG))
        IF ( .NOT. ISTAT ) CALL MSGSCR(ISTAT,' ')
C
C ****  cancel timer if in timer mode
C
        IF ( TIMER_MODE ) THEN
          CALL LIBCTM                     ! Cancel timer
        ENDIF
C
C ****  Return next event
C
        EVENT_PTR = EVENT_PTR + 1
        IF ( EVENT_PTR .GT. MAX_PTR ) THEN      ! Circular queue
          EVENT_PTR = 1
        ENDIF
C
        TCODE = KEYCODE_Q(EVENT_PTR)
        ISTAT = IOCODE_Q(EVENT_PTR)
C&ELSE
C&        IF(.FALSE.) THEN
C&ENDIF
      ELSE
        ISTAT=SMG$READ_KEYSTROKE(KBID,TCODE2,%VAL(0),%VAL(0),DISPID,
     &                           %VAL(0),%VAL(0))
        TCODE = TCODE2
      ENDIF
      LIBKEY=ISTAT
      RETURN
      END
