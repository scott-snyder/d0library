      SUBROUTINE MSETEF(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the MENUDO event flag. MCLREF can be
C-   used to clear it if needed. The specified COMMAND will be returned
C-   by MENUDO when the event flag is set. Note: Up to 255 commands can
C-   be queued.
C-
C-   Inputs  : COMMAND
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-JUL-1990   Harrison B. Prosper
C-   Updated  29-JUN-1991   Harrison B. Prosper
C-      Add queueing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMMAND
C
C&IF VAXVMS
      INTEGER ISTAT,PTR,KEYPTR
      INTEGER SYS$SETEF
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
C
C **** Set MENUDO event flag
C
      ISTAT = SYS$SETEF (%VAL(EVENT_FLAG))
      IF ( .NOT. ISTAT ) CALL MSGSCR(ISTAT,' ')
C
C ****  Update event queue
C
      PTR = KEYPTR()
      IF ( PTR .GT. 0 ) THEN
        IOCODE_Q(PTR)  = ISTAT
        KEYCODE_Q(PTR) =-1
        COMMAND_Q(PTR) = COMMAND
      ENDIF
C&ENDIF
  999 RETURN
      END
