      SUBROUTINE TIMAST (TIMERID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : AST routine for MENUDO timer
C-
C-   Inputs  : TIMERID  [I]     ID of timer
C-   Outputs : None
C-   Controls: None
C-
C-   Created  14-MAY-1991   Harrison B. Prosper
C-   Updated  29-JUN-1991   Harrison B. Prosper  
C-      Add queueing 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TIMERID
C&IF VAXVMS
      INTEGER ISTAT,PTR,KEYPTR
      INTEGER SYS$SETEF
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
C
C ****  Set event flag
C
      ISTAT = SYS$SETEF (%VAL(EVENT_FLAG))
      IF ( .NOT. ISTAT ) CALL MSGSCR(ISTAT,' ')
C
      PTR = KEYPTR()
      IF ( PTR .GT. 0 ) THEN
        IOCODE_Q(PTR)  = ISTAT
        KEYCODE_Q(PTR) =-1
        COMMAND_Q(PTR) = TIMER_COMMAND
      ENDIF
C
C&ENDIF
  999 RETURN
      END
