      SUBROUTINE LIBSTM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Activate MENUDO timer.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   9-AUG-1990   Harrison B. Prosper
C-   Updated  14-MAY-1991   Harrison B. Prosper  
C-      Activate AST 
C-   Updated  29-JUN-1991   Harrison B. Prosper  
C-      Remove timer flag 
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
C
      INTEGER STATUS
      INTEGER SYS$SETIMR, SYS$CLREF
C
      INCLUDE 'D0$INC:KEYCOM.INC'
      EXTERNAL TIMAST
C----------------------------------------------------------------------
      STATUS = SYS$SETIMR(,
     &                    %REF(BINARY_TIME),
     &                    TIMAST,
     &                    %VAL(TIMER_ID))
      IF ( .NOT. STATUS ) CALL MSGSCR(STATUS,' ')
      TIMER_ON = .TRUE.
C&ENDIF
  999 RETURN
      END
