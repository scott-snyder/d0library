      SUBROUTINE LIBEFC(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return command associated with the
C-   MENUDO event flag or timeout.
C-
C-   Inputs  : None
C-   Outputs : COMMAND  [C*]    Command to be returned by MENUDO
C-   Controls:
C-
C-   Created   8-AUG-1990   Harrison B. Prosper
C-   Updated  29-JUN-1991   Harrison B. Prosper  
C-      Add queueing 
C-   Updated  31-OCT-1991   Herbert Greenlee  
C-    Remove machine blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*)  COMMAND
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
      IF ( EVENT_PTR .GT. 0 ) THEN
        COMMAND = COMMAND_Q(EVENT_PTR)
      ELSE
        COMMAND = ' '
      ENDIF
  999 RETURN
      END
