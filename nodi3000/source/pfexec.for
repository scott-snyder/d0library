      LOGICAL FUNCTION PFEXEC() 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DUMMY for the D0 general event display
C-
C-   Inputs  :
C-   Outputs : NOMORE [L] : .TRUE. if we haven't asked for another event.
C-                          DI3000 is disabled in this case. The calling
C-                          program should then clear it's own flags if needed.
C-
C-   Created  28-JUL-1988   Olivier Callot
C- DUMMY version for NODI000 library
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C
      CALL INTMSG(' EVENT DISPLAY DUMMIED IN THIS VERSION')
      PFEXEC=.FALSE.
  999 RETURN
      END
