      FUNCTION PUTMSG(LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called to output message to screen from special
C-                         error handler routines.
C-
C-   Inputs  : LINE : Line to be put on screen
C-   Outputs : None
C-
C-   Created  10-JUN-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PUTMSG
      CHARACTER*(*) LINE
      CHARACTER*132 CTEMP
C----------------------------------------------------------------------
      CTEMP = ' '//LINE(1:LEN(LINE))//CHAR(7)
      CALL INTMSG(CTEMP)
      PUTMSG=0
  999 RETURN
      END
