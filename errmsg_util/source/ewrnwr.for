      SUBROUTINE EWRNWR(MSG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : write out a string to a WARNing device
C-        (as defined in the ERROR_MESSAGE system).  This version uses the
C-        screen as a warning device, via INTMSG.
C-
C-   Inputs  : MSG message text, 1st character as carriage control
C-   Outputs : None
C-   Controls: None
C-
C-   Created   3-JAN-1989   James T. Linnemann
C-   Updated  25-Feb-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MSG
      CHARACTER*132 CTEMP
      INTEGER I,J,N
C----------------------------------------------------------------------
C
C...squeeze out leading or trailing blanks

      CALL SWORDS(MSG,I,J,N)    
      IF(N.GT.1)  THEN
C
C...for some devices, it may be necessary to trim down or break up long strings
C...here we assume INTMSG will do this for us
        CTEMP = ' '//MSG(I:J)
        CALL INTMSG(CTEMP(1:J-I+2))
      ENDIF

  999 RETURN
      END
