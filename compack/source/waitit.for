      INTEGER FUNCTION WAITIT(SEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Hibernate for a number of seconds
C-                         VAX-specific
C-
C-   Inputs  : SEC: Number of seconds to hibernate (REAL)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated     6-OCT-1991   Herbert Greenlee
C-   Updated     2-SEP-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL SEC
      INTEGER LIB$WAIT,ISTAT
C----------------------------------------------------------------------
      ISTAT=LIB$WAIT(SEC)                ! Hibernate for SEC seconds 
      IF(ISTAT.GT.1) THEN
        CALL MSGSCR(ISTAT,'LIB$WAIT error->')
      ENDIF
      WAITIT = 0
      RETURN
      END
