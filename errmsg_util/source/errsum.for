      SUBROUTINE ERRSUM(SUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print statistics as a list of IDSTRGs and
C-                         number of occurrences.
C-
C-   Inputs  : SUM         Logical unit on which to produce a summary of
C-                         the number of times a given message was issued.
C-
C-   Outputs : Statistics on request.
C-
C-   Controls: None
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I, SUM
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C
      CALL ERRINT

      IF ( NENTRY .GT. 0 ) THEN
         WRITE(UNIT=SUM,FMT=90) NENTRY
         DO 10 I = 1, NENTRY
             WRITE(UNIT=SUM,FMT=100)  ID(I), COUNT(I)
   10    CONTINUE
         IF( OVLCNT.GT.0)WRITE(UNIT=SUM,FMT=400) OVLCNT,MAXSTR
      ENDIF
   90 FORMAT(1X,'Error messages received : ',I4)
  100 FORMAT(1X,A32,1X,I6,' TIMES')
  400 FORMAT(1X,I6,' Messages lost. More than ',I9,' distinct messages')
C
  999 RETURN
      END
