      SUBROUTINE DISABL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Cancel unsolicited input from terminal.
C-                         VAX-specific.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,SMG$DISABLE_UNSOLICITED_INPUT
C----------------------------------------------------------------------
      IF(.NOT.QIOFLG) THEN
	 ISTAT=SMG$DISABLE_UNSOLICITED_INPUT(PASTID)
	 IF(ISTAT.eq.0) THEN
	    CALL MSGSCR(ISTAT,' DISABLE_INPUT-->')
	 ENDIF
         QIOFLG=.TRUE.
      ENDIF
      RETURN
      END
