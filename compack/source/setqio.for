      SUBROUTINE SETQIO(DSPNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up unsolicited terminal input.
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : DSPNAM: Name of dispatch routine to be called (EXTERNAL)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated     4-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL DSPNAM
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      LOGICAL ISTAT,SMG$ENABLE_UNSOLICITED_INPUT
      EXTERNAL QIOAST
C----------------------------------------------------------------------
      ISTAT=SMG$ENABLE_UNSOLICITED_INPUT(PASTID,QIOAST,DSPNAM)
      IF(.NOT.ISTAT) THEN
        CALL MSGSCR(ISTAT,' ENABLE_UNSOLIC-->')
      ELSE
        QIOFLG=.FALSE.
      ENDIF
      RETURN
      END
