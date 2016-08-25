      SUBROUTINE NORMAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Clears and deletes COMPACK pasteboard
C-                         That is, return screen to normal.
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  26-FEB-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GETDEV
      INTEGER STATUS
      INTEGER SMG$ERASE_PASTEBOARD
      INTEGER SMG$DELETE_PASTEBOARD
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
      IF ( TRMFLG .AND. GETDEV() ) THEN
        STATUS = SMG$ERASE_PASTEBOARD (PASTID)
        IF ( STATUS.eq.0 ) CALL LIB$SIGNAL (%VAL(STATUS))
        STATUS = SMG$DELETE_PASTEBOARD (PASTID,0)
        IF ( STATUS.eq.0 ) CALL LIB$SIGNAL (%VAL(STATUS))
      ENDIF
  999 RETURN
      END
