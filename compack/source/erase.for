      SUBROUTINE ERASE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Clears upper COMPACK window
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created   2-MAR-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GETDEV
      INTEGER USEID
      LOGICAL STATUS
      LOGICAL SMG$ERASE_DISPLAY
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
      IF ( TRMFLG .AND. GETDEV() ) THEN
        IF ( SPLFLG ) THEN
          USEID = MINID1
        ELSE
          USEID = MAINID
        ENDIF
C&IF VAXVMS
        STATUS = SMG$ERASE_DISPLAY (USEID,,,,)
        IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C&ENDIF
      ENDIF
  999 RETURN
      END
