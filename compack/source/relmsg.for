      SUBROUTINE RELMSG (LINE,STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Writes output at relative line position LINE
C-
C-   Inputs  : LINE        Line position relative to previously written
C-                         line
C-             STRING      String containing characters to be written
C-
C-   Outputs : None
C-
C-   Created  14-MAR-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL       GETDEV
      INTEGER       STATUS,USEID,N,LEN,LINE
      CHARACTER*(*) STRING
      INTEGER SMG$ERASE_DISPLAY
      INTEGER SMG$SET_CURSOR_REL
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
        STATUS = SMG$SET_CURSOR_REL (USEID,LINE-1,0)
        IF ( STATUS.eq.0 ) CALL LIB$SIGNAL (%VAL(STATUS))
        N = LEN(STRING)
        CALL INTMSG (STRING(1:N))
      ENDIF
  999 RETURN
      END
