      LOGICAL FUNCTION PAGE (QUIT,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform paging in upper COMPACK window.
C-
C-   Returned value :      TRUE if new page required.
C-   Inputs  : QUIT        If TRUE QUIT typing.
C-             BACK        If TRUE return to upper menu level.
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL       GETDEV,QUIT,BACK
      INTEGER       STATUS,USEID,LINE,COL,PFNUM
      CHARACTER*1   ANS
      INTEGER SMG$RETURN_CURSOR_POS
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
      PAGE = .FALSE.
      IF ( TRMFLG .AND. GETDEV() ) THEN
        IF ( SPLFLG ) THEN
          USEID = MINID1
        ELSE
          USEID = MAINID
        ENDIF
C
        STATUS = SMG$RETURN_CURSOR_POS (USEID,LINE,COL)
        IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C
        IF ( LINE .EQ. (SPLLIN-2) ) THEN
          CALL INTMSG 
     &    ('         Press RETURN to continue, PF1 to QUIT')
          CALL GETPAR (1,' ','U',ANS)
          QUIT = PFNUM() .EQ. 1
          BACK = PFNUM() .EQ. 4
          PAGE = .TRUE.
        ENDIF
      ENDIF
  999 RETURN
      END
