      SUBROUTINE KEYAST (PBID,DISPID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : AST routine to queue a keystroke.
C-
C-   Inputs  : PBID        Pasteboard ID
C-             DISPID      Display ID
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-JUL-1990   Harrison B. Prosper
C-   Updated  29-JUN-1991   Harrison B. Prosper
C-      Add queueing
C-   Updated  20-OCT-1991   Harrison B. Prosper
C-    Add key translation
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER PBID    ! Pasteboard ID
      INTEGER DISPID
C
C&IF VAXVMS
      INTEGER KEY_CODE,ROW,COL
      INTEGER ISTAT,TCODE,PTR,KEYPTR
      INTEGER MODIFIERS
      INTEGER SMG$READ_STRING
      INTEGER SMG$READ_KEYSTROKE
      INTEGER SMG$READ_LOCATOR
      INTEGER SMG$KEYCODE_TO_NAME
      INTEGER SYS$SETEF
C
      CHARACTER*16 INPUT,NAMKEY,EDIT_KEY(6)
      DATA EDIT_KEY/
     &  'FIND','INSERT','REMOVE','SELECT',
     &  'PREV SCREEN','NEXT SCREEN'/
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:KEYCOM.INC'
      INCLUDE '($SMGDEF)'
      INCLUDE '($TRMDEF)'
C----------------------------------------------------------------------
C
C **** Get keystroke
C
       ISTAT = SMG$READ_KEYSTROKE(KEYBDID,TCODE,,,DISPID)
C
C ****  Update event queue
C
      PTR = KEYPTR()
      IF ( PTR .GT. 0 ) THEN
        IOCODE_Q(PTR)  = ISTAT
        KEYCODE_Q(PTR) = TCODE
        ISTAT = SMG$KEYCODE_TO_NAME(TCODE,NAMKEY)
        COMMAND_Q(PTR) = NAMKEY
C
C ****  Check whether to return keycode as a command
C ****  via MENUDO
C
        IF     ( (TCODE .GE. SMG$K_TRM_KP0) .AND.
     &           (TCODE .LE. SMG$K_TRM_PERIOD) ) THEN
          IF ( BTEST(KEY_MODE,0) ) THEN
            KEYCODE_Q(PTR) =-TCODE
            CALL KEYNAM(TCODE,COMMAND_Q(PTR))
          ENDIF
        ELSEIF ( (TCODE .GE. SMG$K_TRM_F6) .AND.
     &           (TCODE .LE. SMG$K_TRM_F20) ) THEN
          IF ( BTEST(KEY_MODE,1) ) THEN
            KEYCODE_Q(PTR) =-TCODE
          ENDIF
        ELSEIF ( (TCODE .GE. SMG$K_TRM_FIND) .AND.
     &           (TCODE .LE. SMG$K_TRM_NEXT_SCREEN) ) THEN
          IF ( BTEST(KEY_MODE,2) ) THEN
            KEYCODE_Q(PTR) =-TCODE
            COMMAND_Q(PTR) = EDIT_KEY(TCODE+1-SMG$K_TRM_FIND)
          ENDIF
        ELSEIF (
     &          ((TCODE .GE. SMG$K_TRM_PF1)   .AND.
     &           (TCODE .LE. SMG$K_TRM_PF4))  ) THEN
          IF ( BTEST(KEY_MODE,3) ) THEN
            KEYCODE_Q(PTR) =-TCODE
            CALL KEYNAM(TCODE,COMMAND_Q(PTR))
          ENDIF
        ELSEIF (
     &          ((TCODE .GE. SMG$K_TRM_UP)   .AND.
     &           (TCODE .LE. SMG$K_TRM_RIGHT))  ) THEN
          IF ( BTEST(KEY_MODE,4) ) THEN
            KEYCODE_Q(PTR) =-TCODE
          ENDIF
        ELSEIF (
     &           (TCODE .NE. SMG$K_TRM_CTRLW) .AND.
     &           (TCODE .NE. SMG$K_TRM_CTRLC) .AND.
     &           (TCODE .NE. SMG$K_TRM_CTRLY)
     &            ) THEN
C
C ****  Return all other key-board keys; except CTRLW, CTRLC, CTRLY
C
          IF ( BTEST(KEY_MODE,5) ) THEN
            KEYCODE_Q(PTR) =-TCODE
          ENDIF
        ENDIF
      ENDIF
C
C **** Purge the input buffer
C
      MODIFIERS = TRM$M_TM_NOECHO .OR.
     &            TRM$M_TM_PURGE
      ISTAT = SMG$READ_STRING(KEYBDID,INPUT,,0,MODIFIERS)
      IF ( .NOT. ISTAT ) CALL MSGSCR(ISTAT,' ')
C
C **** Set MENUDO event flag
C
      ISTAT = SYS$SETEF (%VAL(EVENT_FLAG))
      IF ( .NOT. ISTAT ) CALL MSGSCR(ISTAT,' ')
C
C&ENDIF
  999 RETURN
      END
