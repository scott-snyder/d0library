      SUBROUTINE MENMOD(MODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Activate MENUDO event flag if mode > 0
C-   and select MENUDO mode.
C-   
C-      Mode 0: Return menu items selected with PF1
C-      Mode 1: as Mode 0 plus return KEYPAD keys as commands.
C-      Mode 2: as Mode 0 plus return FUNCTION keys as commands
C-      Mode 4: as Mode 0 plus return EDIT keys as commands.
C-      Mode 8: Return all PF keys as commands and DISABLE display of
C-              menu.
C-      Mode16: Return CURSOR KEYS as commands.
C-      Mode32: Return all other keys as commands.
C-      
C-      Symbolic Codes in D0$PARAMS:MENMOD.DEF
C-      
C-      Mode 0: CMP_K_NORMAL
C-      Mode 1: CMP_K_KEYPAD
C-      Mode 2: CMP_K_FUNCTION
C-      Mode 4: CMP_K_EDIT
C-      Mode 8: CMP_K_PFKEY
C-      Mode16: CMP_K_CURSOR
C-      Mode32: CMP_K_KEYBOARD
C-
C-   Inputs  : MODE     [I]     MENUDO mode
C-   Outputs : None
C-   Controls: None
C-
C-   Created   1-JUL-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MODE
      LOGICAL SKIP
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
      KEY_MODE = MODE
      IF ( KEY_MODE .GT. 0 ) THEN
        CALL MENUEF(.TRUE.)
C&IF VAXVMS
        SKIP = BTEST(KEY_MODE,3)  ! Check for mendis display
        CALL MENSKP(SKIP)
C&ENDIF
      ELSE
        CALL MENUEF(.FALSE.)
        CALL MENSKP(.FALSE.)
      ENDIF
  999 RETURN
      END
