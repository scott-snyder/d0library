      SUBROUTINE PU_GET_SCREEN_ID(PACKAGE,SCREEN,SCREEN_ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return an id which uniquely identifies the
C-   package and screen. Use PU_GET_ACTIVE_SCREEN(SCREEN) to return
C-   the current screen amd PU_ACTIVE_PACKAGE to return the current
C-   package.
C-
C-   Inputs  : PACKAGE  [C*]    Package
C-             SCREEN   [C*]    Screen
C-   Outputs : SCREEN_ID[I]     Package/Screen ID
C-   Controls: None
C-
C-   Created  20-MAY-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PACKAGE,SCREEN
      INTEGER SCREEN_ID
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      CHARACTER*80 LIST(MAXSCREENS)
      INTEGER IDLIST(MAXSCREENS),NLIST
C----------------------------------------------------------------------
      CHARACTER*32 PACKG
      CHARACTER*40 SCREN
      CHARACTER*80 ACTION,OLD_ACTION
      INTEGER OLD_SCREEN_ID,II
      LOGICAL NEW
      SAVE LIST,IDLIST,NLIST,OLD_ACTION,OLD_SCREEN_ID
      DATA NLIST/0/                     ! To initialize list
C----------------------------------------------------------------------
      PACKG  = PACKAGE(1:LEN(PACKAGE))
      SCREN  = SCREEN(1:LEN(SCREEN))
      ACTION = PACKG//SCREN
C
      IF ( ACTION .NE. OLD_ACTION ) THEN
        CALL LOCSTR1(ACTION,LIST,IDLIST,NLIST,NEW,II)
        SCREEN_ID     = IDLIST(II)
        OLD_ACTION    = ACTION
        OLD_SCREEN_ID = SCREEN_ID
      ELSE
        SCREEN_ID = OLD_SCREEN_ID
      ENDIF
  999 RETURN
      END
