      SUBROUTINE FRAME_MENADD(PACKAGE,MENNAM,NOTITL,ITEM,ACTION,HELP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add a menu item for specified package to
C-   the given FRAME menu. Also book a flag with the flag name equal
C-   to the ACTION.
C-
C-   Inputs  : PACKAGE  [C*]    Name of PACKAGE
C-             MENNAM   [C*]    Name of MENU
C-             NOTITL   [L]     Logical flag for title or no title
C-             ITEM     [C*]    Name of item for menu display
C-             ACTION   [C*]    Action verb for the command
C-             HELP     [C*]    Help information for item.
C-   Outputs : None
C-   Controls: None
C-
C-   Created  22-APR-1991   Silvia T. Repond, Harrison B. Prosper
C-      Chip Stewart's code!
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) PACKAGE
      CHARACTER*(*) MENNAM
      LOGICAL NOTITL
      CHARACTER*(*) ITEM
      CHARACTER*(*) ACTION
      CHARACTER*(*) HELP
C----------------------------------------------------------------------
      CHARACTER*2  OFF
      CHARACTER*40 MENU_ITEM,MENU_COMM
      LOGICAL PBD_TEMP_FLAG,ON,FLGCHK
      DATA OFF/'X-'/
C----------------------------------------------------------------------
C
C ****  Check if package is linked-in
C
      IF ( PBD_TEMP_FLAG(PACKAGE(1:LEN(PACKAGE)),ON) ) THEN
C
C ****  Package has been linked-in; now check if it is turned ON
C
        IF ( ON ) THEN
          MENU_ITEM = ITEM(1:LEN(ITEM))
          MENU_COMM = ACTION(1:LEN(ACTION))
        ELSE
          MENU_ITEM = OFF//ITEM(1:LEN(ITEM))
          MENU_COMM = OFF//ACTION(1:LEN(ACTION))
        ENDIF
        CALL MENADD(MENNAM(1:LEN(MENNAM)),NOTITL,
     &    MENU_ITEM,MENU_COMM,HELP(1:LEN(HELP)))
C
C ****  Book flag with name equal to ACTION if not yet booked
C
        IF ( .NOT. FLGCHK(ACTION(1:LEN(ACTION))) ) THEN
          CALL FLGBK(ACTION(1:LEN(ACTION)),1)
        ENDIF
      ENDIF
C
  999 RETURN
      END
