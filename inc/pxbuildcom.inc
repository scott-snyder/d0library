C----------------------------------------------------------------------
C-
C-   Created  12-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INTEGER LUN
      PARAMETER( LUN     = 80 )
C
      INTEGER MAXMENU
      PARAMETER( MAXMENU = 30 )
C
      INTEGER MAXLIST
      PARAMETER( MAXLIST = 50 )
C
      INTEGER MAXLINE
      PARAMETER( MAXLINE = 100)
C
      INTEGER MENU_COUNT
      INTEGER ACTION_COUNT(MAXMENU,MAXLIST)
      INTEGER COMBINED_COUNT(MAXMENU)
      INTEGER SUBMENU_COUNT(MAXMENU)
      INTEGER RCPFILE_COUNT
      INTEGER RCPFILE_INDX(MAXMENU)
      CHARACTER*32 MENU_NAME(MAXMENU),
     &             ACTION_BANK(MAXMENU,MAXLIST,MAXLIST),
     &             ACTION_BANK_COMBINED(MAXLIST),
     &             ACTION_NAME(MAXMENU,MAXLIST,MAXLIST),
     &             ACTION_ITEM(MAXMENU,MAXLIST,MAXLIST),
     &             ACTION_COMMAND(MAXMENU,MAXLIST,MAXLIST),
     &             ACTION_COMBINED(MAXMENU,MAXLIST),
     &             ACTION_SUBMENU(MAXMENU,MAXLIST),
     &             RCPFILE_NAME(MAXMENU),
     &             PACKAGE_NAME(MAXMENU)
      CHARACTER*80 LINE(MAXLINE),RECORD(MAXLINE)
      INTEGER LLINE(MAXLINE),NRECORD
C
      COMMON /PUBUILDCOM/ NRECORD,
     &                    MENU_COUNT,ACTION_COUNT,
     &                    COMBINED_COUNT,SUBMENU_COUNT,
     &                    RCPFILE_COUNT,
     &                    RCPFILE_INDX,
     &                    LLINE,

     &                    MENU_NAME,
     &                    ACTION_BANK,
     &                    ACTION_NAME,
     &                    ACTION_ITEM,
     &                    ACTION_COMMAND,
     &                    ACTION_COMBINED,
     &                    ACTION_SUBMENU,
     &                    RCPFILE_NAME,
     &                    PACKAGE_NAME,
     &                    LINE,RECORD
