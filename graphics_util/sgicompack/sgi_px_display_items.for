      SUBROUTINE PX_DISPLAY_ITEMS(MAXITM,ITEMS,REM,TOPS,IPOSI)
C-   Inputs  : MAXITM  [I]: Maximum number of items to selecte from
C-             ITEMS[C(*)]: Array with names of the items
C-             REM [C*(*)]: Array with the remarks of each item for help
C-             TOPS   [C*]: Title to put on top of display
C-   Outputs : POSI    [I]: Number of the selected file. If no selection
C-                          this number is 0.
      CHARACTER*(*) ITEMS(*)
      CHARACTER*(*) REM(*)
      CHARACTER*(*) TOPS
C!!!No help in this silly version.
      IPOSI=MAKEMENU(MAXITM,ITEMS,TOPS)
      RETURN
      END
