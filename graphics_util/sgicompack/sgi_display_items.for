      SUBROUTINE DISPLAY_ITEMS(MAXITM,ITEMS,REM,TOPS,IPOSI)
C--------------------------------------------------------------------------
C-
C-   Purpose and Methods: Displays given items and allows selection of one.
C-   If no selection made IPOSI will return the value of 0.
C-
C-   Inputs  : MAXITM  [I]: Maximum number of items to selecte from
C-             ITEMS[C(*)]: Array with names of the items
C-             REM [C*(*)]: Array with the remarks of each item for help
C-             TOPS   [C*]: Title to put on top of display
C-
C-   Outputs : IPOSI   [I]: Number of the selected file. If no selection
C-                          this number is 0.
C-
C-   Created  Unknown  Mike Shupe
C-
C--------------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) ITEMS(*)
      CHARACTER*(*) REM(*)
      CHARACTER*(*) TOPS
      INTEGER MAXITM,IPOSI
C
      INTEGER MAKEMENU
C--------------------------------------------------------------------------
      IPOSI=MAKEMENU(MAXITM,ITEMS,TOPS)
      RETURN
      END
