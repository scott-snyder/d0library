C----------------------------------------------------------------------
C
C   Created   8-APR-1988   Wyatt Merritt
C
C       Contains filenames where the menus for program EXAMINE
C       can be found:
C       MENUFILE(1)     Initialization menu   -|
C       MENUFILE(2)     Processing menu        |->  REQUIRED  
C       MENUFILE(3)     Post-processing menu  -|
C       MENUFILE(4-10)  For users                   OPTIONAL
C----------------------------------------------------------------------
      CHARACTER*10 MENUFILE
C
      COMMON /EXM_MENU_FILES/ MENUFILE(10)
C----------------------------------------------------------------------
