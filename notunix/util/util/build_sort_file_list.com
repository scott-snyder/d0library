$!------------------------------------------------
$!
$! Name      : BUILD_SORT_FILE_LIST
$!
$! Purpose   : Build SORT_FILE_LIST.EXE
$!
$! Arguments : none
$!
$! Created   3-MAY-1993   Harrison B. Prosper 
$!
$!------------------------------------------------
$!   ON ERROR     Then Goto EXIT
$!   ON CONTROL_Y Then Goto EXIT
$
$   WRITE SYS$OUTPUT "BUILD_SORT_FILE_LIST: Building program SORT_FILE_LIST"
$   LINK/NOMAP/EXE=D0$UTIL:SORT_FILE_LIST.EXE -
        D0$UTIL:UTIL/INCLUDE=(SORT_FILE_LIST)/LIB,-
        D0$GENERAL:GENERAL/LIB,-
        'CERNP'
$   WRITE SYS$OUTPUT "BUILD_SORT_FILE_LIST: Done!"
$
$! EXIT:
$!   EXIT
