$!------------------------------------------------
$!
$! Name      : SHOWERLIBRARY
$!
$! Purpose   : Runs SHOWERLIBRARY job
$!
$! Arguments : P1 = Tape name
$!
$! Created  13-NOV-1989   Rajendran Raja
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$define beta d0$cms:[showerlibrary]
$SET DEF D0$cms:[SHOWERLIBRARY]
$@setup_showerlibrary 'p1'
$@usr$root2:[raja.tape]exabyte_mount 'p1' mkb100
$showerlibrarynd
$@beta:sub_sort 'p1'    !Sort it as well
$EXIT:
$   EXIT
