$!========================================================================
$!
$! Name      : SETUP_XFRAME
$!
$! Purpose   : for d0$xframe stuff
$!
$! Arguments : 
$!
$! Created  22-NOV-1992   Drew Baden
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$! get rid of pesky util's logical
$!
$ if f$trnlnm("D0$XFRAME","LNM$PROCESS_TABLE").eqs."D0$UTIL" 
$ then
$   deassign d0$xframe
$ endif
$ define/nolog d0xuid d0$xframe$source:d0x.uid
$ d0x_color :== "define d0xuid d0$xframe$source:d0x.uid"
$ d0x_bw :== "define d0xuid d0$xframe$source:d0xbw.uid"
$inq:
$ inquire ws "Do you want color (C) or black/white (BW)?"
$ ws1 = f$edit(f$extract(0,1,ws),"UPCASE")
$ if ws1.nes."B".and.ws1.nes."C" then goto inq
$ if ws1.eqs."B" then define/nolog d0xuid d0$xframe$source:d0xbw.uid
$ d0x :== $d0$xframe:d0x
$copy sys$input sys$output

d0x     runs the program

if you want to switch from color to b&w or vice versa, either
do a d0setup xframe again and specify, or use the symbols:

d0x_color     

   or

d0x_bw

$
$EXIT:
$   EXIT
