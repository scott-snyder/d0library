$!========================================================================
$!
$! Name      : PIXIE_HOOKS
$!
$! Purpose   : Make PBD hooks; calls d0$beta_util:make_hooks
$!
$! Arguments : P1   Name of combined package
$!             P2   List of packages
$!
$! Created  15-MAY-1992   Nobuaki Oshima
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   @d0$beta_util:make_hooks "''P1'" "''P2'" PIXIE 
$EXIT:
$   EXIT
