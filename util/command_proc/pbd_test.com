$!========================================================================
$!
$! Name      : PBD_TEST
$!
$! Purpose   : test correctness of link object list for a pbd file
$!              optionally, produce a load map and/or call tree of the package
$!             Unless the latter option is chosen, all files are
$!                      deleted upon successful completion
$!
$! Arguments : P1   name of the package
$!             P2 = MAP    make a load map 
$!                  TREE   make a load map AND a call tree of the package
$!             P2 or P3 = KEEP   Do not delete ANY files
$!
$! Created  21-JUL-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ IF ("''P2'".NES."KEEP").OR.("''P3'".NES."KEEP") THEN -
   Delete/exclude=(*.tree,*.map) *'P1'_d0user.*;*
$ PBD/frame=d0user/pack='P1'
$ IF ("''P2'".EQS."MAP")
$ THEN
$   d0$beta_util:swap 'P1'_D0USER.lnk nomap "map/cross"
$   DELETE SWAP.log;0
$ ENDIF
$ IF ("''P2'".EQS."TREE")  
$ THEN 
$    @D0$UTIL:MAKE_TREE 'P1'_D0USER.LNK
$ ELSE
$   @'P1'_D0user.lnk
$ ENDIF
$ IF ("''P2'".NES."KEEP").OR.("''P3'".NES."KEEP") THEN -
   Delete/exclude=(*.tree,*.map) *'P1'_d0user.*;*
$EXIT:
$   EXIT
