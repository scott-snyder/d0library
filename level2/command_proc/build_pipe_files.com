$!========================================================================
$!
$! Name      : BUILD_PIPE_FILES
$!
$! Purpose   : Build PIPE input files to create 3 different OBJ files for 
$!              L2PROD 
$!
$! Arguments : P1 = Group to search to make list
$!
$! Created  26-FEB-1994   Alan M. Jonckheere
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO ERR_EXIT
$   ON CONTROL_Y THEN $ GOTO CTY_EXIT
$
$   WRITE SYS$OUTPUT "*** Start BUILD_PIPE_FILES ''p1' ***"
$
$   if f$search("''p1'.lis") .nes. "" then -
        delete/noconf 'p1'.lis;*
$   cms sh elem/for="#E"/out='p1'.lis 'p1'
$   if f$search("''p1'.lis") .nes. ""
$   then
$       search/out='p1'_for.lis 'p1'.lis ".for"
$       if $status .ne. 1 then delete/noconf 'p1'_for.lis;*
$       search/out='p1'_pas.lis 'p1'.lis ".pas"
$       if $status .ne. 1 then delete/noconf 'p1'_pas.lis;*
$       search/out='p1'_epas.lis 'p1'.lis ".epas"
$       if $status .ne. 1 then delete/noconf 'p1'_epas.lis;*
$       search/out='p1'_c.lis   'p1'.lis ".c"
$       if $status .ne. 1 then delete/noconf 'p1'_c.lis;*
$! save for now $       delete/noconf 'p1'.lis;*
$   endif
$
$NOR_EXIT:
$   WRITE SYS$OUTPUT "*** End BUILD_PIPE_FILES ''p1' ***"
$   EXIT
$CTY_EXIT:
$ERR_EXIT:
$   WRITE SYS$OUTPUT "*** End BUILD_PIPE_FILES /w ERROR ***"
$   EXIT
