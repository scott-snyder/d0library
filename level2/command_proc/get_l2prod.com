$!========================================================================
$!
$! Name      : GET_L2PROD
$!
$! Purpose   : copy l2prod release to default directory
$!  this should be d0$prod:[d0production.l2prod]    (use with P1 = prev)
$!              or d0$prod:[d0production.l2prod.new] (with P1 = new )
$!              or d0$prod:[d0production.l2prod.old]  (P1 = old)
$!              or d0$prod:[d0production.l2prod.old.vxx_yy]  (P1= Vxx_yy)   
$!
$! Arguments : P1       '' or 'prev' or 'top' (top dir, accessed with 
$!                                                          libprod/prev)
$!                      'old'   (.old)          (access with libprod/old)
$!                      'new'  (new release)    (access with libprod)
$!                      'Vxx_yy'  (vxx_yy release)
$!                         (for P1 = V02_03, access with eg libprod/old=v02_03)
$!
$! Created  29-MAR-1993   James T. Linnemann
$! Modified 30-JUL-1993   James T. Linnemann support old.Vxx_yy 
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$  say :== write sys$output
$!does not support making of custom .STP files at remote nodes
$   what := *.*/exclude=(regular_*.*,filt_test_*.*,filt_shadow_*.*,*.dir,-
        *.for*,deb_*.*,*_deb_*.*,*.obj,*.lnk,*.const,*.map,FOR00*.DAT,*.opt,-
        vms_filter_stp.exe,filter_end_display.exe,*.pbd,*.dif)
$   if P1.EQS."" then P1 = "PREV"
$   src = "gar$bage"
$   if (P1.EQS."PREV").OR.(P1.EQS."TOP").OR.(P1.EQS."NEW") THEN  -
        src := d0gs06::d0$prod:[d0production.l2prod]    !base release
$   if (P1.EQS."OLD") THEN src := d0gs06::d0$prod:[d0production.l2prod.old]
$   if (f$locate("V",P1).EQ.0) THEN  -
        src := d0gs06::d0$prod:[d0production.l2prod.old.'P1']
$   say "copying ''src'"
$   DIRECTORY/SIZ  'src''what' 
$   copy/log 'src''what' *
$   if (P1.EQS."NEW")
$   THEN    !copy NEW on top of it
$     src := d0gs06::d0$prod:[d0production.l2prod.new]
$     say "copying ''src'"
$     DIRECTORY/SIZ  'src''what' 
$     copy/log 'src''what' *
$   ENDIF
$   purge *.*
$EXIT:
$   EXIT
