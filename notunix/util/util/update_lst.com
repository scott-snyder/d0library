$!========================================================================
$!
$! Name      : UPDATE_LST
$!
$! Purpose   : updates list of files 
$!
$! Arguments : P1 - names of files to include in list (may contain wildcards) 
$!             P2 - name of new list file (including extension)
$!             P3 - names of existing list files (including extensions) 
$!                      whose contents is to be excluded from new list file 
$!                      (may contain wildcards)
$!
$! Created   5-NOV-1992   Ulrich Heintz
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   set verify
$   say :== write sys$output
$   dxx :== delete/nolog/noconfirm 
$   proc_name=f$process()           ! get process ID for unique name
$   context=""
$   temp = f$context ("process", context, "prcnam", proc_name, "EQL")
$   pid = f$pid(context)
$   
$   if p1.eqs."".or.p2.eqs."" 
$   then
$       say "specify which files to list (P1) and name of list file (P2)"
$       goto exit
$   endif
$   say " "
$   say "making list of files: ''p1'..."
$!==>directory<===========================================================
$   dir/columns=1/out=dir_'pid'.dat  'p1'
$!==>sort<================================================================
$   if f$locate("D0$DATA",p1).eq.0 
$   then
$     directory = f$extract(0,f$locate(":",p1)+1,p1)
$   else
$     disk = f$parse("''p1'",,,"device","syntax_only")
$     directory = f$parse("''p1'",,,"directory","syntax_only")
$     directory = disk + directory
$   endif
$   def inlist dir_'pid'.dat
$   def outlist new_'pid'.lst
$   open/write com sort_'pid'.com
$   write com "$   r usr$root0:[uli.lst]sort"
$   write com "''directory'"
$   write com " "
$   close com
$   @sort_'pid'
$!==>difference with old lists to get rid of overlaps <===================
$!   assign out_'pid'.dat sys$output
$   if p3.nes."".and.f$search("''p3'").nes."" 
$   then
$     append/new 'p3' old_'pid'.lst
$     edit new_'pid'.lst
subs/./!/whole/notype
exit
$     purge/nolog new_'pid'.lst
$     edit old_'pid'.lst
subs/./!/whole/notype
exit
$     purge/nolog old_'pid'.lst
$     difference/comment="!"/ignore=(comments,exact)/match=1/separate=master -
        /out/nonumber new_'pid'.lst old_'pid'.lst
$     sea/out='p2'  new_'pid'.dif "!"
$     edit 'p2'
subs/!/./whole/notype
exit
$     purge/nolog 'p2'
$   else
$     copy new_'pid'.lst 'p2'
$   endif
$EXIT:
$   deass sys$output
$   if f$search("dir_''pid'.dat").nes."" then dxx dir_'pid'.dat;*
$   if f$search("new_''pid'.lst").nes."" then dxx new_'pid'.lst;*
$   if f$search("old_''pid'.lst").nes."" then dxx old_'pid'.lst;*
$   if f$search("new_''pid'.dif").nes."" then dxx new_'pid'.dif;*
$   if f$search("sort_''pid'.com").nes."" then dxx sort_'pid'.com;*
$   EXIT
