$!========================================================================
$!
$! Name      : PULL_CLEANUP
$!
$! Purpose   : Clean up obsolete directories etc.
$!
$! Arguments : P1 = Sub_Master, source machine
$!
$! Created   8-FEB-1994   Alan M. Jonckheere
$! Modified  7-JUL-1995   D0TNG::JONCKHEERE Added sub_master and return to def
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO err_EXIT
$   ON CONTROL_Y THEN $ GOTO ctry_EXIT
$
$   sub_master = p1
$   if p1 .eqs. "" then sub_master := D0TNG
$   def_dir = f$environment("DEFAULT")
$
$   write sys$output "**** Update CERNLIB SETUP where appropriate ****"
$   call special_file [d0library.cernlib]setup_cernlib.com
$   call special_file [d0library.test.cernlib]setup_cernlib.com
$   call special_file [d0library.test.cernlib.src]update_piaf.com
$   if updated
$   then
$       @[d0library.test.cernlib.src]update_piaf.com
$   endif
$   call special_file [d0library.test.cernlib.src]update_packlib.com
$   if updated
$   then
$       @[d0library.test.cernlib.src]update_packlib.com
$   endif
$
$   WRITE SYS$OUTPUT "**** Updating VAXNEWS where appropriate ****"
$   if f$trnlnm("d0$vaxnews") .eqs. ""
$   then
$       WRITE SYS$OUTPUT "**** -W- VAXNEWS doesn't exist ****"
$       goto exit
$   endif
$
$   set def d0$vaxnews:
$   if f$search("000_VAXNEWS_V3.17") .nes. ""
$   then
$       WRITE SYS$OUTPUT "  ** -I- VAXNEWS already up to date **"
$       goto exit
$   endif
$   WRITE SYS$OUTPUT "  ** -I- Copy new files **"
$
$   copy/log 'sub_master'::d0$disk:[d0library.vaxnews]*.* -
        /exc=(*.hlb,*.exe,*.olb,*.obj,*.dir,00*.*,vaxnews_options.com) -
            d0$vaxnews:
$
$   if f$search("d0$vaxnews:src.dir") .eqs. "" then -
            create/dir d0$vaxnews$root:[src]
$   copy/log 'sub_master'::d0$disk:[d0library.vaxnews.src]*.* -
            d0$vaxnews$root:[src]
$
$   if f$search("d0$vaxnews:doc.dir") .eqs. "" then -
            create/dir d0$vaxnews$root:[doc]
$   copy/log 'sub_master'::d0$disk:[d0library.vaxnews.doc]*.* -
            d0$vaxnews$root:[doc]
$
$   if f$search("d0$vaxnews_data:timezones.list") .eqs. "" then -
        copy/log -
            'sub_master'::d0$disk:[d0library.vaxnews_data]timezones.list -
                d0$vaxnews_data:
$
$   if f$search("d0$vaxnews:*.doc") .nes. "" then -
            del/log d0$vaxnews:*.doc;*
$   if f$search("d0$vaxnews:*.hlb") .nes. "" then -
            del/log d0$vaxnews:*.hlb;*
$   if f$search("d0$vaxnews:*.*book*") .nes. "" then -
            del/log d0$vaxnews:*.*book*;*
$   if f$search("d0$vaxnews:*.ps") .nes. "" then -
            del/log d0$vaxnews:*.ps;*
$   @setup_vaxnews
$   @build_vaxnews
$
$   set noon
$   purge/log d0$vaxnews$root:[000000...]
$   rena d0$vaxnews$root:[000000...]*.*/exc=*.dir *.*
$   del d0$vaxnews:00*.*;*
$   set on
$   copy/log 'sub_master'::d0$vaxnews:00*.* d0$vaxnews:
$   if f$trnlnm("d0$vaxnews_data") .nes. ""
$   then
$       set noon
$       del/log/noconf d0$vaxnews_data:00*.*;*
$       set on
$       copy/log 'sub_master'::d0$vaxnews:00*.* -
            d0$vaxnews_data:
$   endif
$
$   WRITE SYS$OUTPUT ""
$   WRITE SYS$OUTPUT "**** Done with update ****"
$
$EXIT:
$   set default 'def_dir'
$   set on
$   EXIT
$
$ctry_exit:
$err_EXIT:
$   set default 'def_dir'
$   set on
$   EXIT
$!**********************************************************************
$special_file: subroutine
$!================================================
$!   SPECIAL_FILE, pull special files. These are single files,
$!      mostly from the TOP level of D0library
$!      P1 = File name. If it does *not* contain "[" then file is in and goes
$!              into the top level of D0Library. Otherwise, the entire directory
$!              tree must be specified.
$!================================================
$   ON ERROR     THEN $ GOTO ERR_EXIT
$   ON CONTROL_Y THEN $ GOTO ERR_EXIT
$
$   updated :== false
$   file = "[D0Library]''p1'"
$   if f$locate("[",p1) .ne. f$length(p1) then file = "''p1'"
$
$   dst_disk := d0$disk
$   if p2 .nes. "" then dst_disk := 'p2'
$
$   tag_rem = -
        f$search("''sub_master'::d0$disk:''file'")
$   IF tag_rem .eqs. ""
$   THEN
$       WRITE SYS$OUTPUT -
            "**** -I- ''p1' does not exist at ''sub_master' ****"
$       if f$search("''dst_disk':''file'") then -
            delete/log/noconf 'dst_disk':'file';*
$       goto norm_EXIT
$   ENDIF
$
$   cdt_rem = f$file_attributes(tag_rem,"CDT")
$
$   tag_loc = -
        f$search("''dst_disk':''file'")
$   IF tag_loc .eqs. ""
$   THEN
$       WRITE SYS$OUTPUT "**** -I- ''p1' doesn't exist, get it ****"
$       copy/log 'sub_master'::d0$disk:'file' [*]
$       write pulled " Update ''file'"
$       purge/log 'dst_disk':'file'
$       updated :== true
$       goto norm_exit
$   ENDIF
$
$   cdt_loc = f$file_attributes(tag_loc,"CDT")
$   if cdt_loc .nes. cdt_rem
$   then
$       WRITE SYS$OUTPUT "**** -I- ''p1' is old, get new one ****"
$       delete/log 'dst_disk':'file';*
$       copy/log 'sub_master'::d0$disk:'file' [*]
$       write pulled " Update ''file'"
$       purge/log 'dst_disk':'file'
$       updated :== true
$   else
$       WRITE SYS$OUTPUT "**** -I- ''p1' is up-to-date ****"
$   endif
$
$norm_exit:
$   exit
$
$ERR_EXIT:
$   exit 2
$   endsubroutine
