$!========================================================================
$!
$! Name      : PULL_D0LIBRARY
$!
$! Purpose   : Pull D0Library from Master node based on dates of FLAG files and
$!              architecture.
$!
$! Arguments : P1 = File containing list of libraries to check.
$!             P2 = Sub_master from which to pull files.
$!             P3 = "mail_addr" for mail notification
$!                  or
$!                  "D0NEWS/mail_addr" for D0News + Local mail notice
$!                  or
$!                  D0NEWS for d0news only.
$!                   NOTE: mail_addr can be a "," dilimited list or even
$!                         a distribution list "@dist.dis"
$!
$! Output    : Symbol PULL$STATUS =  0  OK
$!                                  -1  Error Exit
$!                                   1  Control-Y Exit
$!
$! Created   4-AUG-1993   Alan M. Jonckheere
$! Modified 26-JAN-1994   Alan M. Jonckheere
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO ERR_EXIT
$   ON CONTROL_Y THEN $ GOTO crtl_EXIT
$   pull$status == 0
$   nlib == 0
$
$   mail_test := FNAL
$   decnet_mail_addr = "FNAL::JONCKHEERE"
$   internet_mail_addr = "jonckheere@fnal.gov"
$
$   pull_list = p1
$   IF pull_list .eqs. "" THEN -
        pull_list := d0$disk:[d0library]pull_d0library.list
$   IF f$search("''pull_list'") .eqs. "" THEN -
        pull_list := d0$util:pull_d0library.list_template
$   open/read/error=open_err p_list 'pull_list'
$
$   user = f$user()
$   node = f$getsyi("NODENAME")
$   open/write pulled d0$disk:[d0library]000_pulled.mail
$   write pulled "*** D0Library Pull update by User ''user' to node: ''node'"
$
$   IF p2 .eqs. ""
$   THEN
$       IF f$search("sys$system:alphavmssys.par") .nes. ""
$       THEN
$           axp := true
$           sub_master := D0TNG
$           sub_type := AXP
$       ELSE
$           axp := false
$           sub_master := D0GS06
$           sub_type := VAX
$       ENDIF
$       WRITE SYS$OUTPUT "*** ''sub_type' system, submaster is ''sub_master' ***"
$   ELSE
$       sub_type := VAX
$       IF f$search("sys$system:alphavmssys.par") .nes. "" then sub_type := AXP
$       sub_master := 'p2'
$       WRITE SYS$OUTPUT "*** User chosen ''sub_type' submaster is ''sub_master' ***"
$   ENDIF
$
$   src_disk = ""
$   if (sub_master .eqs. "DABR01") then src_disk = "LBLIB"
$
$! check if we have DECNet connectivity, either normal or over IP
$   open/err=no_decnet tst_mail 'mail_test'::"mail="
$   close/err=no_decnet tst_mail
$   goto norm_decnet
$no_decnet:         ! no normal DECNet, try over IP
$   if "''p2'" .eqs. "" then sub_master = "''sub_master'.fnal.gov"       ! assume at FNAL if not given
$
$norm_decnet:
$   write pulled "*** ''sub_type' Sub_Master is ''sub_master'"
$   write pulled ""
$
$!================================================
$!   Setup mail notification addresses
$!================================================
$   p3 = f$edit(p3,"TRIM,COMPRESS,UPCASE")
$   p3_len = f$length(p3)
$   if f$extract(0,6,p3) .eqs. "D0NEWS"
$   then
$       do_news := true
$       if p3_len .gt. 6 then p3 = f$extract(7,p3_len-7,p3)
$   else
$       do_news := false
$   endif
$   added_mail_addr = ""
$   if p3 .nes. "" .and. p3 .nes. "D0NEWS" then added_mail_addr = ",''p3'"
$
$   mail_addr = "''decnet_mail_addr'''added_mail_addr'"
$   IF f$trnlnm( "SMTP_MAILSHR" ) .nes. "" THEN  -
        mail_addr = "SMTP%""""""''internet_mail_addr'""""""''added_mail_addr'"
$   IF f$search( "SYS$SHARE:UCX$SMTP_MAILSHR.EXE" ) .nes. "" THEN  -
        mail_addr = "SMTP%""""""''internet_mail_addr'""""""''added_mail_addr'"
$   IF f$trnlnm( "MX_MAILSHR" ) .nes. "" THEN  -
        mail_addr = "MX%""""""''internet_mail_addr'""""""''added_mail_addr'"
$!amj $   open/err=send_the_mail tst_mail fnal::"mail="
$!amj $   close/err=send_the_mail tst_mail
$!amj $   mail_addr = mail_addr = "''decnet_mail_addr'''added_mail_addr'"
$!amj $send_the_mail:
$
$!================================================
$!   Do special files 1st
$   call special_file [d0library]d0prolog.com   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$   call special_file [d0library]d0prosymcom.com   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$   call special_file [d0library]define_czars.com   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$   call special_file [d0library.docs]user_list.txt   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$   call special_file [d0library.docs]entry_point.lis   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$   call special_file [d0library]pull_cleanup.com   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$   call special_file [d0library.util]pull_d0library.com   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$   call special_file [d0library.test.util]pull_d0library.com   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$   call special_file [d0library.util]pull_d0library.list_template        'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$   call special_file [d0library.test.util]pull_d0library.list_template   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$
$!================================================
$
$loop:
$   read/end=finish p_list inline
$   inline = f$edit(inline,"COMPRESS,TRIM,UNCOMMENT,UPCASE")
$   if f$length(inline) .eq. 0 then goto loop
$   call check_it 'inline' ""   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$   call check_it 'inline' TEST   'src_disk'
$   if pull$status .lt. 0 then goto err_exit
$   if pull$status .gt. 0 then goto crtl_exit
$   goto loop
$
$finish:
$   set noon        ! No errors on notification
$   if f$search("d0$disk:[d0library]pull_cleanup.com") .nes. "" then -
        @d0$disk:[d0library]pull_cleanup.com 'sub_master'
$   if f$trnlnm("p_list") .nes. "" then close p_list
$   WRITE pulled "*** -I- Finished with update ****"
$   if f$trnlnm("pulled") .nes. "" then close pulled
$   mail d0$disk:[d0library]000_pulled.mail/noself/subject="SUCCESSFUL update of ''node' ''sub_type' D0Library" 'mail_addr'
$   if nlib .gt. 0 .and. do_news then call d0news_notice "SUCCESSFUL update of ''node' ''sub_type' D0Library"
$   delete/noconf/nolog d0$disk:[d0library]000_pulled.mail;*
$   WRITE SYS$OUTPUT "**** -I- Finished with update ****"
$   delete/symbol/global nlib
$   set on
$   EXIT
$
$open_err:
$   WRITE SYS$OUTPUT "**** -F- Can't open ''pull_list' as pull list file ****"
$   write pulled "*** -F- Can't open ''pull_list' as pull list file ***"
$ERR_EXIT:
$   set noon        ! No errors on notification
$   if f$trnlnm("p_list") .nes. "" then close p_list
$   write pulled "*** -F- Exit with error"
$   if f$trnlnm("pulled") .nes. "" then close pulled
$   mail d0$disk:[d0library]000_pulled.mail/noself/subject="ERROR during update of ''node' ''sub_type' D0Library" 'mail_addr'
$   if nlib .gt. 0 .and. do_news then call d0news_notice "ERROR during update of ''node' ''sub_type' D0Library"
$   delete/noconf/nolog d0$disk:[d0library]000_pulled.mail;*
$   WRITE SYS$OUTPUT "**** -F- Exit with error ****"
$   delete/symbol/global nlib
$   set on
$   EXIT
$crtl_EXIT:
$   set noon        ! No errors on notification
$   if f$trnlnm("p_list") .nes. "" then close p_list
$   write pulled "*** -F- Exit with Crtl Y"
$   if f$trnlnm("pulled") .nes. "" then close pulled
$   mail d0$disk:[d0library]000_pulled.mail/noself/subject="CRTL-Y during update of ''node' ''sub_type' D0Library" 'mail_addr'
$   if nlib .gt. 0 .and. do_news then call d0news_notice "CRTL-Y during update of ''node' ''sub_type' D0Library"
$   delete/noconf/nolog d0$disk:[d0library]000_pulled.mail;*
$   WRITE SYS$OUTPUT "**** -F- Exit with Crtl Y ****"
$   delete/symbol/global nlib
$   set on
$   EXIT
$
$!**********************************************************************
$special_file: subroutine
$!================================================
$!   SPECIAL_FILE, pull special files. These are single files,
$!      mostly from the TOP level of D0library
$!      P1 = File name. If it does *not* contain "[" then file is in and goes
$!              into the top level of D0Library. Otherwise, the entire directory
$!              tree must be specified.
$!      P2 = Source disk (normally D0$DISK)
$!================================================
$   ON ERROR     THEN $ GOTO ERR_EXIT
$   ON CONTROL_Y THEN $ GOTO crtl_EXIT
$
$   d0_disk := 'p2'
$   if p2 .eqs. "" then d0_disk := D0$DISK
$   file = "[D0Library]''p1'"
$   if f$locate("[",p1) .ne. f$length(p1) then file = "''p1'"
$
$   n = 0
$tag1:
$   tag_rem = f$search("''sub_master'::''d0_disk':''file'")
$   IF tag_rem .eqs. ""
$   THEN
$       IF n .lt. 3             ! try 4 times
$       THEN
$           n = n + 1
$! make quiet $           WRITE SYS$OUTPUT "---- -I- Wait 1 sec then try again to find directory on ''sub_master' ----"
$           WAIT 0:00:01.0
$           goto tag1
$       ELSE
$           if f$search("''sub_master'::''d0_disk':[d0library]d0local.com") .eqs. ""
$           then
$               WRITE SYS$OUTPUT "**** -F- ''sub_master' not reachable ****"
$               write pulled "*** -F- ''sub_master' not reachable ***"
$               goto err_exit
$           else
$               WRITE SYS$OUTPUT "**** -I- ''p1' does not exist at ''sub_master' ****"
$! **** $! don't delete, leave for pull_cleanup.com 
$! **** $               if f$search("d0$disk:''file'") then delete/log/noconf d0$disk:'file';*
$               goto norm_EXIT
$           endif
$       ENDIF
$   ENDIF
$
$   cdt_rem = f$file_attributes(tag_rem,"CDT")
$   cdt_rem = f$extract(0,f$length(cdt_rem)-3,cdt_rem)
$
$   tag_loc = f$search("d0$disk:''file'")
$   IF tag_loc .eqs. ""
$   THEN
$       WRITE SYS$OUTPUT "**** -I- ''p1' doesn't exist, get it ****"
$       copy/log 'sub_master'::'d0_disk':'file' [*]
$       nlib == nlib + 1
$       write pulled " Update D0$DISK:''file'"
$       purge/log d0$disk:'file'
$       goto norm_exit
$   ENDIF
$
$   cdt_loc = f$file_attributes(tag_loc,"CDT")
$   cdt_loc = f$extract(0,f$length(cdt_loc)-3,cdt_loc)
$   if cdt_loc .nes. cdt_rem
$   then
$       WRITE SYS$OUTPUT "**** -I- ''p1' is old, get new one ****"
$       delete/log d0$disk:'file';*
$       copy/log 'sub_master'::'d0_disk':'file' [*]
$       nlib == nlib + 1
$       write pulled " Update D0$DISK:''file'"
$       purge/log d0$disk:'file'
$   else
$       WRITE SYS$OUTPUT "**** -I- ''p1' is up-to-date ****"
$   endif
$
$norm_exit:
$   exit
$
$ERR_EXIT:
$   pull$status == -1
$   exit
$crtl_exit:
$   pull$status == 1
$   exit
$   endsubroutine
$!**********************************************************************
$check_it:  subroutine
$!================================================
$!   CHECK_IT, check validity of local version
$!      P1 = Library (Product) name
$!      P2 = TEST or ""
$!      P3 = disk name at remote site
$!================================================
$   ON ERROR     THEN $ GOTO ERR_EXIT
$   ON CONTROL_Y THEN $ GOTO crtl_EXIT
$
$   if p2 .nes. "" then p2 = "." + "''p2'"
$
$   d0_disk := 'p3'
$   if p3 .eqs. "" then d0_disk := d0$disk
$
$   n = 0
$tag2:
$   IF f$search("''sub_master'::''d0_disk':[d0library''p2']''p1'.dir") .eqs. ""
$   THEN
$       IF n .lt. 3             ! try 4 times
$       THEN
$           n = n + 1
$! make quiet $           WRITE SYS$OUTPUT "---- -I- Wait 1 sec then try again to find directory on ''sub_master' ----"
$           WAIT 0:00:01.0
$           goto tag2
$       ELSE
$           if f$search("''sub_master'::''d0_disk':[d0library]d0local.com") .eqs. ""
$           then
$               WRITE SYS$OUTPUT "**** -F- ''sub_master' not reachable ****"
$               write pulled "*** -F- ''sub_master' not reachable ***"
$               goto err_exit
$           else
$               WRITE SYS$OUTPUT "**** -I- ''p2' ''p1' does not exist at ''sub_master' ****"
$               if p2 .nes. "" then call clean_it 'p1' 'p2'     ! *Don't* delete OFFICIAL, leave for PULL_CLEANUP.COM
$               goto norm_EXIT
$           endif
$       ENDIF
$   ENDIF
$
$   tag_rem = f$search("''sub_master'::''d0_disk':[d0library''p2'.''p1']000_''p1'_v*.*")
$   IF tag_rem .eqs. ""
$   THEN
$       WRITE SYS$OUTPUT -
            "**** -W- ''p2' d0$disk:''p1' does not have valid TAG File at ''sub_master' ****"
$       goto norm_exit
$   ENDIF
$   cdt_rem = f$file_attributes(tag_rem,"CDT")
$
$   IF f$search("d0$disk:[d0library''p2']''p1'.dir") .eqs. ""
$   THEN
$       WRITE SYS$OUTPUT "**** -I- ''p2'.''p1' doesn't exist, get it ****"
$       call getit 'p1' 'p2'
$       goto norm_exit
$   ENDIF
$   tag_loc = f$search("d0$disk:[d0library''p2'.''p1']000_''p1'_v*.*")
$   IF tag_loc .eqs. ""
$   then
$       WRITE SYS$OUTPUT "**** -I- ''p2'.''p1' tag not there, get new one ****"
$       call clean_it 'p1' 'p2'
$       call getit 'p1' 'p2'
$       goto norm_exit
$   endif
$
$   cdt_loc = f$file_attributes(tag_loc,"CDT")
$   if cdt_loc .nes. cdt_rem
$   then
$       WRITE SYS$OUTPUT "**** -I- ''p2'.''p1' is old, get new one ****"
$       call clean_it 'p1' 'p2'
$       call getit 'p1' 'p2'
$   else
$       WRITE SYS$OUTPUT "**** -I- ''p2'.''p1' is up-to-date ****"
$   endif
$
$norm_exit:
$   exit
$
$ERR_EXIT:
$   pull$status == -1
$   exit
$crtl_exit:
$   pull$status == 1
$   exit
$   endsubroutine
$!**********************************************************************
$
$clean_it: SUBROUTINE
$!================================================
$!   CLEAN_IT, delete the old directory
$!      P1 = Library (Product) name
$!      P2 = TEST or ""
$!================================================
$   ON ERROR     THEN $ GOTO ERR_EXIT
$   ON CONTROL_Y THEN $ GOTO crtl_EXIT
$
$! Delete old directory before copy
$   set noon
$   IF f$search("d0$disk:[d0library''p2']''p1'.dir") .nes. ""
$   THEN
$       WRITE SYS$OUTPUT "**** -I- Deleting ''p2'.''p1' ****"
$       IF f$search("d0$disk:[d0library''p2'.''p1'...]*.*;*") .nes. ""
$       THEN
$           delete/nolog/noconf d0$disk:[d0library'p2'.'p1'...]*.*;*/exc=*.dir
$           IF f$search("d0$disk:[d0library''p2'.''p1']*.dir") .nes. ""
$           THEN
$               set file/nolog/prot=o:rwed d0$disk:[d0library'p2'.'p1']*.dir
$               set acl/nolog/def d0$disk:[d0library'p2'.'p1']*.dir
$               delete/nolog/noconf d0$disk:[d0library'p2'.'p1']*.dir*;*
$           ENDIF
$       ENDIF
$       IF p2 .nes. ""
$       THEN
$           set file/nolog/prot=o:rwed d0$disk:[d0library.test]'p1'.dir
$           set acl/nolog/def d0$disk:[d0library.test]'p1'.dir
$           delete/nolog/noconf d0$disk:[d0library.test]'p1'.dir;*
$       ENDIF
$   ENDIF
$
$   set on
$   exit
$
$ERR_EXIT:
$   pull$status == -1
$   exit
$crtl_exit:
$   pull$status == 1
$   exit
$   endsubroutine
$!**********************************************************************
$
$getit:     SUBROUTINE
$!================================================
$!   GET_IT, actually do the copy
$!      P1 = Library (Product) name
$!      P2 = TEST or ""
$!================================================
$   ON ERROR     THEN $ GOTO ERR_EXIT
$   ON CONTROL_Y THEN $ GOTO crtl_EXIT
$
$! COPY new files from source Sub-master
$   WRITE SYS$OUTPUT "**** -I- Fetching ''p2'.''p1' from ''sub_master':: ****"
$   if f$search("d0$disk:[d0library''p2']'p1'.dir") .eqs. "" then -
        create/dir d0$disk:[d0library'p2'.'p1']
$   IF f$search("''sub_master'::''d0_disk':[d0library''p2']''p1'.dir") .nes. ""
$   THEN
$       copy/log -
            'sub_master'::'d0_disk':[d0library'p2'.'p1'...]*.*;* -
                /exc=(000_'p1'_v*.*;*) [*]
$       copy/log -
            'sub_master'::'d0_disk':[d0library'p2'.'p1']000_'p1'_v*.*;* [*]
$       nlib == nlib + 1
$       write pulled " Update ''p2'.''p1'"
$   ENDIF
$   EXIT
$
$ERR_EXIT:
$   pull$status == -1
$   exit
$crtl_exit:
$   pull$status == 1
$   exit
$   endsubroutine
$!**********************************************************************
$
$d0news_notice:     SUBROUTINE
$!================================================
$!   Send the D0News Notice
$!      P1 = Subject string for the D0News Notice
$!================================================
$   ON ERROR     THEN $ GOTO ERR_EXIT
$   ON CONTROL_Y THEN $ GOTO crtl_EXIT
$
$   Pid = F$PID(Context)
$   copy nl: d0$disk:[d0library]'pid'_d0news.com
$   open/append news_file d0$disk:[d0library]'pid'_d0news.com
$   write news_file "$ d0news"
$   write news_file -
        "add/folder=""d0.releases""/expir=7/local/descr=""''p1'"" d0$disk:[d0library]000_pulled.mail"
$   write news_file "YES"
$   write news_file "exit"
$   close news_file
$   spawn/wait/input=nl:/output=d0$disk:[d0library]'pid'_d0news.out @d0$disk:[d0library]'pid'_d0news.com
$   del/log d0$disk:[d0library]'pid'_d0news.com;*,d0$disk:[d0library]'pid'_d0news.out;*
$
$ERR_EXIT:
$   pull$status == -1
$   exit
$crtl_exit:
$   pull$status == 1
$   exit
$   endsubroutine
$!**********************************************************************
