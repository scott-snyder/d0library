$!========================================================================
$!
$! Name      : SUBMIT_PULL_D0LIBRARY
$!
$! Purpose   : Pull D0Library from D0TNG based on dates of FLAG files
$!
$! Arguments : P1 = File containing list of libraries to check (pass this to PULL_D0LIBRARY) 
$!
$! Created  27-OCT-1994   Alan M. Jonckheere 
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO ERR_EXIT
$   ON CONTROL_Y THEN $ GOTO ERR_EXIT
$   sver = f$verify(0)
$
$   progress_flag =  0
$   bat_que := ????????
$
$loop:
$   IF f$trnlnm("d0$disk") .eqs. ""
$   THEN
$       wait 0:05:0
$       goto loop
$   ENDIF
$   progress_flag = 1
$   set default d0$disk:[d0library]
$   define d0$util d0$disk:[d0library.test.util],d0$disk:[d0library.util]
$
$! Submit next one 1st, so we always have one in the queue
$   progress_flag = 2
$   submit/after="tomorrow+0 02:00"/NOPRINT/QUEUE='BAT_QUE' -       ! ??????? put in your favorite time ????????
          /log=sys$login:pull_d0library.log -
          /param="''p1'" -
               d0$disk:[d0library]submit_pull_d0library.com
$
$   progress_flag = 3
$! Change name to quarantee we only have one running
$   if f$mode() .nes. "INTERACTIVE" then set proc/name="PULL_D0LIBRARY"
$
$   progress_flag = 4
$   WRITE SYS$OUTPUT "**** -I- Beginning d0$util:pull_d0library.com ''p1'"
$! make copy so we can update UTIL too.
$   copy/log d0$util:pull_d0library.com []
$   progress_flag = 5
$   @pull_d0library.com "''p1'" "" D0NEWS       ! ?????????? this call may be customize ?????????
$   progress_flag = 6
$   set default d0$disk:[d0library]
$   delete/noconf/log pull_d0library.com;*
$
$finish:
$   WRITE SYS$OUTPUT "**** -I- Finished with update ****"
$   goto norm_exit
$
$ERR_EXIT:
$   write sys$output "**** -E- SUBMIT_PULL_D0LIBRARY.COM ERROR "+progress_flag
$   temp = f$verify(save_proc_verify,save_image_verify)
$NORM_EXIT:
$   sver = f$verify(sver)
$   exit
