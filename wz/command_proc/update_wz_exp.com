$!========================================================================
$!
$! Name      : UPDATE_WZ_EXP
$!
$! Purpose   : select WZ daughter event sample
$!             makes list of EXP data files that have not yet been analyzed
$!             runs WZ selection program and resubmits itself for next update
$!             after selected time
$!
$! Arguments : P1 - number of days between jobs (default=3)
$!
$! Created   6-FEB-1993   Ulrich Heintz
$!
$!========================================================================
$ ON ERROR     THEN $ GOTO EXIT
$ ON CONTROL_Y THEN $ GOTO EXIT
$ ACCESS WZ
$ ACCESS WZ_3
$ SET DEF WZ$HROOT:[EVENT_SAMPLE.JUNK]
$ DEF WZ$LST WZ$HROOT:[EVENT_SAMPLE.LST]
$ DEF WZ$LOG WZ$HROOT:[EVENT_SAMPLE.LOG]
$ DEF WZ$EXE wz$hroot:[event_sample.EXE]
$ DEF WZ$UTIL wz$hroot:[event_sample.UTIL]
$ !----------------------------------------------------------------------
$ ! resubmit job
$   if p1.eqs."" then p1=3
$   submit/noprinter/notify/log=WZ$LOG/char=wz_prj  -
       wz$EXE:update_wz_exp/after=+'P1'-00
$ !----------------------------------------------------------------------
$   proc_name=f$process()           ! get process ID for unique name
$   context=""
$   temp = f$context ("process", context, "prcnam", proc_name, "EQL")
$   pid = f$pid(context)
$ !----------------------------------------------------------------------
$   nlist=0
$   dir/column=1/out='pid'.lis WZ$lst:exp_1012_%%%%.lst;
$   open dir 'pid'.lis
$ read_next_line:
$   read/end=no_more_lines dir line
$   indx=f$locate(".LST",line)
$   if indx.ge.f$length(line) then goto read_next_line
$   ilist=f$extract(indx-4,4,line)
$   if ilist.gt.nlist then nlist=ilist
$   goto read_next_line
$ no_more_lines:
$   close dir
$   nlist=nlist+1
$   ty 'pid'.lis
$   delete 'pid'.lis;
$   write sys$output " "
$   write sys$output "new files will start at index ''nlist'"
$ !----------------------------------------------------------------------
$ ! time stamp
$   TIME_STAMP = F$TIME()
$   l=f$length(TIME_STAMP)
$   TIME_STAMP=f$extract(0,11,TIME_STAMP)+":"+f$extract(12,l-12,TIME_STAMP)
$   write sys$output "time stamp: "+TIME_STAMP
$ !----------------------------------------------------------------------
$   access wz
$   @wz$util:update_lst d0$data$dst:exp*.*dst*101*  -
        wz$lst:exp_1012_update.lst wz$lst:exp_1012_0*.lst
$   def inlist WZ$lst:exp_1012_update.lst
$   if nlist.lt.10 
$   then
$     DEF OUTLIST WZ$lst:exp_1012_000'NLIST'.LST
$   else
$     if nlist.lt.100
$       DEF OUTLIST WZ$lst:exp_1012_00'NLIST'.LST
$     else
$       if nlist.lt.1000
$         DEF OUTLIST WZ$lst:exp_1012_0'NLIST'.LST
$       else
$         DEF OUTLIST WZ$lst:exp_1012_'NLIST'.LST
$       endif
$     endif
$   endif
$   open/write com x.com
$   r wz$UTIL:sort


$ !----------------------------------------------------------------------
$ ! run selection job
$   submit/noprinter/notify/log=WZ$LOG/char=wz_prj   -
        d0$wz:run_wz_exp/pa=(exp_1012_'NLIST',NOMUON)
$ !----------------------------------------------------------------------
$ exit:
$   ! notify me that the job has ended
$   open/write mail mail.txt
$   write mail  -
        "UPDATE_WZ_EXP job has ended and created the following list files:"
$   write mail " "
$   close mail
$   dir/column=1/sin='time_stamp'/out='pid'.lis WZ$lst:exp_1012_%%%%.lst;
$   append 'pid'.lis mail.txt
$   delete 'pid'.lis;
$   mail/subject="UPDATE_WZ_EXP" mail.txt fnald0::uli
$   delete/nolog/noconfirm mail.txt;
$ !----------------------------------------------------------------------
$   exit
