$!========================================================================
$!
$! Name      : RUN_WZ
$!
$! Purpose   : select WZ daughter event sample
$!             makes list of RGE data files that have not yet been analyzed
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
$ 
$ ! notify me that the job has started
$ 
$ create mail.txt
WZ event selection job has started
$ mail/subject="WZ event selection started" mail.txt fnald0::uli
$ delete/nolog/noconfirm mail.txt;
$ 
$ proc_name=f$process()           ! get process ID for unique name
$ context=""
$ temp = f$context ("process", context, "prcnam", proc_name, "EQL")
$ pid = f$pid(context)
$ 
$ if p1.eqs."" then p1=3
$ 
$ ACCESS WZ_3
$ ACCESS WZ_1
$ SET DEF WZ_3$HROOT:[EVENT_SAMPLE.EXE]
$
$! update list file
$
$ date=f$extract(0,11,f$time())
$ if f$locate(" ",date).eq.0 then date="0"+f$extract(1,10,date)
$ list="WZ_3$HROOT:[EVENT_SAMPLE.LST]RGE_''date'.LST"
$ @wz_3$hroot:[event_sample.util]update_lst  -
      d0$data$dst:*.*RGE* 'list' WZ_3$HROOT:[EVENT_SAMPLE.LST]RGE_*.LST
$ if f$search("''list'").eqs."" 
$ then
$     write sys$output "no list file created"
$     goto nolist
$ endif
$     
$! create WZ.INP
$ 
$ create wz_'pid'.inp
Output Data Files
WEV         !-< Output stream, 3 CHARACTERS,<cr> exit >
WEV_FILE    !-< Name for output file >                 
N           !-< File mode X,G or N ? [N]:>
N           !-< Write only event records(no b-o-r or e-o-r)? [N]>
Y           !-< Want to add or remove from list? [N]>
CACH
CAD1
CAD2
CDD1
CDD2
CDD3
CDD4
GEAN
HITS
JPTS
MUD1
TRGR


Output Data Files
ZEE         !-< Output stream, 3 CHARACTERS,<cr> exit >
ZEE_FILE    !-< Name for output file >                 
N           !-< File mode X,G or N ? [N]:>
N           !-< Write only event records(no b-o-r or e-o-r)? [N]>
Y           !-< Want to add or remove from list? [N]>
CACH
CAD1
CAD2
CDD1
CDD2
CDD3
CDD4
GEAN
HITS
JPTS
MUD1
TRGR


Output Data Files
WMU         !-< Output stream, 3 CHARACTERS,<cr> exit >
WMU_FILE    !-< Name for output file >                 
N           !-< File mode X,G or N ? [N]:>
N           !-< Write only event records(no b-o-r or e-o-r)? [N]>
Y           !-< Want to add or remove from list? [N]>
CACH
CAD1
CAD2
CDD1
CDD2
CDD3
CDD4
GEAN
HITS
JPTS
TRGR


Output Data Files
ZMU         !-< Output stream, 3 CHARACTERS,<cr> exit >
ZMU_FILE    !-< Name for output file >                 
N           !-< File mode X,G or N ? [N]:>
N           !-< Write only event records(no b-o-r or e-o-r)? [N]>
Y           !-< Want to add or remove from list? [N]>
CACH
CAD1
CAD2
CDD1
CDD2
CDD3
CDD4
GEAN
HITS
JPTS
TRGR


Auto Process
N           !-< Will you be reading files from tape? [N]>
FILE_NAMES  !-<Input File Name>
N
2
$ 
$! run selection job
$ 
$ @WZ_D0USER
$
$ DEFINE FILE_NAMES 'list'
$ DEFINE WEV_FILE WZ_3$HROOT:[EVENT_SAMPLE.WEV]WEV_RGE_'date'.DST
$ DEFINE ZEE_FILE WZ_3$HROOT:[EVENT_SAMPLE.ZEE]ZEE_RGE_'date'.DST
$ DEFINE WMU_FILE WZ_1$HROOT:[EVENT_SAMPLE.WMU]WMU_RGE_'date'.DST
$ DEFINE ZMU_FILE WZ_1$HROOT:[EVENT_SAMPLE.ZMU]ZMU_RGE_'date'.DST
$ 
$ D0USER/NOSMG/COMMAND=WZ_'pid'.INP
$ 
$ nolist:
$ 
$ ! resubmit job
$ 
$ submit/char=(wz_prj)/log=WZ_3$HROOT:[EVENT_SAMPLE.log] -
       wz_3$hroot:[event_sample.util]update_wz/after="+'P1'-00"
$ 
$EXIT:
$ create  mail.txt
WZ event selection job has ended
$ mail/subject="WZ event selection ended" mail.txt fnald0::uli
$ delete/nolog/noconfirm mail.txt;,wz_'pid'.inp;
$ EXIT
