$!========================================================================
$!
$! Name      : RUN
$!
$! Arguments : P1 - name of input file list
$!             
$! Created   5-OCT-1992   Ulrich Heintz
$!
$!========================================================================
$ ON ERROR     THEN $ GOTO EXIT
$ ON CONTROL_Y THEN $ GOTO EXIT
$ 
$ ACCESS WZ
$ ACCESS WZ_3
$ SET DEF WZ$HROOT:[EVENT_SAMPLE.JUNK]
$ 
$ proc_name=f$process()           ! get process ID for unique name
$ context = " "
$ temp = f$context ("process", context, "prcnam", proc_name, "EQL")
$ pid = f$pid(context)
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
$ @WZ$HROOT:[EVENT_SAMPLE.EXE]WZ_EXP_D0USER
$
$ DEFINE FILE_NAMES WZ$HROOT:[EVENT_SAMPLE.LST]'P1'.LST
$ IF P2.EQS."NOELECTRON" 
$ THEN
$ DEFINE WEV_FILE NL:
$ DEFINE ZEE_FILE NL:
$ ELSE
$ DEFINE WEV_FILE WZ_3$HROOT:[EVENT_SAMPLE.WEV]WEV_'P1'.DST
$ DEFINE ZEE_FILE WZ_3$HROOT:[EVENT_SAMPLE.ZEE]ZEE_'P1'.DST
$ ENDIF
$ IF P2.EQS."NOMUON" 
$ THEN
$ DEFINE WMU_FILE NL:
$ DEFINE ZMU_FILE NL:
$ ELSE
$ DEFINE WMU_FILE mucalib_1$HROOT:[dst.wmunu]WMU_'P1'.DST
$ DEFINE ZMU_FILE mucalib_1$HROOT:[dst.zmumu]ZMU_'P1'.DST
$ ENDIF
$ 
$ D0USER/NOSMG/COMMAND=WZ_'pid'.INP
$
$EXIT:
$ DELETE/LOG/NOCONFIRM WZ_'PID'.INP;
$ EXIT
