$!========================================================================
$!
$! Name      : FILE_TYPE
$!
$! Purpose   : Determine file types with a view to being transported by 
$!             ftp or zftp to UNIX systems.
$!
$! Arguments : P1 - Wildcard file specification.
$!             P2 - Options.  Any combination of single letters specifying
$!                  valid file types.  Only files with the specified types.
$!                  are printed out.  If the first character is "-", then
$!                  print all but the specified types.  Default is all types.
$!             P3 - Output file (default sys$output).
$!
$! Recognized file types:
$!
$! T - Text.
$! V - VMS specific (non-transportable).
$! M - Compack menu file (Fixed length 2444 byte records).
$! X - Data exchange mode FZ (32,760 byte records).
$! G - Geant exchange mode FZ (3600 byte records).
$! N - VAX native FZ.
$! E - Exchange mode RZ files -- default record length (1024 byte records).
$! Z - Exchange mode RZ files -- long record length (8191 byte records).
$! R - VAX native RZ files -- default record length (1024 byte records).
$! L - VAX native RZ files -- long record length (8191 byte records).
$! U - Unknown/read-protected
$!
$! Transportability:
$!
$! Type      ftp        zftp
$! ----      ---        ----
$!
$!  T        ascii      geta
$!  V        no         no
$!  M        binary     getx (lrecl=2444)
$!  X        binary     getx (lrecl=32760)
$!  G        binary     getx (lrecl=3600)
$!  N        no         getfz
$!  E        binary     getx (lrecl=1024)
$!  Z        binary     getx (lrecl=8191)
$!  R        no         getrz
$!  L        no         getrz
$!  U        no         no
$!
$! Output    : The output consists of a list of files and file types 
$!             corresponding to the options.
$!
$! Created  30-NOV-1993   Herbert Greenlee
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ wildcard = p1
$ options = p2
$ output = p3
$ fzmode = f$search("d0$disk:[d0library.test.util]fzmode.exe")
$ if fzmode.eqs."" then -
  fzmode = f$search("d0$disk:[d0library.util]fzmode.exe")
$ if fzmode.eqs.""
$ then
$       write sys$output "Fzmode executable not found"
$       exit
$ endif
$ fzmode = "$" + fzmode
$ if f$trnlnm("output").nes."" then close output
$ if f$trnlnm("output").nes."" then deas output
$ if output.eqs."" 
$ then 
$       def output sys$output
$ else
$       open/write/share output 'output'
$ endif
$!
$! Scan options and set defaults.
$!
$ if options.eqs."" then options = "TVMXGNEZRLU"
$ disp_t = 0
$ disp_v = 0
$ disp_m = 0
$ disp_x = 0
$ disp_g = 0
$ disp_n = 0
$ disp_e = 0
$ disp_z = 0
$ disp_r = 0
$ disp_l = 0
$ disp_u = 0
$ if f$locate("T",options).lt.f$length(options) then disp_t = 1
$ if f$locate("V",options).lt.f$length(options) then disp_v = 1
$ if f$locate("M",options).lt.f$length(options) then disp_m = 1
$ if f$locate("X",options).lt.f$length(options) then disp_x = 1
$ if f$locate("G",options).lt.f$length(options) then disp_g = 1
$ if f$locate("N",options).lt.f$length(options) then disp_n = 1
$ if f$locate("E",options).lt.f$length(options) then disp_e = 1
$ if f$locate("Z",options).lt.f$length(options) then disp_z = 1
$ if f$locate("R",options).lt.f$length(options) then disp_r = 1
$ if f$locate("L",options).lt.f$length(options) then disp_l = 1
$ if f$locate("U",options).lt.f$length(options) then disp_u = 1
$ if f$extract(0,1,options).eqs."-"
$ then
$       disp_t = 1-disp_t
$       disp_v = 1-disp_v
$       disp_m = 1-disp_m
$       disp_x = 1-disp_x
$       disp_g = 1-disp_g
$       disp_n = 1-disp_n
$       disp_e = 1-disp_e
$       disp_z = 1-disp_z
$       disp_r = 1-disp_r
$       disp_l = 1-disp_l
$       disp_u = 1-disp_u
$ endif
$ first_file = ""
$ file_loop:
$       file = f$search(wildcard)
$       if file.eqs."" .or. file.eqs.first_file then goto file_done
$       if first_file.eqs."" then first_file = file
$!
$! Tests based on file name.
$!
$       dir = f$parse(file,,,"directory","syntax_only")
$       type = f$parse(file,,,"type","syntax_only")
$       if f$locate(".CMS",dir).lt.f$length(dir) then goto vms
$       if type.eqs.".FOR" then goto text
$       if type.eqs.".COM" then goto text
$       if type.eqs.".RCP" then goto text
$       if type.eqs.".MEM" then goto text
$       if type.eqs.".STRUC" then goto text
$       if type.eqs.".STDIA" then goto text
$       if type.eqs.".DOC" then goto text
$       if type.eqs.".TXT" then goto text
$       if type.eqs.".DIR" then goto vms
$       if f$locate("LB",type).eq.2.and.f$length(type).eq.4 then goto vms
$       if type.eqs.".EXE" then goto vms
$       if type.eqs.".OBJ" then goto vms
$       if type.eqs.".PEN" then goto vms
$       if type.eqs.".BRN" then goto vms
$       if type.eqs.".UID" then goto vms
$!
$! Tests based on contents and RMS file attributes.
$!
$       set noon
$       rat = f$file(file,"rat")
$       if .not.$status then goto unknown
$       set on
$       if rat.eqs."CR" .or. rat.eqs."FTN" .or. rat.eqs."PRN" then goto text
$!
$! Here we have a Zebra data file candidate.  Test use FZMODE and perform 
$! additional RMS attribute tests.
$!
$       if disp_n .or. disp_x .or. disp_g .or. -
           disp_e .or. disp_z .or. disp_r .or. disp_l
$       then
$           def sys$output nl:
$           def sys$error nl:
$           def for000 nl:
$           set noon
$           fzmode 'file'
$           set on
$           deas sys$output
$           deas sys$error
$           mode = f$trnlnm("fz$mode")
$           org = f$file(file,"org")
$           rfm = f$file(file,"rfm")
$           mrs = f$file(file,"mrs")
$           if org.eqs."SEQ" .and. rfm.eqs."VAR" .and.  -
                mode.eqs."NATIVE" then  -
                goto fn
$           if org.eqs."SEQ" .and. rfm.eqs."FIX" .and.  -
                mrs.eqs.32760 .and. mode.eqs."EXCHANGE" then  -
                goto fx
$           if org.eqs."SEQ" .and. rfm.eqs."FIX" .and.  -
                mrs.eqs.3600 .and. mode.eqs."EXCHANGE" then  -
                goto fg
$           if org.eqs."SEQ" .and. rfm.eqs."FIX" .and.  -
                mrs.eqs.4096 .and. mode.eqs."RZ_EXCHANGE" then  -
                goto re
$           if org.eqs."SEQ" .and. rfm.eqs."FIX" .and.  -
                mrs.eqs.32764 .and. mode.eqs."RZ_EXCHANGE" then  -
                goto rz
$           if org.eqs."SEQ" .and. rfm.eqs."FIX" .and.  -
                mrs.eqs.4096 .and. mode.eqs."RZ_NATIVE" then  -
                goto rr
$           if org.eqs."SEQ" .and. rfm.eqs."FIX" .and.  -
                mrs.eqs.32764 .and. mode.eqs."RZ_NATIVE" then  -
                goto rl
$           if org.eqs."SEQ" .and. rfm.eqs."FIX" .and.  -
                mrs.eqs.2444 then  -
                goto menu
$       endif
$       goto unknown
$!
$! Final checkout
$!
$ unknown:
$       if disp_u then write output "U	''file'"
$       goto file_loop
$ text:
$       if disp_t then write output "T	''file'"
$       goto file_loop
$ vms:
$       if disp_v then write output "V	''file'"
$       goto file_loop
$ menu:
$       if disp_m then write output "M	''file'"
$       goto file_loop
$ fx:
$       if disp_x then write output "X	''file'"
$       goto file_loop
$ fg:
$       if disp_g then write output "G	''file'"
$       goto file_loop
$ fn:
$       if disp_n then write output "N	''file'"
$       goto file_loop
$ re:
$       if disp_e then write output "E	''file'"
$       goto file_loop
$ rz:
$       if disp_z then write output "Z	''file'"
$       goto file_loop
$ rr:
$       if disp_r then write output "R	''file'"
$       goto file_loop
$ rl:
$       if disp_l then write output "L	''file'"
$       goto file_loop
$ file_done:
$EXIT:
$ if output.nes."sys$output" then close output
$   EXIT
