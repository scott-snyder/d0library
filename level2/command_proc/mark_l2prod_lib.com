$!------------------------------------------------
$!
$! Name      : mark_l2prod_lib  version 1.0
$!          (based on fake_gamma)
$! Purpose   : make tag file and update version function for library
$!      It should be run in an area [XXXX.L2PROD.libname]
$!
$!
$! Created  20-Aug-1992   James T. Linnemann
$!------------------------------------------------
$ ON CONTROL_Y THEN $ GOTO EXIT
$ ON CONTROL_C THEN $ GOTO EXIT
$!
$! set logicals to point here; handle possible rooted directory
$ heredir = F$ENVIRONMENT("DEFAULT") - "000000."
$ SET DEFAULT [-]
$ gammadir = F$ENVIRONMENT("DEFAULT")
$ SET DEFAULT 'heredir'
$ gname = gammadir - "000000" - "]"
$ libname = heredir - gname
$ libname = libname  - "." - "]"
$ DEFINE HERE 'heredir'
$ IF F$SEARCH("LOGICALS.COM") .NES. "" THEN @LOGICALS.COM
$ DEFINE D0$'libname' HERE, D0$'libname'$ROOT:[000000]
$!
$!
$!create tag file which formally declares this a library
$ @L2$PROD:define_l2_pass
$ prodtag = "_''l2_version_num'.''l2_pass_num'"
$  COPY NL: 000_'libname'_l2prod'prodtag'
$!
$!see whether this is simple case of a library without fortran files
$!
$!Update version function
$ IF F$SEARCH("D0$''libname':V''libname'.FOR") .EQS. "" THEN GOTO EXIT
$! update the version function
$   time = F$TIME()
$   time = time - f$extract(f$locate(".",time),3,time)
$   String = "     X  " + "'" + " L2PROD''prodtag' " + ''time' + "'//"
$   true = -1
$   false = 0
$   OPEN/WRITE new_version GAMMA.DATE
$   COPY D0$'libname':V'libname'.FOR *
$   OPEN/READ/END=CLOS old_version V'libname'.FOR
$READ_LOOP:
$   found = false
$   READ/END=CLOS/ERROR=CLOS old_version line
$   IF f$locate("V''libname' =",line) .NE. f$length(line)
$     THEN
$       WRITE new_version line
$       WRITE new_version "''string'"
$       found = true
$   ENDIF
$   IF f$locate("FAKE",line) .NE. f$length(line)
$     THEN
$       found = true
$   ENDIF
$   IF f$locate("L2PROD_",line) .NE. f$length(line)
$     THEN
$       found = true
$   ENDIF
$   IF found.EQ.false THEN WRITE new_version line
$   GOTO READ_LOOP
$CLOS:
$   CLOSE new_version
$   CLOSE old_version
$   RENAME GAMMA.DATE V'libname'.FOR
$   IF f$search("source.dir") .NES. "" THEN COPY V'libname'.FOR [.SOURCE]
$     FOR v'libname'.FOR
$     PIPE LIBRARY/REPLACE *.OLB/EXCLUDE=(DEB_*.OLB) v'libname'.OBJ
$     DELETE v'libname'.OBJ;*
$     FOR/DEBUG=all/NOOPT v'libname'.FOR
$     PIPE LIBRARY/REPLACE DEB*.OLB v'libname'.OBJ
$     DELETE v'libname'.OBJ;*
$!
$EXIT:
$   SET NOON
$   if f$trnlnm("new_version") .nes. "" then CLOSE new_version
$   if f$trnlnm("old_version") .nes. "" then CLOSE old_version
$   if f$trnlnm("logfile") .nes. "" then CLOSE logfile
$   DEASSIGN D0$'libname'    !set back to previous value
$   DEASSIGN HERE
$   IF F$SEARCH("GAMMA.DATE").NES."" THEN DELETE GAMMA.DATE;*
$   SET ON
$   EXIT
