$!------------------------------------------------
$!
$! Name      : CSFMAKE
$!
$! Purpose   : Create and run CSFMAKE.EXE to create
$!             file CSF_STPFILE.DAT
$!
$! Arguments : None
$!
$! Created  15-APR-1992   Chip Stewart
$!
$!------------------------------------------------
$   sav_ver = f$verify(0)
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$   WR  := WRITE SYS$OUTPUT
$
$   group := geocal
$   IF f$trnlnm("D0$RELEASE") .nes. ""
$   THEN
$       WR "****''group'-I- Doing Release, check if we need to run ****"
$       IF f$search("d0$release:[stp]''group'.dir") .eqs. ""
$       THEN
$           WR "*** No STP group ''group' - abort"
$           goto exit
$       ENDIF
$       IF f$search("d0$release:[stp.''group']*.*") .eqs. ""
$       THEN
$           WR "*** No new data in ''group' - abort"
$           goto exit
$       ENDIF
$   ENDIF
$
$   IF p1 .eqs. "DEBUG"
$   THEN
$       swit = "/DEB"
$       deb_ = "DEB_"
$   ELSE
$       swit = ""
$       deb_ = ""
$   ENDIF
$
$   WR "  "
$   WR " Create:        ''deb_'CSFMAKE.EXE"
$   WR "  "
$
$!.....        check CSFMAKE_BETA.OPT
$
$ IF "''linkopt'" .eqs. ""
$ THEN
$     linkopt = "CSFMAKE_BETA.OPT"
$ ENDIF
$
$ optfile = f$search("''linkopt'")
$ IF optfile .eqs. ""
$ THEN
$     Write Sys$output "        No option file ''linkopt', will link without"
$     BETA_OPT = ""
$ ELSE
$     BETA_OPT = "''linkopt'/Opt,"
$ ENDIF
$
$ DEFINE_COM = f$search("DEFINE.COM")
$ IF DEFINE_COM .nes. ""
$ THEN
$  @DEFINE.COM
$  DEFINE D0$STP$GEOCAL [],D0$STP$ROOT:[GEOCAL]
$ ENDIF
$
$ tmp_ver = f$verify(1)
$   LINK/nomap'swit'/EXE='deb_'CSFMAKE -
    D0$STP:'DEB_'GEOCAL/INCLUDE=(CSFMAKE)/LIB,-
        'BETA_OPT' -
    D0$CALOR_UTIL:'DEB_'CALOR_UTIL/LIBRARY,-
    D0$GENERAL:'DEB_'GENERAL/LIBRARY,-
    'cernp'                 ! D0$CERNLIB:PACKLIB/LIBRARY,KERNLIB/LIBRARY
$ tmp_ver = f$verify(tmp_ver)
$
$   WR "  "
$   WR " Invoke:        SETUP_CSFMAKE.COM"
$   WR "  "
$   @D0$STP$GEOCAL:SETUP_CSFMAKE
$   DEFINE/NOLOG FOR003 NL:
$
$   IF p1 .eqs. "DEBUG"
$   THEN
$       WR " "
$       WR "        DEB_CSFMAKE.EXE created"
$       WR "          will not be run"
$       WR " "
$       goto exit
$   ENDIF
$
$   WR "  "
$   WR " Run:           CSFMAKE.EXE"
$   WR "  "
$
$   RUN 'deb_'CSFMAKE
$
$!------------------------------------------------
$!   Tidy up a little
$!------------------------------------------------
$!   PURGE/NOCONFIRM/NOLOG 'deb_'CSFMAKE.EXE
$!   PURGE/NOCONFIRM/NOLOG CSF_STPFILE
$
$   delete/nolog 'deb_'csfmake.exe;*
$
$EXIT:
$   sav_ver = f$verify(sav_ver)
$   EXIT
