$!------------------------------------------------
$!
$! Name      : CAWSTP
$!
$! Purpose   : Create and run CAWSTP.EXE to create
$!             files CAL_STPFILE.DAT and LV0_STPFILE.DAT
$!
$! Arguments : None
$!
$! Created  10-FEB-1989   Harrison B. Prosper
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
$!
$   WR "  "
$   WR " Create:        ''deb_'CAWSTP.EXE"
$   WR "  "
$!
$
$!.....        check DEB_BETA_RELEASE.OPT
$
$ IF "''linkopt'" .eqs. ""
$ THEN
$     linkopt = "''DEB_'BETA_RELEASE.OPT"
$ ENDIF
$
$ optfile = f$search("''linkopt'")
$ IF optfile .eqs. ""
$ THEN
$     wr "        No option file ''linkopt', will link without"
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
$   LINK/nomap'swit'/EXE='deb_'CAWSTP -
        'BETA_OPT' -
    D0$STP:'DEB_'GEOCAL/INCLUDE=(CAWSTP)/LIB,-
    D0$CALOR_UTIL:'DEB_'CALOR_UTIL/LIBRARY,-
    d0$dbl3:'deb_'d0dbl3/lib,-
    d0$dbl3:'deb_'dbl3/lib,-
    d0$dbl3:'deb_'cpc/lib,-
    d0$compack:'deb_'compack/lib,-
    D0$GENERAL:'DEB_'GENERAL/LIBRARY,-
    d0$cernlib:geant315/lib,-
    'cernp'     ! D0$CERNLIB:PACKLIB/LIBRARY,KERNLIB/LIBRARY etc
$!
$ tmp_ver = f$verify(tmp_ver)
$   WR "  "
$   WR " Invoke:        SETUP_CAWSTP.COM"
$   WR "  "
$   @D0$STP$GEOCAL:SETUP_CAWSTP
$   DEFINE/NOLOG FOR003 NL:
$!
$   IF p1 .eqs. "DEBUG"
$   THEN
$       WR " "
$       WR "        DEB_CAWSTP.EXE created"
$       WR "          will not be run"
$       WR " "
$       goto exit
$   ENDIF
$!
$   WR "  "
$   WR " Run:           CAWSTP.EXE"
$   WR "  "
$!
$   RUN 'deb_'CAWSTP
$!
$!------------------------------------------------
$!   Tidy up a little
$!------------------------------------------------
$!   PURGE/NOCONFIRM/NOLOG CAWSTP.EXE
$!   PURGE/NOCONFIRM/NOLOG CAL_STPFILE,LV0_STPFILE
$!
$   delete/nolog cawstp.exe.*
$   delete/NOCONFIRM/NOLOG -
        SRCP_REST.dat;*,SRCP_ECAL.dat;*,SRCP_UCAL.dat;*,srcp_lv0.dat;*
$EXIT:
$   sav_ver = f$verify(sav_ver)
$   EXIT
