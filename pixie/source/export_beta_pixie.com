$!========================================================================
$!
$! Name      : EXPORT_BETA_PIXIE
$!
$! Purpose   : To export the Beta Release ALL_DISPLAY_PIXIE.EXE with
$!             SETUP_PIXIE.COM  and RCP files of D0SFT *via DECNet*.
$!
$! Arguments : 
$!
$! Created  11-MAR-1993   Nobuaki Oshima
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   WR          :== WRITE SYS$OUTPUT
$!
$   INQUIRE  ans   "Do you have free blocks > 7500 ? [Y/N]"
$   IF ans .EQS. "Y" .OR. ans .EQS. "y"
$   THEN
$       WR "  "
$       WR " We expect you maintain the D0LIBRARY properly !!! "
$       WR " ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ "
$       WR "  "
$       INQUIRE vers " Which version, DI3000(DI3) or XWindow(XWN)? [DI3]" 
$       IF vers .EQS. "XWN"
$       THEN
$           WR "  "
$           WR " *************************************************** "
$           WR " * You will receive the XWNDI3 emulator version of * "
$           WR " *          ALL_DISPLAY_XWN_PIXIE.EXE              * "
$           WR " *************************************************** "
$           WR "  "
$           COPY/LOG -
            D0SFA::D0CMS_PRJ$HROOT:[PIXIE.XWNEMU]ALL_DISPLAY_XWN_PIXIE.EXE *.*
$       ELSE
$           WR "  "
$           WR " *************************************************** "
$           WR " * You will receive the standard DI3000 version of * "
$           WR " *            ALL_DISPLAY_PIXIE.EXE                * "
$           WR " *************************************************** "
$           WR "  "
$           COPY/LOG -
            D0SFA::D0CMS_PRJ$HROOT:[PIXIE]ALL_DISPLAY_PIXIE.EXE *.*
$       ENDIF
$       COPY/LOG D0SFA::D0CMS_PRJ$HROOT:[PIXIE]*.RCP *.*
$       COPY/LOG D0SFA::D0CMS_PRJ$HROOT:[PIXIE]SETUP_PIXIE.EXPORT *.COM
$       WR "  "
$       WR " >>> Now try; $ @SETUP_PIXIE <RET> "
$       WR "  "
$   ELSE
$       WR "  "
$       WR " Make free blocks on your disk first! Bye - Nobu "
$       WR "  "
$   ENDIF
$EXIT:
$   EXIT
