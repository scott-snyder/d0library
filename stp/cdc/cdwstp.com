$!------------------------------------------------
$!
$!     Ghita Rahal-Callot      17-FEB-1988
$!     Updated  20-APR-1989    Qizhong Li-Demarteau    for new banks
$!
$!     Updated  27-MAR-1992    Qizhong Li-Demarteau   create two STPFILE for
$!                                CDC now: one for MC data; one for D0 data,
$!                                and added DELAY_GUIDO.DAT file
$!     Updated  21-JUN-1992    Qizhong Li-Demarteau   added cdc_gain file
$!                                and added delay_guido_2.dat file
$!     Updated  17-JUL-1992    Qizhong Li-Demarteau change STP file to X-mode
$!     Updated  31-DEC-1992    Qizhong Li-Demarteau added CDCSTP.RCP and
$!                                              D.Pizzuto's routines and
$!                                              nonlinearity data files
$!
$!   Creates the CDC_STPFILE and CDC_D0STPFILE data file for CDC processing
$!   it uses the following files from D0$STP$CDC
$!
$! CDWSTP,BLDALH,BLDGEH,BLDGNH,BLDMAT,BLDPDH,BLDRFT,BLDTMH,BLDWAL
$! BLDCBD, BLDTMP, BLDALH_0, DRDDLY, DCSTOR, DRDGNS,
$! BLDCBD_D0, BLDNLX, BLDTVA, BLDXXP
$!
$!------------------------------------------------
$!
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   wr := write sys$output
$
$   group := cdc
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
$ write sys$output "Linking CDC/STP object library"
$   LINK/nomap/EXE=CDWSTP -
D0$STP:CDC/INCLUDE=(CDWSTP),CDC/LIBRARY,-
D0$CDC_UTIL:CDC_UTIL/LIBRARY,-
D0$GENERAL:GENERAL/LIBRARY,-
'cernp'         ! D0$CERNLIB libraries
$
$ write sys$output "Running CDC/STP image"
$ copy d0$stp$cdc:dl_velocities.dat *
$! copy d0$stp$cdc:delay_guido.dat *
$! copy d0$stp$cdc:delay_guido_2.dat *
$ copy d0$stp$cdc:delay_guido_3.dat *
$! copy d0$stp$cdc:gain_jun92.dat *
$! copy d0$stp$cdc:gain_sep92.dat *
$ copy d0$stp$cdc:gain_dec92.dat *
$ copy d0$stp$cdc:tpdelays.dat *
$ copy d0$stp$cdc:d0vclbdiffdb.dat *
$ copy d0$stp$cdc:d0nonlini.dat *
$ copy d0$stp$cdc:d0nonlino.dat *
$ COPY D0$STP$CDC:CDCSTP.RCP *
$ define delay_guido delay_guido_3.dat
$ define cdc_gain gain_dec92.dat
$ define tp_delays       tpdelays.dat
$ define vel_diff        d0vclbdiffdb.dat
$ define nonlin_inner    d0nonlini.dat
$ define nonlin_outer    d0nonlino.dat
$ define CDCSTP_RCP cdcstp.rcp
$ define/user_mode for003 nl:
$   RUN CDWSTP
$ delete/nolog cdwstp.exe;*
$ delete/nolog dl_velocities.dat;*
$ delete/nolog delay_guido_3.dat;*
$ delete/nolog gain_dec92.dat;*
$ delete/nolog tpdelays.dat;*
$ delete/nolog d0vclbdiffdb.dat;*
$ delete/nolog d0nonlini.dat;*
$ delete/nolog d0nonlino.dat;*
$EXIT:
$   EXIT
