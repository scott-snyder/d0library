#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
#     Ghita Rahal-Callot      17-FEB-1988
#     Updated  20-APR-1989    Qizhong Li-Demarteau    for new banks
#     Updated  27-MAR-1992    Qizhong Li-Demarteau   create two STPFILE for
#                                CDC now: one for MC data; one for D0 data,
#                                and added DELAY_GUIDO.DAT file
#     Updated  21-JUN-1992    Qizhong Li-Demarteau   added cdc_gain file
#                                and added delay_guido_2.dat file
#     Updated  17-JUL-1992    Qizhong Li-Demarteau change STP file to X-mode
#     Updated  31-DEC-1992    Qizhong Li-Demarteau added CDCSTP.RCP and
#                                              D.Pizzuto's routines and
#                                              nonlinearity data files
#
#   Creates the CDC_STPFILE and CDC_D0STPFILE data file for CDC processing
#   it uses the following files from D0$STP$CDC
#
# CDWSTP,BLDALH,BLDGEH,BLDGNH,BLDMAT,BLDPDH,BLDRFT,BLDTMH,BLDWAL
# BLDCBD, BLDTMP, BLDALH_0, DRDDLY, DCSTOR, DRDGNS,
# BLDCBD_D0, BLDNLX, BLDTVA, BLDXXP
#
#------------------------------------------------
#
if( $1 == debug )then
  set prefix = deb_
else
  set prefix = ''
endif
#
echo "Linking CDC/STP object library"
ar x `uff $d0stp/${prefix}cdc.a` cdwstp.o
$f77 $ldflags -o ${prefix}cdwstp cdwstp.o \
  `uff $d0stp/${prefix}cdc.a` \
  `uff $d0cdc_util/${prefix}cdc_util.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
echo "Running CDC/STP image"
ln -sf `uff $d0stp/cdc/dl_velocities.dat` dl_velocities >& /dev/null
ln -sf `uff $d0stp/cdc/delay_guido_3.dat` delay_guido >& /dev/null
ln -sf `uff $d0stp/cdc/gain_dec92.dat` cdc_gain >& /dev/null
ln -sf `uff $d0stp/cdc/tpdelays.dat` tp_delays >& /dev/null
ln -sf `uff $d0stp/cdc/d0vclbdiffdb.dat` vel_diff>& /dev/null
ln -sf `uff $d0stp/cdc/d0nonlini.dat` nonlin_inner>& /dev/null
ln -sf `uff $d0stp/cdc/d0nonlino.dat` nonlin_outer>& /dev/null
ln -sf `uff $d0stp/cdc/cdcstp.rcp` cdcstp_rcp >& /dev/null
ln -sf cdc_stpfile.dat cdc_stpfile >& /dev/null
ln -sf cdc_d0stpfile.dat cdc_d0stpfile >& /dev/null
eval ${prefix}cdwstp
rm ${prefix}cdwstp cdwstp.o
rm fort.*
