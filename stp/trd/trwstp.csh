#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
#     A.M.Jonckheere      21-MAR-1988
#
#   TRWSTP.COM - Create TRD ZEBSTP banks
#
#------------------------------------------------
#
if( $1 == debug )then
  set prefix = deb_
else
  set prefix = ''
endif
#
echo "Linking TRD/STP object library"
ar x `uff $d0stp/${prefix}trd.a` trwstp.o
$f77 $ldflags -o ${prefix}trwstp trwstp.o \
  `uff $d0stp/${prefix}trd.a` \
  `uff $d0trd_util/${prefix}trd_util.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
echo "Running TRD/STP image"
ln -sf `uff $d0stp/trd/manea10.dat` manea10 >& /dev/null
ln -sf `uff $d0stp/trd/manea20.dat` manea20 >& /dev/null
ln -sf `uff $d0stp/trd/maneb10.dat` maneb10 >& /dev/null
ln -sf `uff $d0stp/trd/maneb20.dat` maneb20 >& /dev/null
ln -sf `uff $d0stp/trd/manec10.dat` manec10 >& /dev/null
ln -sf `uff $d0stp/trd/manec20.dat` manec20 >& /dev/null
ln -sf `uff $d0stp/trd/maxe10.dat` maxe10 >& /dev/null
ln -sf `uff $d0stp/trd/maxe20.dat` maxe20 >& /dev/null
ln -sf `uff $d0stp/trd/pretotal310.dat` pretotal310 >& /dev/null
ln -sf `uff $d0stp/trd/pretotal320.dat` pretotal320 >& /dev/null
ln -sf `uff $d0stp/trd/pretotale10.dat` pretotale10 >& /dev/null
ln -sf `uff $d0stp/trd/pretotale20.dat` pretotale20 >& /dev/null
ln -sf `uff $d0stp/trd/pretronq310.dat` pretronq310 >& /dev/null
ln -sf `uff $d0stp/trd/pretronq320.dat` pretronq320 >& /dev/null
ln -sf `uff $d0stp/trd/pretronqu10.dat` pretronqu10 >& /dev/null
ln -sf `uff $d0stp/trd/pretronqu20.dat` pretronqu20 >& /dev/null
ln -sf `uff $d0stp/trd/prlikecla10.dat` prlikecla10 >& /dev/null
ln -sf `uff $d0stp/trd/prlikecla20.dat` prlikecla20 >& /dev/null
ln -sf `uff $d0stp/trd/prlikeclb10.dat` prlikeclb10 >& /dev/null
ln -sf `uff $d0stp/trd/prlikeclb20.dat` prlikeclb20 >& /dev/null
ln -sf `uff $d0stp/trd/prlikeclc10.dat` prlikeclc10 >& /dev/null
ln -sf `uff $d0stp/trd/prlikeclc20.dat` prlikeclc20 >& /dev/null
ln -sf `uff $d0stp/trd/prliketot10.dat` prliketot10 >& /dev/null
ln -sf `uff $d0stp/trd/prliketot20.dat` prliketot20 >& /dev/null
ln -sf `uff $d0stp/trd/stp_trd_1a0.dat` stp_trd_1a0 >& /dev/null
ln -sf `uff $d0stp/trd/stp_trd_1b0.dat` stp_trd_1b0 >& /dev/null
ln -sf `uff $d0stp/trd/canary_fit_1.dat` canary_fit_1 >& /dev/null
ln -sf `uff $d0stp/trd/tables.dat` tables_jfl >& /dev/null
# YD stuff (put in April 10th 1996)
ln -sf `uff $d0stp/trd/con1_trd.dat` con1_trd >& /dev/null
ln -sf `uff $d0stp/trd/con2_trd.dat` con2_trd >& /dev/null
ln -sf `uff $d0stp/trd/conv_cdc.dat` conv_cdc >& /dev/null
ln -sf `uff $d0stp/trd/ele1_trd.dat` ele1_trd >& /dev/null
ln -sf `uff $d0stp/trd/ele2_trd.dat` ele2_trd >& /dev/null
ln -sf `uff $d0stp/trd/elec_cdc.dat` elec_cdc >& /dev/null
ln -sf `uff $d0stp/trd/fak1_trd.dat` fak1_trd >& /dev/null
ln -sf `uff $d0stp/trd/fak2_trd.dat` fak2_trd >& /dev/null
ln -sf `uff $d0stp/trd/fake_cdc.dat` fake_cdc >& /dev/null
ln -sf `uff $d0stp/trd/min1_trd.dat` min1_trd >& /dev/null
ln -sf `uff $d0stp/trd/min2_trd.dat` min2_trd >& /dev/null
ln -sf `uff $d0stp/trd/mini_cdc.dat` mini_cdc >& /dev/null
ln -sf `uff $d0stp/trd/celor_min3ch_cdc_nosmear_new1_241604.dat` celor_3ch_cdc >& /dev/null
ln -sf `uff $d0stp/trd/celor_min3ch_dedxcut_cdc_new_241604.dat` celor_3ch >& /dev/null
# AZ stuff (put in April 10th 1996)
ln -sf `uff $d0stp/trd/trd_cor_temp.dat` trd_cor_temp >& /dev/null
#
ln -sf trd_stpfile.dat trd_stpfile >& /dev/null
eval ${prefix}trwstp
rm ${prefix}trwstp trwstp.o fort.*
