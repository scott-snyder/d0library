#
setup cps  v2_9
setup rbio v7_2
set tmp = ($d0inc,$cwd/source,$CPSINC)
setenv d0inc $tmp
set tmp = ($d0params,$cwd/source)
setenv d0params $tmp

if ( $D0FLAVOR == 'SIUNIX' ) then
  set Link = f77
  set LIBSUN = '-lsun'
  set Forflags = ''
  set lib = libsgi
endif
if ( $D0FLAVOR == 'IBMAIX' ) then
#  cp $CPSINC/acp_user.inc ./source
  set Link = xlf
  set LIBSUN = ''
  set Forflags = '-qposition=append -I$CPSINC'
  set lib = libibm
endif

rm *.o acplib.a >& /dev/null
fort	$Forflags \
	./source/ftype.for ./source/rcptype.for ./source/acp_mount.for \
	./source/acp_open.for ./source/acp_success.for
fort	./source/clib.c
ar cr acplib.a *.o
echo Finished Building acplib.a
fort	$Forflags \
	./source/acp_output.for ./source/acp_input.for ./source/acp_fzio.for \
	./source/acp_dzero.for ./source/dummy.for

$Link    $f77flags -o d0geant_output.x	\
        ./acp_output.o		\
        acplib.a  $RBIO_LIB $CPSLIB $RBIO_LIB $LIBSUN
echo Finished Building d0geant_output.x
mv d0geant_output.x ../cps_batch

$Link	$f77flags -o d0geant_input.x 	\
	./acp_input.o		\
	acplib.a $RBIO_LIB $CPSLIB $RBIO_LIB $LIBSUN
echo Finished Building d0geant_input.x
mv d0geant_input.x ../cps_batch


$Link	$f77flags -o d0geant.x		\
	./acp_dzero.o		\
	./dummy.o		\
	./acp_fzio.o		\
	./$lib/fulld0_d0geant.o		\
	./$lib/localcps.o		\
	./$lib/localbeta.o		\
	acplib.a			\
	$CPSLIB				\
	./$lib/d0geant.a			\
	./$lib/showerlibrary.a		\
	./$lib/calor_util.a		\
	./$lib/cd_util.a			\
	./$lib/trd_util.a		\
	./$lib/vtx_util.a		\
	./$lib/cdc_util.a		\
	./$lib/fdc_util.a		\
	./$lib/cd_util.a			\
	./$lib/muon_util.a		\
	./$lib/general.a			\
	./$lib/gxint.o			\
	./$lib/geant.a			\
	./$lib/pawlib.a		\
	./$lib/graflib.a		\
	./$lib/grafX11.a		\
	./$lib/packlib.a		\
	./$lib/genlib.a		\
	./$lib/kernlib.a		\
	./$lib/packlib.a		\
	./$lib/unix.a -lcurses -lX11

echo Finished Building d0geant.x
mv d0geant.x ../cps_batch
/bin/rm *.o acplib.a >& /dev/null
