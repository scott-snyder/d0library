#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
# Name      : upg_geant_isu_lnk.csh
#
# Purpose   : Link upg_geant
#
# Usage     :
#
# % upg_geant_isu_lnk.csh  [-d] [-b]
#
# Options   :
#
# -o   - Link an optimized  version of upg_geant (default)
# -d   - Link a debug version of upg_geant
# -b   - Link a batrun version of upg_geant
#
# Created   09-Feb-1994  Michael Wendling
#
#------------------------------------------------
#
set deb = 0
set opt = 0
set bat = 0
set prefix = ''
set tail = ''
#
set argv = (`getopt dbo $argv`)
#
while("$argv[1]" != '--')
  switch($argv[1])
  case -d:
    set deb = 1
    shift
    breaksw
  case -o:
    set opt = 1
    shift
    breaksw
  case -b:
    set bat = 1
    shift
    breaksw
  endsw
end
#
if($deb && $bat || $deb && $opt || $opt && $bat)then
   /bin/echo "choose only one of -o, -d and -c"
   exit 0
endif
#
if($deb) set prefix = deb_
if($bat)then
 set tail = _batrun
 ar x `uff $d0d0geant/${prefix}d0geant.a` dzero.o
else
 set bflags = ''
 grep -v "CALL AVOIDNFS" `uff $d0d0geant/dzero/dzero.f` > dzero.f 
 $f77 -c dzero.f
endif
#
echo "linking ${prefix}upg_geant${tail}"
echo
$f77 $bflags $ldflags -o ${prefix}upg_geant${tail} dzero.o \
  `force_load $d0upg_geant/upg_geant_d0geant.o` \
  `force_load $d0upg_geant/upg_geant.a` \
  `uff $d0library/phase1_geant/phase1_geant.a` \
  `uff $d0d0geant/deb_d0geant.a` \
  `uff $d0showerlibrary/${prefix}showerlibrary.a` \
  `uff $d0physics_util/${prefix}physics_util.a` \
  `uff $d0calor_util/${prefix}calor_util.a` \
  `uff $d0calor_off/${prefix}calor_off.a` \
  `uff $d0cd_util/${prefix}cd_util.a` \
  `uff $d0cdc_util/${prefix}cdc_util.a` \
  `uff $d0fdc_util/${prefix}fdc_util.a` \
  `uff $d0trd_util/${prefix}trd_util.a` \
  `uff $d0vtx_util/${prefix}vtx_util.a` \
  `uff $d0muon_util/${prefix}muon_util.a` \
  `uff $d0cd_util/${prefix}cd_util.a` \
  `uff $d0dbl3/${prefix}d0dbl3.a` \
  `uff $d0dbl3/${prefix}dbl3.a` \
  `uff $d0lcompack/${prefix}compack.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/gxint.o` \
  `uff $d0cernlib/geant.a` \
  `uff $d0cernlib/pawlib.a` \
  `uff $d0cernlib/graflib.a` \
  `uff $d0cernlib/grafX11.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/genlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
#
rm -rf dzero.o dzero.f
#
echo "Link done"
echo
#
exit:
exit
