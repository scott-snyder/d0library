set framelib =   `uff $d0d0geant/${pfx}d0geant.a`
ar x $framelib dzero.o
set objects = ( $objects dzero.o `uff $d0cernlib/gxint315.o` )
 
set util_libs = ( \
    `uff $d0showerlibrary/${pfx}showerlibrary.a`  \
    `uff $d0calor_util/${pfx}calor_util.a`        \
    `uff $d0cd_util/${pfx}cd_util.a`              \
    `uff $d0trd_util/${pfx}trd_util.a`            \
    `uff $d0vtx_util/${pfx}vtx_util.a`            \
    `uff $d0cdc_util/${pfx}cdc_util.a`            \
    `uff $d0fdc_util/${pfx}fdc_util.a`            \
    `uff $d0cd_util/${pfx}cd_util.a`              \
    `uff $d0muon_util/${pfx}muon_util.a`          \
    `uff $d0level1/${pfx}level1.a`                \
    `uff $d0dbl3/${pfx}d0dbl3.a`                  \
    `uff $d0dbl3/${pfx}dbl3.a`                    \
    `uff $d0lcompack/${pfx}compack.a`             \
    `uff $d0general/${pfx}general.a`              \
    `uff $d0unix/${pfx}unix.a`                    )
 
set cern_libs = ( \
   `uff $d0cernlib/geant315.a` \
   `uff $d0cernlib/graflib.a`  \
   `uff $d0cernlib/grafX11.a`  \
   `uff $d0cernlib/pawlib.a`   \
   `uff $d0cernlib/packlib.a`  \
   `uff $d0cernlib/mathlib.a`  \
   `uff $d0cernlib/kernlib.a`  )
 
$f77 $ldflags -o $exe \
  $objects \
  $force_objects \
  $user_libs \
  $framelib \
  $util_libs \
  $cern_libs \
  $syslibs \
  -lX11
