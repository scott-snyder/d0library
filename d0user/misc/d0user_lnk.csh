set framelib =   `uff $d0d0user/${pfx}frame.a`
set objects = ( $objects d0user.o )
ar x $framelib d0user.o

set util_libs = ( \
    `uff $d0nodi3000/${pfx}nodi3000.a`         \
    `uff $d0physics_util/${pfx}physics_util.a` \
    `uff $d0calor_util/${pfx}calor_util.a`     \
    `uff $d0muon_util/${pfx}muon_util.a`       \
    `uff $d0level0/${pfx}level0.a`             \
    `uff $d0cd_util/${pfx}cd_util.a`           \
    `uff $d0trd_util/${pfx}trd_util.a`         \
    `uff $d0cdc_util/${pfx}cdc_util.a`         \
    `uff $d0fdc_util/${pfx}fdc_util.a`         \
    `uff $d0vtx_util/${pfx}vtx_util.a`         \
    `uff $d0cd_util/${pfx}cd_util.a`           \
    `uff $d0dbl3/${pfx}d0dbl3.a`               \
    `uff $d0dbl3/${pfx}dbl3.a`                 \
    `uff $d0lcompack/${pfx}compack.a`          \
    `uff $d0general/${pfx}general.a`           \
    `uff $d0tpmfarm/${pfx}tpmfarm.a`           \
    `uff $d0unix/${pfx}unix.a`                 )

set cern_libs = ( \
   `uff $d0cernlib/packlib.a` \
   `uff $d0cernlib/mathlib.a` \
   `uff $d0cernlib/kernlib.a` )

$f77 $ldflags -o $exe \
  $objects \
  $force_objects \
  $user_libs \
  $framelib \
  $util_libs \
  $cern_libs \
  $syslibs
