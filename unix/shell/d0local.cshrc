#------------------------------------------------------------------------------
#
# Name      : d0local.cshrc
#
# Purpuse   : Standard d0 .cshrc
#
# Arguments : None
#
# Created 21-JUN-1991   Herbert B. Greenlee
# Updated 28-Mar-1992   Herbert B. Greenlee
#    Added machine dependence for AIX compatibility
# Updated 16-May-1994   John Drinkard
#    Added some machine dependence for SUN compatibility
#
# This file sets standard d0 aliases.  It should be sourced from the users
# .cshrc.
#
#------------------------------------------------------------------------------
#
# Define standard aliases
#
alias nolibtest 'set argv=(-o \!*) ;source `find_first $d0unix/libtest.csh`'
alias nolibprod 'set argv=(-o all) ;source `find_first $d0unix/libtest.csh`'
alias access 'set argv=(\!*) ;source `find_first $d0unix/access.csh`'
alias libtest 'set argv=(\!*) ;source `find_first $d0unix/libtest.csh`'
alias libprod 'set argv=(\!*) ;source `find_first $d0unix/libprod.csh`'
alias d0setup 'set argv=(\!*) ;source `find_first $d0util/d0setup.csh`'
alias d0entry 'egrep -i \!* `find_first $d0docs/entry_point.lis`'
#replaced by msql/web version
#alias whod0 'egrep -i \!* `find_first $d0docs/user_list.txt`'
#
set lexlib = -ll
#
# Machine dependent aliases and shell variables
#
set sys = `uname -s`
if( $sys =~ IRIX* )then
  set sys = IRIX
  set syslv = `uname -r | tr -d .`
endif
if( $sys == AIX )then
  set syslv = `uname -v`
  if( $syslv < 4 )set sys = AIX.3
endif
#
# IBM/AIX 3
#
if( $sys == AIX.3 )then
  set f77 = xlf
  set cc = cc
  set f77flags = "-qintlog -qcharlen=4096 -qextname -qmaxmem=5000 -qnoescape -NQ30000 -NT40000"
  set ccflags = '-DD0FLAVOR=IBMAIX'
  set lccflags = '-DD0FLAVOR=IBMAIX'
  set debflags = '-g'
  set optflags = '-O'
  set arflags = 'crsl'
  set ldflags = '-Wl,-berok'
  set syslibs = -lcurses
#
# IBM/AIX 4
#
else if ( $sys == AIX ) then
  set f77 = xlf
  set cc = cc
  set f77flags = "-qxlf77=blankpad -qintlog -qcharlen=4096 -qextname -qmaxmem=5000 -qnoescape -NQ30000 -NT40000 -qctyplss"
  set ccflags = '-DD0FLAVOR=IBMAIX -D_ALL_SOURCE'
  set lccflags = '-DD0FLAVOR=IBMAIX -D_ALL_SOURCE'
  set debflags = '-g'
  set optflags = '-O'
  set arflags = 'crsl'
  set ldflags = '-Wl,-berok'
  set syslibs = -lcurses
#
# Dec/ULTRIX
#
else if( $sys == ULTRIX )then
  alias d0entry 'grep -i \!* `find_first $d0docs/entry_point.lis`'
  alias whod0 'grep -i \!* `find_first $d0docs/user_list.txt`'
  alias awk /usr/bin/nawk
  set f77 = f77
  set cc = cc
  set f77flags = "-vms -col72 -assume backslash -G 3 -align dcommons "
  set ccflags = '-G 3 -DD0FLAVOR=ULTRIX'
  set lccflags = '-DD0FLAVOR=ULTRIX'
  set debflags = '-O0 -g2'
  set optflags = '-O2'
  set arflags = 'crsl'
  set ldflags = ''
  set syslibs = -lX11
  set batrun_libs = /products/condor/lib
  set bflags = " -h$batrun_libs -tr -B -Wl,-L$batrun_libs -lc "
#
# Linux
#
else if ($sys == Linux) then
  set cc = cc
  set f77 = "g77"
  set f77flags = "-fdollar-ok -fno-automatic -fno-backslash -fugly -fone-underscore"
  set ccflags = "-fdollars-in-identifiers -DD0FLAVOR=LINUX -I/usr/include/ncurses"
  set lccflags = "-fdollars-in-identifiers -DD0FLAVOR=LINUX -I/usr/include/ncurses"
  set debflags = '-O0 -g'
  set optflags = '-O2'
  set arflags = 'crsl'
  set ldflags = ''
  set syslibs = -lcurses
  set lexlib = -lfl
#
# SGI/IRIX >=4
#
else if( $sys == IRIX )then
  set G = 3
  if( `uname -r | cut -c1` > 4 )set G = 0
  alias awk /usr/bin/nawk
  set f77 = f77
  set cc = cc
  set f77flags = "-o32 -nocpp -static -col72 -backslash -G $G"
  if( $syslv < 65 )set f77flags = "$f77flags -Nn15000 -Wb,-force_branch_fixup -Olimit 3000 -Wo,-loopunroll,2"
  if( $syslv >= 65 )set f77flags = "$f77flags -Wl,-LD_MSG:off=84"
  set ccflags = "-o32 -dollar -G $G -DD0FLAVOR=SIUNIX"
  if( $syslv < 65 )set ccflags = "$ccflags -woff 275 -Wf,-XNl8192"
  if( $syslv >= 65 )set ccflags = "$ccflags -woff 1172 -Wl,-LD_MSG:off=84"
  set lccflags = '-o32 -dollar -DD0FLAVOR=SIUNIX'
  if( $syslv < 65 )set lccflags = "$lccflags -woff 275 -Wf,-XNl8192"
  if( $syslv >= 65 )set lccflags = "$lccflags -woff 1167,1172,1116,1110 -Wl,-LD_MSG:off=84"
  set debflags = '-O0 -g2'
  set optflags = '-O2'
  set arflags = 'crsl'
  set ldflags = '-Wl,-U'
  set syslibs = -lcurses
#
# SunOS
#
else if( $sys == SunOS )then
  alias awk /usr/bin/nawk
  alias echo /usr/5bin/echo
  set f77 = f77
  set cc = acc
  set f77flags = " -xl "
  set ccflags = ' -w -DD0FLAVOR=SUNOS '
  set lccflags = ' -DD0FLAVOR=SUNOS '
  set debflags = ' -g '
  set optflags = '-O2'
  set arflags = 'crsl'
  set ldflags = ' -lg '
  set syslibs = -lcurses
#
# Alpha/OSF
#
else if( $sys == OSF1 )then
  unalias make
  alias awk /usr/bin/nawk
  set f77 = f77
  set cc = cc
  set f77flags = '-col72 -assume backslash -G 3 -align dcommons -static -nocpp'
  set ccflags = '-w -G 3 -DD0FLAVOR=ALFOSF'
  set lccflags = '-w -DD0FLAVOR=ALFOSF'
  set debflags = '-g'
  set optflags = '-O2'
  set arflags = 'crsl'
  set ldflags = '-D 40000000 -T 20000000'
  set syslibs = ''
endif
#
# Conditional alias for GNU make
#
if( -x /usr/local/bin/gmake )then
  alias make /usr/local/bin/gmake
endif
