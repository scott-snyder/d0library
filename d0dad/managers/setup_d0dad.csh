#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#
#  Setup file for D0DAD.  Needed only to define management executable...
#
alias d0dad `vff '$d0d0dad/d0dad.x'`
alias deb_d0dad `vff '$d0d0dad/deb_d0dad.x'`
alias event_scan `vff '$d0d0dad/event_scan.x'`
alias deb_event_scan `vff '$d0d0dad/deb_event_scan.x'`
alias check_newruns `vff '$d0d0dad/check_newruns'`
alias deb_check_newruns `vff '$d0d0dad/deb_check_newruns'`

setenv d0d0dad__todo        `uff $d0d0dad__catalogs/todo`
setenv d0d0dad__temp        `uff $d0d0dad__catalogs/working`
setenv d0d0dad__archive     `uff $d0d0dad__catalogs/uevtcat`
setenv d0d0dad__streams     `uff $d0d0dad__catalogs/streams`

setenv d0dad_todo        $d0d0dad__todo
setenv d0dad_temp        $d0d0dad__temp
setenv d0dad_archive     $d0d0dad__archive
setenv d0dad_streams     $d0d0dad__streams

