#! /bin/csh -f
#
#  Translate the master copy of the file catalog into local filenames.
#
if( -f /d0library/admin/.cshrc ) source /d0library/admin/.cshrc
if( -f /d0library/admin/.login ) source /d0library/admin/.login

date
libtest d0dad
alias d0dad `uff '$d0d0dad/d0dad.x'`
set destdir=`echo $d0d0dad__catalogs | cut -f1 -d' ' `

cd $d0fs_d0dad

foreach file (*.filecat)
 echo Translating $file at `date`
 d0dad /debug=4 /mode=translate $file $destdir/$file
end
#
date
