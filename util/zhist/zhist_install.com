$!From:	D0FSU1::XIAO          2-NOV-1992 16:08:03.35
$!To:	SERBAN
$!CC:	VAXINO::YOUSSEF,XIAO
$!Subj:	zhist_install.com for d0library distribution
$!
$!Hi Serban,
$!	We updated the zHist installation procedure.  Please replace
$!zhist_install.com in the d0library area by the following file.  The old
$!file is obsolete.
$!			Dong Xiao/Saul Youssef
$!
$!************************************************************************
$!
$!  This command file grabs instl.com from freehep.scri.fsu.edu,
$!  (IP number 192.70.169.247), then excute it to install zHist package.
$!                                         -- Dong Xiao,  Oct. 27, 1992.
$!  For unix machines, use zhist_install.
$
$ ftp freehep.scri.fsu.edu
USER anonymous
PASS freehep_user
cd //freehep/analysis/zhist
get instl.com
quit
$ @instl.com
$ delete/noconfirm/nolog instl.com;*
$ exit
