$!------------------------------------------------
$!
$!     R.Raja 31-Jul-1989
$!
$!   Creates the GEN_STPFILE data file for GEN processing
$!   it uses the files from D0$STP:GEOGEN
$!
$!
$!------------------------------------------------
$!
$   wr := write sys$output
$
$   group := geogen
$   IF f$trnlnm("D0$RELEASE") .nes. ""
$   THEN
$       WR "****''group'-I- Doing Release, check if we need to run ****"
$       IF f$search("d0$release:[stp]''group'.dir") .eqs. ""
$       THEN
$           WR "*** No STP group ''group' - abort"
$           goto exit
$       ENDIF
$       IF f$search("d0$release:[stp.''group']*.*") .eqs. ""
$       THEN
$           WR "*** No new data in ''group' - abort"
$           goto exit
$       ENDIF
$   ENDIF
$
$ write sys$output "Linking GEOGEN/STP object library"
$   LINK/nomap/EXE=GNWSTP -
D0$STP:GEOGEN/INCLUDE=(GNWSTP),GEOGEN/LIBRARY,-
D0$GENERAL:GENERAL/LIBRARY,-
'cernp'                 ! D0$CERNLIB:PACKLIB/LIB,KERNLIB/LIBRARY etc
$ write sys$output "Running GEOGEN/STP image"
$   DEFINE GNWSTP_RCP d0$stp$geogen:GNWSTP.RCP
$   RUN GNWSTP
$ delete/nolog GNWSTP.EXE.*
$exit:
$ exit
