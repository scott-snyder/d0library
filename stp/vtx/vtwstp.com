$!------------------------------------------------
$!
$!     Ghita Rahal-Callot      17-FEB-1988
$!
$!   Creates the VTX_STPFILE data file for VTX processing
$!   it uses the files from D0$STP:VTX
$!
$!  Modified 25-SEPT-1992 P. Grudberg Define VTWSTP_RCP
$!
$!------------------------------------------------
$!
$   wr := write sys$output
$
$   group := vtx
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
$ write sys$output "Linking VTX/STP object library"
$   LINK/nomap/EXE=VTWSTP -
        D0$STP:VTX/INCLUDE=(VTWSTP),VTX/LIBRARY,-
        D0$CD_UTIL:CD_UTIL.OLB/LIB,-
        D0$TRD_UTIL:TRD_UTIL.OLB/LIB,-
        D0$VTX_UTIL:VTX_UTIL.OLB/LIB,-
        D0$CDC_UTIL:CDC_UTIL.OLB/LIB,-
        D0$FDC_UTIL:FDC_UTIL.OLB/LIB,-
        D0$CD_UTIL:CD_UTIL.OLB/LIB,-
        D0$GENERAL:GENERAL/LIBRARY,-
        'cernp'         ! D0$CERNLIB:PACKLIB/LIB,KERNLIB/LIBRARY
$ write sys$output "Running VTX/STP image"
$ define vtwstp_rcp d0$stp$vtx:vtwstp.rcp
$ define/user_mode for003 nl:
$   RUN VTWSTP
$ delete/nolog VTWSTP.EXE.*
$exit:
$ exit
