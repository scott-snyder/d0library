$!========================================================================
$!
$! Name      : FDWSTP
$!
$! Purpose   : Creates the two FDC_STPFILEs for FDC event data processing.
$!
$! Arguments : none
$!
$!    Created  10-MAY-1988   Jeffrey Bantly
$!    2-JUL-1991   Jeffrey Bantly   generate two files now that are
$!                                  used, one for Monte Carlo runs
$!                                  and one for D0 Hall runs
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   wr := write sys$output
$
$   group := fdc
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
$!
$ WRITE SYS$OUTPUT "Linking MC FDC/STP object library"
$!
$   LINK/nomap/EXE=FDWSTP -
        D0$STP:FDC/INCLUDE=(FDWSTP_MC),FDC/LIBRARY,-
        D0$CD_UTIL:CD_UTIL.OLB/LIB,-
        D0$TRD_UTIL:TRD_UTIL.OLB/LIB,-
        D0$VTX_UTIL:VTX_UTIL.OLB/LIB,-
        D0$CDC_UTIL:CDC_UTIL.OLB/LIB,-
        D0$FDC_UTIL:FDC_UTIL.OLB/LIB,-
        D0$CD_UTIL:CD_UTIL.OLB/LIB,-
        D0$GENERAL:GENERAL/LIBRARY,-
        'cernp'         ! D0$CERNLIB:PACKLIB/LIB,KERNLIB/LIBRARY etc
$!
$ WRITE SYS$OUTPUT "Running MC FDC/STP image"
$!
$ DEFINE/USER_MODE FOR003 NL:
$   RUN FDWSTP
$ DELETE/NOLOG FDWSTP.EXE.*
$!
$!-------------------------------------------------------------------
$!
$ WRITE SYS$OUTPUT "Linking D0 FDC/STP object library"
$!
$   LINK/nomap/EXE=FDWSTP -
        D0$STP:FDC/INCLUDE=(FDWSTP_D0),FDC/LIBRARY,-
        D0$CD_UTIL:CD_UTIL.OLB/LIB,-
        D0$TRD_UTIL:TRD_UTIL.OLB/LIB,-
        D0$VTX_UTIL:VTX_UTIL.OLB/LIB,-
        D0$CDC_UTIL:CDC_UTIL.OLB/LIB,-
        D0$FDC_UTIL:FDC_UTIL.OLB/LIB,-
        D0$CD_UTIL:CD_UTIL.OLB/LIB,-
        D0$GENERAL:GENERAL/LIBRARY,-
        'cernp'         ! D0$CERNLIB:PACKLIB/LIB,KERNLIB/LIBRARY etc
$!
$ WRITE SYS$OUTPUT "Running D0 FDC/STP image"
$!
$ DEFINE FSTP_RCP D0$STP$FDC:FSTP.RCP
$ DEFINE/USER_MODE FOR003 NL:
$   RUN FDWSTP
$ DELETE/NOLOG FDWSTP.EXE.*
$!
$EXIT:
$ EXIT
