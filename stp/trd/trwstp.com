$!------------------------------------------------
$!
$!     A.M.Jonckheere      21-MAR-1988
$!
$!   TRWSTP.COM - Create TRD ZEBSTP banks
$!
$!------------------------------------------------
$!
$   wr := write sys$output
$
$   group := trd
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
$ write sys$output "Linking TRD/STP object library"
$ link/nomap/exe=trwstp -   ! **amj** tstpbk,tsttyp,-
    D0$STP:TRD/INCLUDE=TRWSTP/LIBRARY,-
    D0$CD_UTIL:CD_UTIL.OLB/LIB,-
    D0$TRD_UTIL:TRD_UTIL.OLB/LIB,-
    D0$VTX_UTIL:VTX_UTIL.OLB/LIB,-
    D0$CDC_UTIL:CDC_UTIL.OLB/LIB,-
    D0$FDC_UTIL:FDC_UTIL.OLB/LIB,-
    D0$CD_UTIL:CD_UTIL.OLB/LIB,-
    D0$GENERAL:GENERAL/LIBRARY,-
    'cernp'         ! D0$CERNLIB:PACKLIB/LIBRARY,GENLIB/LIBRARY,KERNLIB/LIBRARY
$ write sys$output "Running TRD/STP image"
$ DIR    = "D0$STP$TRD:"
$ DEFINE/NOLOG   MANEA10      'DIR'MANEA10.DAT
$ DEFINE/NOLOG   MANEA20      'DIR'MANEA20.DAT
$ DEFINE/NOLOG   MANEB10      'DIR'MANEB10.DAT
$ DEFINE/NOLOG   MANEB20      'DIR'MANEB20.DAT
$ DEFINE/NOLOG   MANEC10      'DIR'MANEC10.DAT
$ DEFINE/NOLOG   MANEC20      'DIR'MANEC20.DAT
$ DEFINE/NOLOG   MAXE10       'DIR'MAXE10.DAT
$ DEFINE/NOLOG   MAXE20       'DIR'MAXE20.DAT
$ DEFINE/NOLOG   PRETOTAL310  'DIR'PRETOTAL310.DAT
$ DEFINE/NOLOG   PRETOTAL320  'DIR'PRETOTAL320.DAT
$ DEFINE/NOLOG   PRETOTALE10  'DIR'PRETOTALE10.DAT
$ DEFINE/NOLOG   PRETOTALE20  'DIR'PRETOTALE20.DAT
$ DEFINE/NOLOG   PRETRONQ310  'DIR'PRETRONQ310.DAT
$ DEFINE/NOLOG   PRETRONQ320  'DIR'PRETRONQ320.DAT
$ DEFINE/NOLOG   PRETRONQU10  'DIR'PRETRONQU10.DAT
$ DEFINE/NOLOG   PRETRONQU20  'DIR'PRETRONQU20.DAT
$ DEFINE/NOLOG   PRLIKECLA10  'DIR'PRLIKECLA10.DAT
$ DEFINE/NOLOG   PRLIKECLA20  'DIR'PRLIKECLA20.DAT
$ DEFINE/NOLOG   PRLIKECLB10  'DIR'PRLIKECLB10.DAT
$ DEFINE/NOLOG   PRLIKECLB20  'DIR'PRLIKECLB20.DAT
$ DEFINE/NOLOG   PRLIKECLC10  'DIR'PRLIKECLC10.DAT
$ DEFINE/NOLOG   PRLIKECLC20  'DIR'PRLIKECLC20.DAT
$ DEFINE/NOLOG   PRLIKETOT10  'DIR'PRLIKETOT10.DAT
$ DEFINE/NOLOG   PRLIKETOT20  'DIR'PRLIKETOT20.DAT
$ DEFINE/NOLOG   STP_TRD_1A0  'DIR'STP_TRD_1A0.DAT
$ DEFINE/NOLOG   STP_TRD_1B0  'DIR'STP_TRD_1B0.DAT
$ DEFINE/NOLOG   CANARY_FIT_1  'DIR'CANARY_FIT_1.DAT
$ DEFINE/NOLOG   TABLES_JFL    'DIR'TABLES.DAT
$!
$!  YD stuff (put in April 10th 1996)
$!
$ DEFINE/NOLOG    CON1_TRD     'dir'CON1_TRD.DAT
$ DEFINE/NOLOG    CON2_TRD     'dir'CON2_TRD.DAT
$ DEFINE/NOLOG    CONV_CDC     'dir'CONV_CDC.DAT
$ DEFINE/NOLOG    ELE1_TRD     'dir'ELE1_TRD.DAT
$ DEFINE/NOLOG    ELE2_TRD     'dir'ELE2_TRD.DAT
$ DEFINE/NOLOG    ELEC_CDC     'dir'ELEC_CDC.DAT
$ DEFINE/NOLOG    FAK1_TRD     'dir'FAK1_TRD.DAT
$ DEFINE/NOLOG    FAK2_TRD     'dir'FAK2_TRD.DAT
$ DEFINE/NOLOG    FAKE_CDC     'dir'FAKE_CDC.DAT
$ DEFINE/NOLOG    MIN1_TRD     'dir'MIN1_TRD.DAT
$ DEFINE/NOLOG    MIN2_TRD     'dir'MIN2_TRD.DAT
$ DEFINE/NOLOG    MINI_CDC     'dir'MINI_CDC.DAT
$ DEFINE/NOLOG   CELOR_3CH_CDC 'dir'CELOR_MIN3CH_CDC_NOSMEAR_NEW1_241604.DAT
$ DEFINE/NOLOG    CELOR_3CH    'dir'CELOR_MIN3CH_DEDXCUT_CDC_NEW_241604.DAT
$!
$!  AZ stuff (put in April 10th 1996)
$!
$ DEFINE/NOLOG TRD_cor_temp  'dir'TRD_COR_TEMP.DAT
$ define/user_mode for003 nl:
$ run trwstp
$ delete/nolog trwstp.exe.*
$exit:
$ exit
