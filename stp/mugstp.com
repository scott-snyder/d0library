$!------------------------------------------------
$!
$! Name      : MUGSTP
$!
$! Purpose   : Create and run FULL_D0GEANT.EXE to create
$!             STP file MURECO_GSAVE.DAT
$!
$! Arguments : None
$!
$! Created      S. ABACHI    09-MAR-1992
$!
$!------------------------------------------------
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$   wr := write sys$output
$
$   group := muon
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
$   @D0$D0GEANT:D0GEANT_SETUP     ! Make standard assignments
$   @D0$D0GEANT:FULL_D0GEANT.LNK
$!
$!  Note that his command file uses nonstandard switches,
$!  including SD0(3) is introduced and set to 1.
$!
$   RUN FULL_D0GEANT
0
LIST
SD0  0.0 0.0 1.0
DLV0 0
DCEN 0
DCAL 1
DMUO 1
DSAM 1
PAIR 0
COMP 2
PHOT 2
BREM 0
MUNU 0
LOSS 2
MULS 1
CUTS .00005,.00005,.002,.002,.002,.00005,.00005,10000.,10000.,.005,1.E10
SAVE 'INIT'
STOP
QUIT
$!
$ purge/noconfirm  GSAVE.DAT
$ rename GSAVE.DAT  MURECO_GSAVE314.DAT
$ purge/noconfirm  MURECO_GSAVE314.DAT
$ delete/noconfirm *.METAFILE;*
$ delete/noconfirm FULL_D0GEANT.EXE;*
$!
$ EXIT:
$   EXIT
