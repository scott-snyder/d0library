#! /bin/csh -f
#------------------------------------------------
#
# Name      : MUGSTP
#
# Purpose   : Create and run FULL_D0GEANT.EXE to create
#             STP file MURECO_GSAVE.DAT
#
# Arguments : None
#
# Created      S. ABACHI    09-MAR-1992
#
#------------------------------------------------
#
eval `uff $d0d0geant/command_proc/d0geant_setup.csh`
eval `uff $d0d0geant/command_proc/d0geant_lnk.csh`
#
#  Note that his command file uses nonstandard switches,
#  including SD0(3) is introduced and set to 1.
#
ln -sf mureco_gsave314.dat gsave.dat >& /dev/null
d0geant.x << END
LIST
SD0  0.0 0.0 1.0
DCEN 0
DCAL 1
DMUO 1
DSAM 1
DLV0 0
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
0
QUIT
END
#
rm *.metafile >& /dev/null
rm full_d0geant, dzero.o fort.* >& /dev/null
rm RUNSAVE >& /dev/null
