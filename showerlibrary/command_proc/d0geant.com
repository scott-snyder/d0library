$ SET VERIFY
$!		GEANT.COM
$!
$!	RUN LOWPRI JOB - DZERO-GEANT
$! 
$SET DEFAULT USR$ROOT:[RAJA.SHOWERLIBRARY.D0GEANT]
$ASSIGN SYS$PRINT LOG
$write sys$print "Running D0GEANT WITH D0- SHOWERLIBRARY"
$@SETUP_D0GEANT
$ASSIGN  BAT'P1'.DAT FOR004
$dir *.exe
$RUN/NODEB DEB_D0GEANT
  READ 4
$SET VERIFY
$ NF=P1+1
$ NFMX=1
$ IF  NF .GT. NFMX   THEN GOTO  EXIT
$ SUBMIT/NOPRINTER/NOTIFY/QUEUE=D0SF13_BATCH/NAME=SHLB'NF'/PARAMETERS='NF' -
 D0GEANT
$EXIT:
