$ SET NOVERIFY
$!========================================================================
$! DBDETACHED_STARTUP.COM - Command File for Detached DBL3 Server
$!
$! DESCRIPTION
$!   This command file, which executes as a detached job, starts a DBL3
$!   server. The network name for the DBL3 server is extracted from the
$!   process name.
$!
$! ARGUMENTS: 
$!   None
$!
$! AUTHOR:
$!   J. Frederick Bartlett
$!   D0 Construction Department
$!   Research Division
$!   Fermilab
$!
$! CREATION DATE:
$!   29-Jul-1991
$!
$! MODIFIED   02-27-92    SHAHRIAR ABACHI     Modified from clock server
$!                                            command files to be used for
$!                                            DBL3 database servers to
$!                                            start a detached process.
$!
$!========================================================================
$ ON ERROR THEN GOTO EXIT
$ ON CONTROL_Y THEN GOTO EXIT
$!
$!      Define local symbols and logicals
$! SET VERIFY
$!
$ LOCAL_DIRECTORY = F$ENVIRONMENT("DEFAULT")
$ DEFINE/NOLOG SYS$LOGIN "''LOCAL_DIRECTORY'"
$ SAY == "WRITE SYS$OUTPUT"
$ CA_NODE = "D0HSA"
$ MU_NODE = "D0HSA"
$ CD_NODE = "D0HSA"
$ @D0$DISK:[D0LIBRARY]D0LOCAL
$ LIBTEST ALL
$!
$!	Extract the DBL3 calibration identifier from the process name
$!
$! DBSERV_ID = F$EXTRACT(0, 4, F$GETJPI("", "PRCNAM"))
$ DBSERV_ID = F$EXTRACT(0, 2, F$GETJPI("", "PRCNAM"))
$!
$ IF (DBSERV_ID .EQS. "CA") THEN -
   DBSERV_IDL = "CAL"
$ IF (DBSERV_ID .EQS. "MU") THEN -
   DBSERV_IDL = "MUO"
$ IF (DBSERV_ID .EQS. "CD") THEN -
   DBSERV_IDL = "CDC"
$ IF (DBSERV_ID .NES. "CA") .AND. (DBSERV_ID .NES. "MU") .AND. -
       (DBSERV_ID .NES. "CD")
$ THEN
$   SAY " Server identifier ''DBSERV_ID' is unknown"
$   GOTO EXIT
$ ENDIF
$!
$!	Define the server logical names
$!
$ DEFINE/NOLOG DBL3$CLIENT "''DBSERV_ID'"
$ DATABASE = "DBCALIB$''DBSERV_IDL'"
$ DEFINE/NOLOG 'DATABASE' "D0DB$''DBSERV_IDL':DBCALIB$''DBSERV_IDL'.DAT"
$!
$!	Start the server
$!
$ RUN DBSERV$'DBSERV_IDL':'DBSERV_ID'_DBSERVER.EXE
$EXIT:
$   EXIT
