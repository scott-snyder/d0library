$ SET NOVERIFY
$!-----------------------------------------------------------------------------
$! DBREMOTE_STARTUP.COM - Remote Command File for Starting a Detached DBL3
$!                      Server on the Target Node
$!
$! DESCRIPTION:
$! This command file starts a DBL3 server as a detached process on the local
$! node. It is executed as a remote process and communicates with the
$! initiating process over the network connection device SYS$NET by which it
$! recieves strings which specify the DBL3 entity, the name of the person
$! executing the restart, and the reason for the restart.
$!
$! AUTHOR:
$!   J. Frederick Bartlett
$!   D0 Construction Department
$!   Research Division
$!   Fermilab
$!
$! CREATION DATE:
$!   26-Jul-1991
$!
$! MODIFIED   02-27-1992     SHAHRIAR ABACHI    Modified from clock server
$!                                              command files to be used for
$!                                              DBL3 severs to start
$!                                              detached processes..
$!
$!-----------------------------------------------------------------------------
$!
$!	Set local conditions
$!
$! SET VERIFY
$ DEFINE/NOLOG NETSERVER$TIMEOUT "0 0:0:10"
$ LOCAL_DIRECTORY = F$ENVIRONMENT("DEFAULT")
$ TODAY = F$TIME()
$ SAY = "WRITE NETFILE"
$ ON ERROR     THEN GOTO EXIT
$ ON CONTROL_Y THEN GOTO EXIT
$ set noverify
$! @D0$DISK:[D0LIBRARY]D0LOCAL.COM
$! LIBTEST DBL3
$! set verify
$!
$!      Define procedure parameters
$!
$ KEEP_COUNT = 4                         !File purge limit
$ COMMAND_FILE = -
   "ONLINE:[DBL3]DBDETACHED_STARTUP.COM   !Command file for the detached process
$!
$!	Define the server node symbols
$!
$ CA_NODE = "D0HSA"
$ CA_UIC = "[1600,100]"
$ MU_NODE = "D0HSA"
$ MU_UIC = "[1600,100]"
$ CD_NODE = "D0HSA"
$ CD_UIC = "[1600,100]"
$!
$!	Define the detached job's parameters
$!
$ SERV_PRIO = "6"
$ SERV_PRIV = "(PRMMBX, TMPMBX, NETMBX, SYSNAM)"
$!
$!	Open the network connection and get the startup parameters
$!
$ OPEN/READ/WRITE NETFILE SYS$NET
$ READ/END_OF_FILE=NO_ID NETFILE DBSERV_ID
$ READ/END_OF_FILE=NO_NAME NETFILE USERNAME
  READ/END_OF_FILE=NO_REASON NETFILE REASON
$!
$!	Verify server identifier
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
$ DEFINE/NOLOG DBL3$CLIENT 'DBSERV_ID'
$!
$ RESTART_LOG_FILE = -
   "DBSERV$''DBSERV_IDL':"+"''DBSERV_ID'_DBSERV_RESTART.LOG" !Restart log file
$!
$!	Define identifier-dependant symbols
$!
$ SERVER_NAME = "''DBSERV_ID'_DBSERVER"
$ TARGET_NODE = 'DBSERV_ID'_NODE
$ PROCESS_NAME = "''DBSERV_ID'_DBSERVER"
$ PROCESS_UIC = 'DBSERV_ID'_UIC
$ PROCESS_LOG = "DBSERV$''DBSERV_IDL':''DBSERV_ID'_DBSERVER.LOG"
$ PROCESS_ERR = "DBSERV$''DBSERV_IDL':''DBSERV_ID'_DBSERVER.ERR"
$!
$!	Verify target node
$!
$ IF F$GETSYI("NODENAME") .NES. TARGET_NODE
$ THEN
$  SAY " This process must be executed from node ''TARGET_NODE'"
$  GOTO EXIT
$ ENDIF
$!
$!	Check that the necessary logicals are defined
$!
$! IF (F$TRNLNM("DBSERV$''DBSERV_IDL'") .EQS. "") .OR. -
!        (F$TRNLNM("D0DB$''DBSERV_IDL'") .EQS. "") .OR. -
!         (F$TRNLNM("DBMCOM$''DBSERV_IDL'") .EQS. "") .OR. -
!         (F$TRNLNM("DBSPOOL$''DBSERV_IDL'") .EQS. "") .OR. -
!         (F$TRNLNM("DBL3$CLIENT") .EQS. "") .OR. -
!         (F$TRNLNM("DBCALIB$''DBSERV_IDL'") .EQS. "")
$!  THEN
$!   SAY " Some logical names are not defined"
$!   GOTO EXIT
$!  ENDIF
$!
$!	Expand the file names
$!
$ LOG_FILE = F$PARSE("''PROCESS_LOG'",,,, "NO_CONCEAL, SYNTAX_ONLY")
$ ERR_FILE = F$PARSE("''PROCESS_ERR'",,,, "NO_CONCEAL, SYNTAX_ONLY")
$ COM_FILE = F$PARSE("''COMMAND_FILE'",,,, "NO_CONCEAL, SYNTAX_ONLY")
$!
$!	Check that the detached process command file exists
$!
$ DBSERV_DIR = "dbserv$''dbserv_idl':"
$ dir 'DBSERV_DIR'
$ IF F$SEARCH(COM_FILE) .EQS. "" THEN GOTO NO_FILE
$!
$!	Stop an existing server
$!
$ CNTX = ""
$ NULL = F$CONTEXT("PROCESS", CNTX, "PRCNAM", "''PROCESS_NAME'", "EQL")
$ PID = F$PID(CNTX)
$ IF PID .NES. ""
$ THEN
$  STOP/ID='PID'
$  SAY "Stopping existing process ''PROCESS_NAME'"
$ ENDIF
$ IF F$TYPE(CNTX) .EQS. "PROCESS_CONTEXT" THEN -
   NULL = F$CONTEXT("PROCESS", CNTX, "CANCEL")
$!
$!	Purge log files
$!
$ IF F$SEARCH("NETSERVER.LOG") .NES. "" THEN PURGE/KEEP='KEEP_COUNT' -
   NETSERVER.LOG
$ IF F$SEARCH(PROCESS_LOG) .NES. "" THEN PURGE/KEEP='KEEP_COUNT' 'PROCESS_LOG'
$ IF F$SEARCH(PROCESS_ERR) .NES. "" THEN PURGE/KEEP='KEEP_COUNT' 'PROCESS_ERR'
$!
$!	Start the detached process
$!
$ RUN SYS$SYSTEM:LOGINOUT -
	/UIC='PROCESS_UIC' -
        /INPUT='COM_FILE' -
	/OUTPUT='LOG_FILE' -
	/ERROR='ERR_FILE' -
	/PROCESS_NAME='PROCESS_NAME' -
        /BUFFER_LIMIT=75000 -
	/ENQ=700 -
	/PRIORITY='SERV_PRIO' -
	/PRIVILEGES='SERV_PRIV'
$!
$!	Enter user name and reason in the startup log file
$!
$ IF F$SEARCH(RESTART_LOG_FILE) .EQS. ""
$ THEN
$   OPEN/WRITE LOGFILE 'RESTART_LOG_FILE'
$   WRITE LOGFILE "File ''RESTART_LOG_FILE' created on ''TODAY'"
$   WRITE LOGFILE ""
$ ELSE
$   OPEN/APPEND LOGFILE 'RESTART_LOG_FILE'
$ ENDIF
$ WRITE LOGFILE -
   "Restart ''DBSERV_ID' by ''USERNAME' on ''TODAY', reason - ''REASON'"
$ CLOSE LOGFILE

$ SAY "Detached process ''PROCESS_NAME' started"
$ GOTO EXIT
$!
$!	Error logging section
$!
$NO_ID:
$ SAY "No server identifier supplied"
$ GOTO EXIT
$NO_NAME:
$ SAY "No user name supplied"
$ GOTO EXIT
$NO_REASON:
$ SAY "No restart reason supplied"
$ GOTO EXIT
$NO_FILE:
$ SAY "Command file ''COMMAND_FILE' missing"
$ GOTO EXIT
$!
$!	Procedure exit
$!
$EXIT:
$ CLOSE NETFILE
$ EXIT
