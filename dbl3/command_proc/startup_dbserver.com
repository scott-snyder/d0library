$ SET NOVERIFY
$!-----------------------------------------------------------------------------
$! STARTUP_DBSERVER.COM - Local Command File for Starting a Detached DBL3 Server
$!
$! DESCRIPTION:
$!   This command file starts a DBL3 server as a detached process. It executes
$!   a remote process to create the detached process and, so, requires knowledge
$!   of the password to the CALIB account. The procedure accepts four
$!   parameters which, if absent from the command line, are prompted for entry.
$!
$!   The command file arguments are:
$!     P1 = DBL3 calibration selector string (CA, MU, CD, ...)
$!     P2 = Password for CALIB account
$!     P3 = User's name
$!     P4 = Reason for restart
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
$! MODIFIED     02-27-92    SHAHRIAR ABACHI     Modified from clock server
$!                                              command files to be used for
$!                                              DBL3 database servers to 
$!                                              start a detached process.
$!
$!-----------------------------------------------------------------------------
$!
$!	Set local conditions
$!
$! SET VERIFY
$ SAY = "WRITE SYS$OUTPUT"
$ ON ERROR THEN GOTO EXIT
$ ON CONTROL_Y THEN GOTO EXIT
$!
$!	Set query field sizes
$!
$ R_FIELD_SIZE = 40
$ R_FIELD_MARK[0, 4] := "|<--"
$ R_FIELD_MARK[R_FIELD_SIZE - 4, 4] := "-->|"
$ N_FIELD_SIZE = 12
$ N_FIELD_MARK[0, 4] := "|<--"
$ N_FIELD_MARK[N_FIELD_SIZE - 4, 4] := "-->|"
$!
$!	Define the server nodes
$!
$ CA_NODE = "D0HSA"
$ MU_NODE = "D0HSA"
$ CD_NODE = "D0HSA"
$!
$!	Check the server name argument (P1)
$!
$ IF P1 .EQS. "" THEN -
   INQUIRE P1 "	Which server (CA, MU, CD)?"
$ IF (P1 .NES. "CA") .AND. (P1 .NES. "MU") .AND. (P1 .NES. "CD")
$ THEN
$  SAY " Server selection parameter ''P1' is undefined"
$  GOTO EXIT
$ ENDIF
$!
$ IF (P1 .EQS. "CA") THEN -
   DBSERV_IDL = "CAL"
$ IF (P1 .EQS. "MU") THEN -
   DBSERV_IDL = "MUO"
$ IF (P1 .EQS. "CD") THEN -
   DBSERV_IDL = "CDC"
$!
$!	Request the server account password
$!
$ IF P2 .NES. ""
$ THEN
$  PASSWORD = P2
$ ELSE
$  SET TERMINAL/NOECHO
$  INQUIRE PASSWORD "Enter password"
$  SET TERMINAL/ECHO
$ ENDIF
$!
$!	Request the user's name
$!
$ IF P3 .NES. ""
$ THEN
$  USERNAME = P3
$ ELSE
$  SAY "Enter your last name (maximum ''N_FIELD_SIZE' characters)"
$  SAY N_FIELD_MARK
$  INQUIRE/NOPUNCTUATION USERNAME ""
$ ENDIF
$ USERNAME = F$EXTRACT(0, N_FIELD_SIZE - 1, USERNAME)
$!
$!	Request the reason for restarting
$!
$ IF P4 .NES. ""
$ THEN
$  REASON = P4
$ ELSE
$  SAY "Enter reason for restart (maximum ''R_FIELD_SIZE' characters)"
$  SAY R_FIELD_MARK
$  READ/PROMPT="" SYS$COMMAND REASON
$ ENDIF
$ REASON = F$EDIT(F$EXTRACT(0, R_FIELD_SIZE - 1, REASON), "COMPRESS, TRIM")
$!
$!	Define string variables
$!	
$ SERVER_NAME = "''P1'_DBSERVER"
$ TARGET_NODE = 'P1'_NODE
$ TARGET = "''TARGET_NODE'""CALIB ''PASSWORD'""::" -
           + """TASK=DBREMOTE_STARTUP"""
$!           + "DBSERV$''DBSERV_IDL':" -
$!
$!	Start a login process under the CALIB account on the target node
$!
$ SAY " Starting the ''SERVER_NAME' server on node ''TARGET_NODE'"
$ OPEN/READ/WRITE/ERROR=OPEN_ERROR NET 'TARGET'
$ WRITE/ERROR=WRITE_ERROR NET P1
$ WRITE/ERROR=WRITE_ERROR NET USERNAME
$ WRITE/ERROR=WRITE_ERROR NET REASON
$READ_LINE:
$ READ/END_OF_FILE=CLOSE_FILE NET REPLY
$ WRITE SYS$OUTPUT REPLY
$ GOTO READ_LINE
$CLOSE_FILE:
$ CLOSE NET
$ GOTO EXIT
$!
$!	Error exits
$!
$OPEN_ERROR:
$ SAY "Remote connection open error"
$ GOTO EXIT
$WRITE_ERROR:
$ SAY "Remote connection write error"
$ CLOSE NET
$ GOTO EXIT
$!
$!	Procedure exit
$!
$EXIT:
$ EXIT
