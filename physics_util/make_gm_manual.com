$!========================================================================
$!
$! Name      : MAKE_GM_MANUAL
$!
$! Purpose   : Make manual for GM program
$!
$! Arguments : None
$!
$! Created  23-FEB-1993   Harrison B. Prosper 
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   SET NOON
$   Name    = "GM"
$   Write Sys$Output "Creating file ''Name'.LNI"
$   
$   RUNOFF/INTERMEDIATE/NOOUTPUT 'Name'
$   RUNOFF/CONTENTS 'Name'
$   RUNOFF/BOLD=4/VARIANT=TOC/RIGHT=5 'Name'
$   RUNOFF/DEVICE=LN03/BOLD=4/VARIANT=TOC/RIGHT=5 'Name'
$   
$   IF F$SEARCH("''Name'.BRN") .NES. "" THEN -
	 DELETE/NOCONFIRM/LOG 'Name'.BRN;*
$   
$   IF F$SEARCH("''Name'.RNT") .NES. "" THEN -
	 DELETE/NOCONFIRM/LOG 'Name'.RNT;*
$EXIT:
$   EXIT
