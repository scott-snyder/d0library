$!========================================================================
$!
$! Name      : SETUP_MAC
$!
$! Purpose   : Sets up logicals and symbols to run
$! Mega_Autocompare_Histogram. Also copies the sample instruction file to
$! the default directory.
$!
$! Arguments : 
$!
$! Created  26-JAN-1994   R. J. Genik II
$! Modified 26-JAN-1994   R. J. Genik II to copy sample instruction file.
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT   
$   def_dir = f$environment("DEFAULT")
$   define MAC_cwd 'def_dir' ! as in "current working directory"
$   Define MAC_Instructions_RCP MAC_cwd:mac_instructions.rcp
$   MAC :== $d0$util:Mega_Autocompare_Histogram.exe
$   Show Symbol MAC
$   Show Logical MAC_CWD
$   Show Logical MAC_Instructions_RCP
$   If (f$length(F$search("MAC_Instructions_RCP")).EQ.0) 
$ Then 
$       Copy/Log d0$util$util:mac_instructions.rcp *
$       Type sys$input
    Sample RCP file copied. You will have to edit before running.
    When finished modifying RCP file,
$   Endif
$   Type sys$input
    Type "MAC" to do comparison. 

    Redefine MAC_Instructions_RCP, if needed, to point to the
    instruction file you wish to use.
    
$EXIT:
$   EXIT
