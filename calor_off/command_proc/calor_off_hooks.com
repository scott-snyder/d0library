$!------------------------------------------------
$!
$! Name      : CALOR_OFF_HOOKS
$!
$! Purpose   : Create a combined package for the
$!             CALOR_OFF framework using the
$!             program builder.
$!
$! Arguments : P1       Name to be given to combined package files:
$!
$!                      'P1'_CALOR_OFF.FOR (contains hooks).
$!                      'P1'_CALOR_OFF.OBJ.
$!                       DEB_'P1'_CALOR_OFF.OBJ.
$!                      'P1'_CALOR_OFF.LNK
$!
$!             P2       List of packages (in quotes if more than one
$!                      delimited by commas).
$!            [P3]      List of Zebra switches:
$!                              /ZEBCOM=
$!                              /ZEBSTP=
$!                              /ZEBWRK=
$!                              /GCBANK=
$!                              /PAWC=
$!                      Use these switches to cause the PBD to produce
$!                      the corresponding INxxxx.FOR with specified
$!                      common block sizes.
$!
$! Created  23-SEP-1990   Harrison B. Prosper
$! Modified 21-DEC-1990   Harrison B. Prosper
$!      Added GCBANK
$! Modified 14-NOV-1991   Boaz Klima 
$!      No SUMMARY file ( to be compatible with new PBD )
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   WR          :== WRITE SYS$OUTPUT
$
$   Framework   = "CALOR_OFF"
$   Required_package   = "CALOR"
$
$   Comb_name   = P1
$   Packages    = P2
$   Zebra_sizes = P3
$!------------------------------------------------
$!
$!   Get name of combined package
$!
$!------------------------------------------------
$   IF Comb_name .EQS. ""
$   THEN
$       INQUIRE Comb_name " Name of COMBINED package "
$   ENDIF
$   IF Comb_name .EQS. "" THEN GOTO EXIT
$!------------------------------------------------
$!
$!   Get list of packages to be combined
$!
$!------------------------------------------------
$
$   WR " NOTE: The package: ''Required_package' will be"
$   WR "       included automatically."
$   WR "  "
$   WR "       Use CAHITS to create the CAEP, CAEH and CATE banks.
$   WR "  "
$   IF Packages .EQS. ""
$   THEN
$       INQUIRE Packages " List all packages to be combined "
$   ENDIF
$   IF Packages .EQS. "" THEN GOTO EXIT
$!------------------------------------------------
$!
$!   Get zebra common block sizes
$!
$!------------------------------------------------
$
$   IF Zebra_sizes .EQS. ""
$   THEN
$       zebcom  = ""
$       zebstp  = ""
$       zebwrk  = ""
$       pawc    = ""
$       gcbank  = ""
$       zebcom1 = "1000000"
$       zebstp1 = "1000000"
$       zebwrk1 = "100"
$       pawc1   = "250000"
$       gcbank1 = ""
$       WR "  "
$       WR " ZEBCOM [''zebcom1'] "
$       WR " ZEBSTP [''zebstp1'] "
$       WR " ZEBWRK [''zebwrk1'] "
$       WR " PAWC   [''pawc1'] "
$       WR " GCBANK [''gcbank1'] "
$       WR "  "
$       
$       INQUIRE answer " Change common block sizes ? [N] "
$       IF answer .EQS. "Y"
$       THEN
$           INQUIRE zebcom " ZEBCOM [''zebcom1'] "
$           INQUIRE zebstp " ZEBSTP [''zebstp1'] "
$           INQUIRE zebwrk " ZEBWRK [''zebwrk1'] "
$           INQUIRE pawc   " PAWC   [''pawc1'] "
$           INQUIRE gcbank " GCBANK [''gcbank1'] "
$       ENDIF
$       
$       IF zebcom .EQS. ""
$       THEN
$           zebcom = zebcom1
$       ENDIF
$       IF zebstp .EQS. ""
$       THEN
$           zebstp = zebstp1
$       ENDIF
$       IF zebwrk .EQS. ""
$       THEN
$           zebwrk = zebwrk1
$       ENDIF
$       IF pawc   .EQS. ""
$       THEN
$           pawc   = pawc1
$       ENDIF
$       IF gcbank   .EQS. ""
$       THEN
$           gcbank   = gcbank1
$       ENDIF
$       
$       Zebra_sizes = Zebra_sizes + "/ZEBCOM=''zebcom'"
$       Zebra_sizes = Zebra_sizes + "/ZEBSTP=''zebstp'"
$       Zebra_sizes = Zebra_sizes + "/ZEBWRK=''zebwrk'"
$       Zebra_sizes = Zebra_sizes + "/PAWC=''pawc'"
$       IF gcbank .NES. ""
$       THEN
$           Zebra_sizes = Zebra_sizes + "/GCBANK=''gcbank'"
$       ENDIF
$       WR " Zebra_sizes : ''zebra_sizes'"
$       WR "  "
$   ENDIF
$
$   required_package = required_package + ","
$   COMMAND = "PBD/FRAME=''Framework'/NAME=''Comb_name'"+ -
        "/PACKAGES=(''required_package'''packages')''Zebra_sizes'"
$
$   IF F$SEARCH("[-]DEFINE.COM") .NES. ""
$   THEN
$       @[-]DEFINE "" "''P1'"  
$   ELSE
$       WRITE SYS$OUTPUT "  ---- NO [-]DEFINE.COM ----"
$   ENDIF

$   WR "  "
$   WR " Take a walk, I'm working..!"
$   WR "  "
$   WR "$ ''COMMAND'"
$!------------------------------------------------
$!
$!   Run PROGRAM-BUILDER
$!
$!------------------------------------------------
$
$   A_while_ago = F$TIME()
$
$   DEFINE/NOLOG SYS$OUTPUT PBD.LOG
$   ON CONTROL_Y THEN GOTO CLEANUP
$
$   'COMMAND'
$
$CLEANUP:
$   DEASSIGN SYS$OUTPUT
$
$   WR "  "
$   WR " Files created by PROGRAM BUILDER"
$   DIRECTORY/DATE/COLUMN=1/VERSION=1/SINCE="''A_while_ago'"  -
            *'comb_name'*.*
$   WR "  "
$
$   IF F$SEARCH("COMPILATION.TMP") .NES. ""
$   THEN
$       DELETE/NOCONFIRM COMPILATION.TMP;*
$   ENDIF
$
$   WR " Created PBD.LOG. "
$   WR "  "
$
$EXIT:
$   EXIT
