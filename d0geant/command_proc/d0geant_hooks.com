$!------------------------------------------------
$!
$! Name      : D0GEANT_HOOKS
$!
$! Purpose   : Create a combined package for the
$!             D0GEANT framework using the
$!             program builder.
$!
$! Arguments : P1       Name to be given to combined package files:
$!
$!                      'P1'_D0GEANT.FOR (contains hooks).
$!                      'P1'_D0GEANT.OBJ.
$!                       DEB_'P1'_D0GEANT.OBJ.
$!                      'P1'_D0GEANT.LNK
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
$   Framework   = "D0GEANT"
$   Required_package   = "D0"
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
$       zebcom1 = "1750000"
$       zebstp1 = "600000"
$       zebwrk1 = "20000"
$       pawc1   = "250000"
$       gcbank1 = "1500000"
$       WR "  "
$       WR " ZEBCOM [''zebcom1'] "
$       WR " ZEBSTP [''zebstp1'] "
$       WR " ZEBWRk [''zebwrk1'] "
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
$       Zebra_sizes = Zebra_sizes + "/GCBANK=''gcbank'"
$       WR " Zebra_sizes : ''zebra_sizes'"
$       WR "  "
$   ENDIF
$
$   required_package = required_package + ","
$   COMMAND = "PBD/FRAME=''Framework'/NAME=''Comb_name'"+ -
        "/PACKAGES=(''required_package'''packages')''Zebra_sizes'"
$
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
$   DEFINE D0$'Framework' [], -
                         D0$BETA:['Framework'],-
                         D0$BETA:['Framework'.PBD],-
                         D0$'Framework'$ROOT:[000000]
$   DEFINE D0$PBD         [], -
                         D0$PBD$ROOT:[000000]
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
$   WR " Check log file PBD.LOG for errors. "
$   WR "  "
$
$EXIT:
$   EXIT
