$!------------------------------------------------
$!
$! Name      : D0USER_HOOKS
$!
$! Purpose   : Create a combined package for the
$!             D0USER framework using the
$!             program builder.
$!
$! Arguments : P1       Name to be given to combined package file.
$!
$!             P2       List of packages (in quotes if more than one
$!                      delimited by commas).
$!            [P3]      List of Zebra switches:
$!                              /ZEBCOM=
$!                              /ZEBSTP=
$!                              /ZEBWRK=
$!                              /PAWC=
$!                      Use these switches to cause the PBD to produce
$!                      the corresponding INxxxx.FOR with specified
$!                      common block sizes.
$!
$! Created  19-Mar-1991   Harrison B. Prosper
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   WR          :== WRITE SYS$OUTPUT
$
$   Framework   = "D0USER"
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
$       zebcom = ""
$       zebstp = ""
$       pawc   = ""
$       WR "  "
$       WR " ZEBCOM [200000] "
$       WR " ZEBSTP [200000] "
$       WR " PAWC   [100000] "
$       WR "  "
$       INQUIRE answer " Increase common block sizes ? [N] "
$       IF answer .EQS. "Y"
$       THEN
$           INQUIRE zebcom " ZEBCOM [200000] "
$           INQUIRE zebstp " ZEBSTP [200000] "
$           INQUIRE pawc   " PAWC   [100000] "
$       ENDIF
$       IF F$INTEGER(zebcom) .GT. 200000
$       THEN
$           Zebra_sizes = Zebra_sizes + "/ZEBCOM=''zebcom'"
$       ENDIF
$       IF F$INTEGER(zebSTP) .GT. 200000
$       THEN
$           Zebra_sizes = Zebra_sizes + "/ZEBSTP=''zebstp'"
$       ENDIF
$       IF F$INTEGER(PAWC) .GT. 100000
$       THEN
$           Zebra_sizes = Zebra_sizes + "/PAWC=''pawc'"
$       ENDIF
$       IF Zebra_sizes .NES. ""
$       THEN
$            WR " Zebra_sizes : ''zebra_sizes'"
$       ENDIF
$       WR "  "
$   ENDIF
$
$   COMMAND = "PBD/FRAME=''Framework'/NAME=''Comb_name'"+ -
        "/PACKAGES=(''packages')''Zebra_sizes'"
$
$   WR "  "
$   WR " Working...please be patient!"
$   WR "  "
$   WR "$ ''COMMAND'"
$   WAIT 00:00:04
$!------------------------------------------------
$!
$!   Run PROGRAM-BUILDER
$!
$!------------------------------------------------
$
$   DEFINE/NOLOG D0$'Framework' [],D0$'Framework'$ROOT:[000000]
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
$   WR " List files created by PROGRAM BUILDER"
$   DIRECTORY/DATE/COLUMN=1/VERSION=1/SINCE="''A_while_ago'"  -
            *'comb_name'*.*
$   WR "  "
$
$   IF F$SEARCH("COMPILATION.TMP") .NES. ""
$   THEN
$       DELETE/NOCONFIRM COMPILATION.TMP;*
$   ENDIF
$   
$   WR " Please check log file PBD.LOG"
$   WR "  "
$
$EXIT:
$   EXIT
