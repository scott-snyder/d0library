$!------------------------------------------------
$!
$! Name      : CALPBD
$!
$! Purpose   : Create a combined package for the
$!             CALOR_OFF framework using the
$!             program builder.
$!
$! Arguments : P1       Name to be given to combined package files:
$!
$!                      'P1'_CALOR_OFF.FOR (contains hooks).
$!                      'P1'_CALOR_OFF.OBJ.
$!                      DEB_'P1'_CALOR_OFF.OBJ.
$!                      'P1'_COMBINED_PACKAGE.SUM.
$!                      'P1'_COMBINED_PACKAGE.PBD.
$!
$!             P2       List of packages (in quotes if more one
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
$! Created  11-DEC-1989   Harrison B. Prosper
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   WR          :== WRITE SYS$OUTPUT
$
$   Framework   = "CALOR_OFF"
$   Required_packages   = "CALOR,CAHITS"
$
$   Comb_name   = P1
$   Packages    = P2
$   Zebra_sizes = P3
$
$   IF Zebra_sizes .EQS. ""
$   THEN
$       Zebra_sizes = "/ZEBCOM=1000000/ZEBSTP=600000/PAWC=200000"
$   ENDIF
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
$       WR "  "
$       IF Required_packages .NES. ""
$       THEN
$           WR " NOTE: The packages: ''Required_packages'"
$           WR "       will be included automatically."
$       ENDIF
$       INQUIRE Packages " List of other packages to be ADDED "
$   ENDIF
$   IF Packages .EQS. "" THEN GOTO EXIT
$!------------------------------------------------
$!
$!   Run PROGRAM-BUILDER
$!
$!------------------------------------------------
$
$   DEFINE/NOLOG D0$'Framework' [],D0$CMS:['Framework'], -
                                D0$'Framework'$ROOT:[000000]
$
$   IF Required_packages .NES. "" 
$   THEN
$       PBD/FRAME='Framework'/NAME='Comb_name'/SUMMARY  -
        'Required_packages','packages''Zebra_sizes'
Y
$   ELSE
$       PBD/FRAME='Framework'/NAME='Comb_name'/SUMMARY  -
        'packages''Zebra_sizes'
Y
$   ENDIF
$   WR "  "
$   WR "  "
$   WR "Files generated:"
$   WR "  "
$   WR "        ''comb_name'_''Framework'.FOR"
$   WR "        ''comb_name'_''Framework'.OBJ"
$   WR "        DEB_''comb_name'_''Framework'.OBJ"
$   WR "        ''comb_name'_''Framework'.SUM"
$   WR "  "
$EXIT:
$   EXIT
