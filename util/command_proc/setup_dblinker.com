$!========================================================================
$!
$! Name      : SETUP_DBLINKER
$!
$! Purpose   : Setup symbols and logicals for the DBLINKER program
$!
$! Arguments : None
$!
$! Created            6-MAY-1993     Harrison B. Prosper
$! Filled/Modified    26-July-1993   Homer A. Neal
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   WR :== WRITE SYS$OUTPUT
$
$!================================================
$!   Setup Symbols/Logicals for the Production
$!   Database
$!================================================
$   DBLINK*ER    :== RALLY RUN D0$BETA:[DBLINKER]DBLINK_PT5
$   WR "  "
$   WR " Type DBLINKER to start the Database Linker Program"
$   WR "  "
$!
$!================================================
$!
$!  INSERT HAN LOGICALS FOR RALLY APPLICATION
$!
$ libtest util 
$ libtest prod_db
$ d0setup pdb
$! define dbfile dbl3$rsm:dbrunsum.dat
$ define rally$3gl_dir d0$beta:[dblinker]
$ assign rally$3gl_dir:rally$3gl_prog1.exe rally$3gl_lib1
$ assign rally$3gl_dir:rally$3gl_prog2.exe rally$3gl_lib2
$ assign rally$3gl_dir:rally$3gl_prog2a.exe rally$3gl_lib2a
$ assign rally$3gl_dir:rally$3gl_xy_beam_spawn.exe rally$3gl_lib3
$ assign d0hs09::user1:[captain.luminosity]never_delete_me.dat -
       luminosity_summary
$ assign d0hs09::dsp3:[dbl3.lum.andor]andor_db.dat andor_db
$ assign rally$3gl_dir:rally$3gl_run_sum_han.exe rally$3gl_lib4
$ assign rally$3gl_dir:rally$3gl_pdb_util_spawn.exe rally$3gl_lib5
$ define dex$shrlib d0$beta:[dblinker]dex$shrlib
$ define dex$dsl$shr d0$beta:[dblinker]dex$dsl$shr
$ define dex$ism_client_shr d0$beta:[dblinker]dex$ism_client_shr
$ setup fortran.v6
$ assign rally$3gl_dir:eve_open.exe eve$open_lib
$ sdf:== $d0$beta:[dblinker]show_ddif_file
$ assign rally$3gl_dir:shar_pix_display.exe pix$display_lib
$!================================================
$!   Setup logicals for DBRUNSUM
$!================================================
$   DEFINE/NOLOG    DBFILE  DBL3$RSM:DBRUNSUM.DAT
$
$EXIT:
$   EXIT
