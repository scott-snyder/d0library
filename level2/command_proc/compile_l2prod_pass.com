$!========================================================================
$!
$! Name      : COMPILE_L2PROD_PASS
$!
$! Purpose   : compile a pass release and combine into pass .obj
$!
$! Arguments :
$!
$! Created   7-DEC-1992   James T. Linnemann
$! Modified 12-DEC-1992   James T. Linnemann separate node and VMS .OBJ's
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   WRITE SYS$OUTPUT "*** Start  COMPILE_L2PROD_PASS ***"
$!?$   SET DEFAULT D0$PROD:[D0PRODUCTION.L2PROD.NEW]
$!?$   @DEFINE_L2_PASS.COM
$   if f$search("*.ob") .nes. "" then DELETE/NOLOG *.OB;*
$
$!================================================
$!   Create PASS_RELEASE_NODE.OBJ
$!================================================
$   @d0$level2$command_proc:build_pipe_files.com L2nod_code
$   if f$search("l2nod_code_for.lis") .nes. "" then -
        PIPER FORTRAN/NOLIS/OBJ=.OB l2nod_code_for.lis
$   if f$search("l2nod_code_pas.lis") .nes. "" then -
        PIPER PASCAL/NOLIS/OBJ=.OB l2nod_code_pas.lis
$   if f$search("l2nod_code_epas.lis") .nes. "" then -
        PIPER EPASCAL/NOLIS/OBJ=.OB l2nod_code_epas.lis
$   if f$search("l2nod_code_c.lis") .nes. "" then -
        PIPER CC/NOLIS/OBJ=.OB l2nod_code_c.lis
$
$   IF f$search("*.OB") .nes. ""
$   THEN
$       pipe convert/fdl=d0$level2$command_proc:l2prod_ob.fdl *.ob *.ob
$       COPY/CONCAT *.OB PASS_RELEASE_NODE.OBJ
$       delete/nolog/noconf *.ob;*
$   ELSE
$       copy nl: pass_release_node.obj
$       convert/fdl=d0$level2$command_proc:l2prod_ob.fdl -
            pass_release_node.obj pass_release_node.obj
$   ENDIF
$
$!================================================
$!   Create DEB_PASS_RELEASE_NODE.OBJ
$!================================================
$   if f$search("l2nod_code_for.lis") .nes. "" then -
        PIPER FORTRAN/DEB=ALL/NOOPT/NOLIS/OBJ=.OB l2nod_code_for.lis
$   if f$search("l2nod_code_pas.lis") .nes. "" then -
        PIPER PASCAL/DEB/NOOPT/NOLIS/OBJ=.OB l2nod_code_pas.lis
$   if f$search("l2nod_code_epas.lis") .nes. "" then -
        PIPER EPASCAL/DEB/NOOPT/NOLIS/OBJ=.OB l2nod_code_epas.lis
$   if f$search("l2nod_code_c.lis") .nes. "" then -
        PIPER CC/DEB/NOOPT/NOLIS/OBJ=.OB l2nod_code_c.lis
$
$   IF f$search("*.OB") .nes. ""
$   THEN
$       pipe convert/fdl=d0$level2$command_proc:l2prod_ob.fdl *.ob *.ob
$       COPY/CONCAT *.OB DEB_PASS_RELEASE_NODE.OBJ
$       delete/nolog/noconf *.ob;*
$   ELSE
$       copy nl: deb_pass_release_node.obj
$       convert/fdl=d0$level2$command_proc:l2prod_ob.fdl -
            deb_pass_release_node.obj deb_pass_release_node.obj
$   ENDIF
$! save for now $   if f$search("l2nod_code*.lis") .nes. "" then -
$! save for now         delete/nolog/noconf l2nod_code*.lis;*
$
$!================================================
$!   Create PASS_RELEASE_SIM.OBJ
$!================================================
$   @d0$level2$command_proc:build_pipe_files.com L2sim_code
$   if f$search("l2sim_code_for.lis") .nes. "" then -
        PIPER FORTRAN/NOLIS/OBJ=.OB l2sim_code_for.lis
$   if f$search("l2sim_code_pas.lis") .nes. "" then -
        PIPER PASCAL/NOLIS/OBJ=.OB l2sim_code_pas.lis
$   if f$search("l2sim_code_epas.lis") .nes. "" then -
        PIPER EPASCAL/NOLIS/OBJ=.OB l2sim_code_epas.lis
$   if f$search("l2sim_code_c.lis") .nes. "" then -
        PIPER CC/NOLIS/OBJ=.OB l2sim_code_c.lis
$
$   IF f$search("*.OB") .nes. ""
$   THEN
$       pipe convert/fdl=d0$level2$command_proc:l2prod_ob.fdl *.ob *.ob
$       COPY/CONCAT *.OB PASS_RELEASE_SIM.OBJ
$       delete/nolog/noconf *.ob;*
$   ELSE
$       copy nl: pass_release_sim.obj
$       convert/fdl=d0$level2$command_proc:l2prod_ob.fdl -
            pass_release_sim.obj pass_release_sim.obj
$   ENDIF
$
$!================================================
$!   Create DEB_PASS_RELEASE_SIM.OBJ
$!================================================
$   if f$search("l2sim_code_for.lis") .nes. "" then -
        PIPER FORTRAN/DEB=ALL/NOOPT/NOLIS/OBJ=.OB l2sim_code_for.lis
$   if f$search("l2sim_code_pas.lis") .nes. "" then -
        PIPER PASCAL/DEB/NOOPT/NOLIS/OBJ=.OB l2sim_code_pas.lis
$   if f$search("l2sim_code_epas.lis") .nes. "" then -
        PIPER EPASCAL/DEB/NOOPT/NOLIS/OBJ=.OB l2sim_code_epas.lis
$   if f$search("l2sim_code_c.lis") .nes. "" then -
        PIPER CC/DEB/NOOPT/NOLIS/OBJ=.OB l2sim_code_c.lis
$
$   IF f$search("*.OB") .nes. ""
$   THEN
$       pipe convert/fdl=d0$level2$command_proc:l2prod_ob.fdl *.ob *.ob
$       COPY/CONCAT *.OB DEB_PASS_RELEASE_SIM.OBJ
$       delete/nolog/noconf *.ob;*
$   ELSE
$       copy nl: deb_pass_release_sim.obj
$       convert/fdl=d0$level2$command_proc:l2prod_ob.fdl -
            deb_pass_release_sim.obj deb_pass_release_sim.obj
$   ENDIF
$! save for now $   if f$search("l2sim_code*.lis") .nes. "" then -
$! save for now         delete/nolog/noconf l2sim_code*.lis;*
$
$!================================================
$!   Create PASS_RELEASE_STP.OBJ
$!================================================
$   @d0$level2$command_proc:build_pipe_files.com L2stp_code
$   if f$search("l2stp_code_for.lis") .nes. "" then -
        PIPER FORTRAN/NOLIS/OBJ=.OB l2stp_code_for.lis
$   if f$search("l2stp_code_pas.lis") .nes. "" then -
        PIPER PASCAL/NOLIS/OBJ=.OB l2stp_code_pas.lis
$   if f$search("l2stp_code_epas.lis") .nes. "" then -
        PIPER EPASCAL/NOLIS/OBJ=.OB l2stp_code_epas.lis
$   if f$search("l2stp_code_c.lis") .nes. "" then -
        PIPER CC/NOLIS/OBJ=.OB l2stp_code_c.lis
$
$   IF f$search("*.OB") .nes. ""
$   THEN
$       pipe convert/fdl=d0$level2$command_proc:l2prod_ob.fdl *.ob *.ob
$       COPY/CONCAT *.OB PASS_RELEASE_STP.OBJ
$       delete/nolog/noconf *.ob;*
$   ELSE
$       copy nl: pass_release_stp.obj
$       convert/fdl=d0$level2$command_proc:l2prod_ob.fdl -
            pass_release_stp.obj pass_release_stp.obj
$   ENDIF
$
$!================================================
$!   Create DEB_PASS_RELEASE_STP.OBJ
$!================================================
$   if f$search("l2stp_code_for.lis") .nes. "" then -
        PIPER FORTRAN/DEB=ALL/NOOPT/NOLIS/OBJ=.OB l2stp_code_for.lis
$   if f$search("l2stp_code_pas.lis") .nes. "" then -
        PIPER PASCAL/DEB/NOOPT/NOLIS/OBJ=.OB l2stp_code_pas.lis
$   if f$search("l2stp_code_epas.lis") .nes. "" then -
        PIPER EPASCAL/DEB/NOOPT/NOLIS/OBJ=.OB l2stp_code_epas.lis
$   if f$search("l2stp_code_c.lis") .nes. "" then -
        PIPER CC/DEB/NOOPT/NOLIS/OBJ=.OB l2stp_code_c.lis
$
$   IF f$search("*.OB") .nes. ""
$   THEN
$       pipe convert/fdl=d0$level2$command_proc:l2prod_ob.fdl *.ob *.ob
$       COPY/CONCAT *.OB DEB_PASS_RELEASE_STP.OBJ
$       delete/nolog/noconf *.ob;*
$   ELSE
$       copy nl: deb_pass_release_stp.obj
$       convert/fdl=d0$level2$command_proc:l2prod_ob.fdl -
            deb_pass_release_stp.obj deb_pass_release_stp.obj
$   ENDIF
$! save for now $   if f$search("l2stp_code*.lis") .nes. "" then -
$! save for now         delete/nolog/noconf l2stp_code*.lis;*
$
$EXIT:
$   if f$search("*.ob") .nes. "" then DELETE/NOLOG *.OB;*
$   EXIT
