$!========================================================================
$!
$! Name      : LOCAL_PASS  [p1] 
$!
$! Purpose   : Concatenate compiled local code to user's private copy of
$!             VMS_FILTER/L1SIM_D0USER.OBJ to override the PASS_RELEASE_SIM.OBJ
$!             modules already copied into it.  This is to allow development
$!             work on code already pass-released under L2$NEW.
$!
$! Arguments : p1 = VMS(_FILTER)     For linking VMS_FILTER packages.
$!                = ELN              For linking ELN .EXEs
$!                = STP              For linking STP .EXEs
$!                = pkg              For linking other pkg_D0USER .EXE
$!                                      eg pkg = L1SIM or L2COMPARE
$!
$! Created  23-APR-1993   Daniel Claes  
$!          14-OCT-1993   Daniel Claes - Identify and compile all C code
$! Modified  3-MAR-1994   James T. Linnemann - compile everything but drop
$!                          selective compilation; support more .EXEs
$!          11-APR-1994   Daniel Claes - patch DEB OBJ building, add CONVERT
$!                                       to allow FOR/C objs to concatenate
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   SAY :== WRITE SYS$OUTPUT
$   say "*** Start LOCAL_PASS ''p1' ***"
$   DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$!
$   IF (p1.EQS."") THEN p1 = "VMS_FILTER"
$   IF (p1.EQS."VMS") THEN p1 = "VMS_FILTER"
$   IF (p1.EQS."VMS_FILTER")
$   THEN
$       SAY "Building and compiling HSTRFL routine"
$       @d0$level2$command_proc:define_l2_pass.com
$       pbd/frame=hstrfl/name=vms_filter/hstrfl/prod=2/pass='l2_pass_num'-
            /ver='l2_version_num'/nocompile
$   ENDIF
$! 
$! select the pass release file to base on, and to build
$!
$   pass_obj = "pass_release_sim.obj"
$   IF (p1.EQS."ELN") THEN pass_obj = "pass_release_node.obj"
$   IF (p1.EQS."STP") THEN pass_obj = "pass_release_stp.obj"
$   target_obj = pass_obj
$   IF (pass_obj.EQS."pass_release_sim.obj") THEN target_obj = p1 +"_D0USER.OBJ"
$   pass_obj = f$search("D0$LEVEL2$L2SIM:''pass_obj'")
$!
$! compile everything found locally
$! the result will be some "multiply defined's" if you make a local copy of a
$! routine in pass release
$!
$   PIPE FORTRAN/OBJ=.OB  *.FOR,*.FOR_*
$   PIPE FORTRAN/NOOPT/DEBUG=ALL/NOLIS/OBJ=.OD *.FOR,*.FOR_*
$   PIPE CC/OBJ=.OB  *.C
$   PIPE CC/NOOPT/DEBUG/NOLIS/OBJ=.OD  *.C
$!
$   say " "
$   IF (pass_obj.NES."")  
$   THEN 
$       say "Starting from ''pass_obj'"
$       pass_obj = "," + pass_obj
$   ENDIF
$   say "Making new ''target_obj's "
$   PIPE CONVERT/FDL=D0$LEVEL2$COMMAND_PROC:L2PROD_OB.FDL *.OB *.OB
$   COPY/CONCAT *.OB'pass_obj' 'target_obj'
$   DELETE/NOCONFIRM *.OB;*
$   IF (pass_obj.NES."")  
$   THEN
$       END = F$LENGTH(pass_obj) - 1
$       TEST = F$LOCATE("]",pass_obj)
$       IF (TEST.NES.END)
$       THEN
$           FRONT =  F$EXTRACT(0,TEST+1,pass_obj)
$           BACK  =  F$EXTRACT(TEST+1,END,pass_obj)
$           pass_obj = FRONT + "DEB_" + BACK
$       ENDIF
$   ENDIF
$   PIPE CONVERT/FDL=D0$LEVEL2$COMMAND_PROC:L2PROD_OB.FDL *.OD *.OD
$   COPY/CONCAT *.OD'pass_obj' DEB_'target_obj'
$   DELETE/NOCONFIRM *.OD;*
$!
$EXIT:
$   say "*** End LOCAL_PASS ''p1' ***"
$   EXIT
