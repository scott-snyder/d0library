$!========================================================================
$! Name      : BUILD_OBJs
$!             A small piece extracted from BUILD_VMS_EXE
$!             The full use of BUILD_VMS_EXE requires release
$!             of too many other libraries for now.
$!
$! Note      : VMS_FILTER_PARAMETERS, VMS_FILTER_INIT created, by hand, thru
$!             run of FILTER_MAKER,  and reflect the most current L2TOOL.DAT.
$!             These  FOR  files are placed in the CMS group L2SIM, compiled
$!             upon release (see below), with resulting  OBJs  kept in L2SIM.
$!
$! Arguments : p1 = SKIP will call  L2LIB_SETUP,  but perform no LIBTESTing
$!                  (option required when executing under release procedure)
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$   SAY :== WRITE SYS$OUTPUT
$
$ WORKDIR = F$TRNLNM("D0$LEVEL2$L2SIM")
$ IF WORKDIR.EQS."PROD$L2PROD"
$ THEN
$   WORKDIR = F$TRNLNM("PROD$L2PROD")
$ ELSE
$   WORKDIR = F$TRNLNM("D0$LEVEL2$ROOT") - "]" + "L2SIM]"
$ ENDIF
$
$   sdir = f$environment("DEFAULT")
$   SET DEFAULT 'WORKDIR' 
$       
$   @D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP 'p1'
$
$   OBJtest = F$SEARCH("PASS_RELEASE_SIM.OBJ")
$   IF OBJtest.EQS.""
$   THEN
$       OBJtest = F$SEARCH("PASS_RELEASE_SIM.FOR")
$       IF OBJtest.NES.""
$       THEN
$           FORT PASS_RELEASE_SIM.FOR
$           FORT/NOOPT/DEBUG=ALL/OBJ=DEB_PASS_RELEASE_SIM.OBJ  -
                PASS_RELEASE_SIM.FOR
$       ENDIF
$   ENDIF
$
$   FORtest = F$SEARCH("VMS_FILTER_PARAMETERS.FOR")
$   IF FORtest.NES.""
$   THEN
$       OBJtest = F$SEARCH("VMS_FILTER_PARAMETERS.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT VMS_FILTER_PARAMETERS.FOR
$       ENDIF
$
$       OBJtest = F$SEARCH("DEB_VMS_FILTER_PARAMETERS.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT/NOOPT/DEBUG=ALL/OBJ=DEB_VMS_FILTER_PARAMETERS.OBJ  -
            VMS_FILTER_PARAMETERS.FOR
$       ENDIF
$   ENDIF
$
$   FORtest = F$SEARCH("VMS_FILTER_INIT.FOR")
$   IF FORtest.NES.""
$   THEN
$       OBJtest = F$SEARCH("VMS_FILTER_INIT.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT VMS_FILTER_INIT.FOR
$       ENDIF
$
$       OBJtest = F$SEARCH("DEB_VMS_FILTER_INIT.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT/NOOPT/DEBUG=ALL/OBJ=DEB_VMS_FILTER_INIT.OBJ -
            VMS_FILTER_INIT.FOR
$       ENDIF
$   ENDIF
$
$   FORtest = F$SEARCH("VMS_FILTER_STP.FOR")
$   IF FORtest.NES.""
$   THEN
$       OBJtest = F$SEARCH("VMS_FILTER_STP.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT VMS_FILTER_STP.FOR
$       ENDIF
$
$       OBJtest = F$SEARCH("DEB_VMS_FILTER_STP.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT/NOOPT/DEBUG=ALL/OBJ=DEB_VMS_FILTER_STP.OBJ -
            VMS_FILTER_STP.FOR
$       ENDIF
$   ENDIF
$
$   FORtest = F$SEARCH("VMS_FILTER_D0USER.FOR")
$   IF FORtest.NES.""
$   THEN
$       OBJtest = F$SEARCH("VMS_FILTER_D0USER.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT VMS_FILTER_D0USER.FOR
$       ENDIF
$
$       OBJtest = F$SEARCH("DEB_VMS_FILTER_D0USER.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT/DEB=ALL/NOOPT/OBJ=DEB_VMS_FILTER_D0USER.OBJ -
            VMS_FILTER_D0USER.FOR
$       ENDIF
$   ENDIF
$
$   OBJtest = F$SEARCH("VMS_FILTER_HSTRFL.FOR")
$   IF OBJtest.NES.""
$   THEN
$       OBJtest = F$SEARCH("VMS_FILTER_HSTRFL.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT VMS_FILTER_HSTRFL.FOR
$       ENDIF
$
$       OBJtest = F$SEARCH("DEB_VMS_FILTER_HSTRFL.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT/NOOPT/DEBUG=ALL/OBJ=DEB_VMS_FILTER_HSTRFL.OBJ  -
                VMS_FILTER_HSTRFL.FOR
$       ENDIF
$   ENDIF
$
$   OBJtest = F$SEARCH("VMS_STP_HSTRFL.FOR")
$   IF OBJtest.NES.""
$   THEN
$       OBJtest = F$SEARCH("VMS_STP_HSTRFL.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT VMS_STP_HSTRFL.FOR
$       ENDIF
$
$       OBJtest = F$SEARCH("DEB_VMS_STP_HSTRFL.OBJ")
$       IF OBJtest.EQS.""
$       THEN
$           FORT/NOOPT/DEBUG=ALL/OBJ=DEB_VMS_STP_HSTRFL.OBJ  -
                VMS_STP_HSTRFL.FOR
$       ENDIF
$   ENDIF
$
$EXIT:
$   d0$status :== TRUE
$   set default 'sdir'
$   EXIT
$ERROR_EXIT:
$CONTY_EXIT:
$   d0$status :== FALSE
$   set default 'sdir'
$   EXIT
