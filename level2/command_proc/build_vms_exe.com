$!========================================================================
$! Name      : BUILD_STANDARD_EXE
$!           << COM file to be run by release procedure >>
$!
$! Purpose   : To build an official "standard"  VMS_FILTER.EXE that
$!             can be copied and used by users running  SETUP_L2SIM.
$!             This package includes  CALOR,CADMAKE,RERUN_L12,L1SIM
$!             and RECDDN (runs only for MC). CAHITS removed 11-FEB.
$!             Also builds  VMS_FILTER_STP  and generates STP files.
$!
$! Note      : VMS_FILTER_PARAMETERS, VMS_FILTER_INIT created, by hand, thru
$!             run of FILTER_MAKER,  and reflect the most current L2TOOL.DAT.
$!             These  FOR  files are placed in the CMS group L2SIM, compiled
$!             upon release (see below), with resulting  OBJs  kept in L2SIM.
$!
$! Arguments : p1       = EXE(s) [def]  build new EXEs only
$!                      = STP(s)        build only new STP.EXE and STPs
$!                      = ALL           build a full set of EXEs and new STPs
$!
$! Created   6-JAN-1992   Daniel R. Claes
$!           4-JAN-1993   DRC  -  generalized for PRODUCTION area/BETA area
$!                                as well as official and GAMMA releases
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$   SAY :== WRITE SYS$OUTPUT
$   DELETE_WHEN_DONE = "NO"
$   SDIR = F$ENVIRONMENT("DEFAULT") ! Starting directory
$
$   @D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP SKIP
$
$   IF (p1.EQS."") 
$   THEN
$       p1 = "EXES"
$   ENDIF
$
$   WORKDIR = F$TRNLNM("D0$LEVEL2$L2SIM")
$   IF WORKDIR.EQS."PROD$L2PROD"
$   THEN
$       WORKDIR = F$TRNLNM("PROD$L2PROD")   ! capture the leading string in
$       DESTdir = F$TRNLNM("PROD$L2PROD")   ! the search list (usually L2$NEW)
$   ELSE
$       WORKDIR = F$TRNLNM("D0$LEVEL2$ROOT") - ".]" + "]"
$       DESTdir = F$TRNLNM("D0$LEVEL2$ROOT") - "]" + "L2SIM]"
$   ENDIF
$
$   SET DEFAULT 'DESTdir'
$
$   OBJtest = F$SEARCH("VMS_FILTER_PARAMETERS.FOR")
$   IF OBJtest.EQS.""
$   THEN
$       GOTO NO_FILTER_CODE
$   ENDIF
$!
$   OBJtest = F$SEARCH("VMS_FILTER_PARAMETERS.OBJ")
$   IF OBJtest.EQS.""
$   THEN
$       GOTO COMPILE_FILTER_CODE
$   ENDIF
$
$   T0 = F$FILE_ATTRIBUTES("VMS_FILTER_PARAMETERS.FOR","CDT")
$   T0 = F$CVTIME(T0)
$   T1 = F$FILE_ATTRIBUTES("VMS_FILTER_PARAMETERS.OBJ","CDT")
$   T1 = F$CVTIME(T1)
$   IF (T1.GTS.T0)
$   THEN
$       GOTO NO_FILTER_CODE ! OBJ newer than FOR
$   ENDIF
$COMPILE_FILTER_CODE:
$   SAY "L2NEW carries new uncompiled FILTER routines."
$   SAY "Will compile VMS_FILTER_PARAMETERS and _INIT."
$
$   @D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP SKIP   ! Already LIBT'ed in SETUP
$
$       FORT VMS_FILTER_PARAMETERS.FOR
$       FORT/NOOPT/DEBUG=ALL/OBJ=DEB_VMS_FILTER_PARAMETERS.OBJ  -
            VMS_FILTER_PARAMETERS.FOR
$       FORT VMS_FILTER_INIT.FOR
$       FORT/NOOPT/DEBUG=ALL/OBJ=DEB_VMS_FILTER_INIT.OBJ VMS_FILTER_INIT.FOR
$
$NO_FILTER_CODE:
$
$   SET DEFAULT 'WORKdir'
$
$   DIRtest = F$SEARCH("TEMP.DIR")
$   IF DIRtest.EQS."" 
$   THEN 
$      CREATE/DIRECTORY [.TEMP]
$      DELETE_WHEN_DONE = "YES"
$   ENDIF
$   SET DEFAULT [.TEMP]
$   @D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP SKIP
$
$   IF (p1.EQS."STP").OR.(p1.EQS."STPS").OR.(p2.EQS."STP").OR.(p2.EQS."STPS")
$   THEN
$       GOTO STP_ONLY
$   ENDIF
$!$   PBD/FRAME=D0USER/PACK=GRAND_FSUM-
$!       /NAME=GRAND_FSUM/COMPILE/ZEBCOM=800000/ZEBSTP=700000
$!$   @GRAND_FSUM_D0USER.LNK
$!$   RENAME GRAND_FSUM_D0USER.EXE [-]
$!$!   @GRAND_FSUM_D0USER.LNK DEBUG
$!$!  RENAME GRAND_FSUM_DEB__D0USER.EXE [-]DEB_GRAND_FSUM.EXE
$!$   DELETE GRAND_FSUM_D0USER*.*;*
$
$   COPY D0$LEVEL2$L2SIM:D0_FILTER.OPT *
$   COPY D0$LEVEL2$L2SIM:DEB_D0_FILTER.OPT *
$   @D0$LEVEL2$COMMAND_PROC:DEFINE_L2_PASS
$
$   SAY "Building and compiling HSTRFL routine"
$   PBD/FRAME=HSTRFL/NAME=VMS_FILTER/HSTRFL/PROD=2/PASS='L2_PASS_NUM' -
        /VER='L2_VERSION_NUM'/NOCOMPILE
$   FOR VMS_FILTER_HSTRFL.FOR
$   FOR/DEBUG=ALL/NOOPT/OBJ=DEB_VMS_FILTER_HSTRFL VMS_FILTER_HSTRFL.FOR
$   SAY "Building VMS_FILTER package"
$   PBD/FRAME=D0USER/PACK=L12SIM%/NAME=VMS_FILTER
$! (This combined package (%) sets ZEBCOM=800000/ZEBSTP=1200000
$! (                    as well as ZEBWRK=40000/PAWC=200000 )
$!   PBD/FRAME=D0USER/PACK=(CALOR,RERUN_L12,L1SIM,RECDDN,VMS_FILTER)-
$!       /NAME=VMS_FILTER/COMPILE/ZEBCOM=800000/ZEBSTP=1200000
$!       /ZEBWRK=40000/PAWC=200000/NOCOMPILE
$   FOR/OBJ=TEMP VMS_FILTER_D0USER
$! get standard PRODUC function so can switch off packages, latest HSTRFL to mark link
$   LIB/EXTRACT=PRODUC/OUTPUT=PRODUC D0$GENERAL:GENERAL.OLB
$   COPY/CONCAT PRODUC.OBJ, -
        VMS_FILTER_HSTRFL.OBJ, -
        FILTER_DEFAULT:PASS_RELEASE_SIM.OBJ, -
        TEMP.OBJ    VMS_FILTER_D0USER.OBJ
$   FOR/DEBUG=ALL/NOOPT/OBJ=DEB_TEMP VMS_FILTER_D0USER
$   LIB/EXTRACT=PRODUC/OUTPUT=DEB_PRODUC D0$GENERAL:DEB_GENERAL.OLB
$   COPY/CONCAT DEB_PRODUC.OBJ, -
        DEB_VMS_FILTER_HSTRFL.OBJ, -
        FILTER_DEFAULT:DEB_PASS_RELEASE_SIM.OBJ, -
        DEB_TEMP.OBJ    DEB_VMS_FILTER_D0USER.OBJ
$   DELETE TEMP.OBJ;*,PRODUC.OBJ;*,DEB_TEMP.OBJ;*,DEB_PRODUC.OBJ;*
$   DELETE VMS_FILTER_HSTRFL.OBJ;*,DEB_VMS_FILTER_HSTRFL.OBJ;*
$
$   COPY D0$LEVEL2$COMMAND_PROC:VMS_FILTER_D0USER.COM *
$   @VMS_FILTER_D0USER.COM "" "" "" DEFAULT
$   @VMS_FILTER_D0USER.LNK
$   RENAME VMS_FILTER_D0USER.EXE 'DESTdir'
$   @VMS_FILTER_D0USER.LNK DEBUG
$   RENAME VMS_FILTER_DEB_D0USER.EXE 'DESTdir'
$   RENAME VMS_FILTER_D0USER.FOR 'DESTdir'
$   RENAME VMS_FILTER_D0USER.OBJ 'DESTdir'
$   RENAME DEB_VMS_FILTER_D0USER.OBJ 'DESTdir'
$
$   IF (p1.EQS."EXE").OR.(p1.EQS."EXES").OR.(p2.EQS."EXE").OR.(p2.EQS."EXES")
$   THEN
$       GOTO EXE_ONLY
$   ENDIF
$
$STP_ONLY:
$   @D0$LEVEL2$COMMAND_PROC:BUILD_STPS.COM
$EXE_ONLY:
$   IF DELETE_WHEN_DONE.EQS."YES"
$   THEN
$     DELETE/NOCONFIRM *.*;*
$     SET DEFAULT [-]
$     SET FILE/PROTECTION=(O:RWED) TEMP.DIR
$     SET ACL/DEFAULT TEMP.DIR
$     DELETE/NOCONFIRM TEMP.DIR;*
$   ENDIF
$
$EXIT:
$   d0$status :== TRUE
$   SET DEFAULT 'sdir'
$   EXIT
$ERROR_EXIT:
$CONTY_EXIT:
$   d0$status :== FALSE
$   SET DEFAULT 'sdir'
$   EXIT
