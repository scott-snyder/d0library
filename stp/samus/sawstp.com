$!========================================================================
$!
$! Name      : SAWSTP.COM
$!
$! Purpose   : Creates the SAM_STPFILE data file for SAMUS processing
$!
$! Arguments : None
$!
$! Created  28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
$! Modified  5-MAY-1991   Vladimir Glebov : Update to RCP files
$! Modified 23-FEB-1994   Alexander Efimov  for Geom data base
$! Modified 28-MAR-1995   Denisov - new survey data - March 1995
$! Mofified 13-NOV-1995   Denisov - new survey data - September 1995
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$!
$   IF p1 .eqs. "DEBUG"
$   THEN
$       swit = "/deb"
$       deb_ = "DEB_"
$       dbnk = "d0$physics_util:physics_util.olb/include=(dbank)/lib,"
$   ELSE
$       swit = ""
$       deb_ = ""
$       dbnk = ""
$   ENDIF
$   WRITE SYS$OUTPUT "    Linking ''deb_'SAMUS/STP"
$   LINK/nomap'swit'/exe='deb_'SAWSTP -
        d0$stp:'deb_'samus.olb/lib/include=sawstp,'dbnk' -
        d0$muon_util:'deb_'muon_util.olb/lib, -
        d0$compack:'deb_'COMPACK.olb/lib, -
        D0$GENERAL:'deb_'GENERAL.olb/LIB, -
        'cernp'
$   IF p1 .nes. "DEBUG"
$   THEN
$       WRITE SYS$OUTPUT "    Running SAMUS/STP image"
$       @d0$stp$samus:setup_sawstp
$       RUN SAWSTP
$       @d0$stp$samus:setup_sawstp _29JAN93
$       RUN SAWSTP
$       @d0$stp$samus:setup_sawstp _02DEC93
$       RUN SAWSTP
$       @d0$stp$samus:setup_sawstp _25FEB94
$       RUN SAWSTP
$       @d0$stp$samus:setup_sawstp _28MAR95
$       RUN SAWSTP
$       @d0$stp$samus:setup_sawstp _07NOV95
$       RUN SAWSTP
$       DELETE/NOLOG sawstp.exe;*
$       delete/nolog for*.dat;*
$   ELSE
$       WRITE SYS$OUTPUT "    Will not run DEB_SAWSTP image"
$   ENDIF
$EXIT:
$   EXIT
