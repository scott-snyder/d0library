$!========================================================================
$!
$! Name      : Default.com
$!
$! Purpose   : Streamlined version which sets up Alpha and Beta area
$!             deafults
$!
$! Arguments : P1 = Beta area name (e.g. D0Geant)
$!             In this case The beta area will be set to D0$beta:['p1']
$!             P2 = alpha area name (e.g. d0geant.upgrade), in this case
$!             The alpha area will be set to sys$login_device:{username.'p2')
$!             If P2 is blank, P2 = P1
$!             P3 = blank, root area = sys$login_device:
$!             Otherise, root area is "P3:"
$!             P4 if =1, will not run Setup file from Alpha area.
$!             Setup file name is taken to be Setup_'P1'.com
$!
$! Created   4-JAN-1994   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!================================================
$!   Extracting the User name
$!================================================
$A=F$USER()
$!SHOW SYMBOL A
$I1=F$LOCATE(",",A)+1
$!SHOW SYMBOL L1
$I2=F$LOCATE("]",A)
$!SHOW SYMBOL I2
$I3=I2-I1
$!SHOW SYMBOL I3
$P0=F$EXTRACT(I1,I3,A)
$!SHOW SYMBOL P0
$
$
$   IF P1 .eqs. ""
$   THEN
$    Inquire p1 " Beta area name? "
$   ENDIF
$        IF P2.eqs.""
$        THEN
$          P2 = "''p1'"
$        ENDIF
$
$        IF P3.eqs.""
$        THEN
$          PP4  = "sys$login_device:[''P0'.''p2']"
$        ELSE
$          PP4 =  "''p3':[''p2']"
$        ENDIF
$
$        PP5  = "''p1'"
$
$DOIT:  
$
$ set default 'PP4'
$ define alpha 'pp4'
$ define beta  d0$beta:['pp5']
$ define/key/nolog f20 "set default ''pp4'" /terminate/echo
$ define/key/nolog f19 "set default d0$beta:[''pp5']" /terminate/echo
$ cms_library :== 'pp5'.cms         !default cms library for ccen
$ cms set library d0$beta:['pp5'.cms]
$ write sys$output "    set default to ''pp4'"
$ write sys$output "    defined f19 to set default d0$beta:[''pp5']"
$ write sys$output "    defined f20 to set default ''pp4'"
$ write sys$output "    defined the symbol cms_library to ''pp5'.cms"
$ write sys$output -
 "    logical alpha has been defined to ''pp4' area"
$ write sys$output -
 "    logical beta has been defined to d0$beta:''pp5' area"
$
$ IF P4 .eqs. ""
$ THEN
$   file = "setup_''p2'.com"
$   file1=f$search(file)
$     IF file1 .eqs. ""
$     THEN
$       inquire file1 "Setup File Name?"
$     ELSE
$     ENDIF
$   Write sys$output -
    "    SETUP command file ''file1'" 
    write sys$output  -
    "    being executed"
$   @'file1'
$ ELSE
$   Write sys$output -
    "    No  SETUP command file will be executed"
$ ENDIF
$EXIT:
