$!========================================================================
$!
$! Name      : SETUP
$!
$! Purpose   : Setup some PIXIE symbols/logicals
$!
$! Arguments :
$!
$! Created  12-SEP-1990   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   DEFINE/NOLOG FOR003         NL:
$   DEFINE/NOLOG PXBUILD_RCP    D0$PIXIE:PXBUILD.RCP
$
$   PXB*UILD            :== $D0$PIXIE:PXBUILD/FULL
$   MAKE_P*BD           :== @D0$PIXIE:PIXIE_PBD
$   MAKE_H*OOKS         :== @D0$PIXIE:PIXIE_HOOKS
$   SETUP_PIX*IE        :== @D0$PIXIE:SETUP_PIXIE 
$   
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT " PIXIE commands "
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT " MAKE_PBD         Create a PIXIE .PBD file"
$   WRITE SYS$OUTPUT " MAKE_HOOKS       Create hooks for PIXIE"
$   WRITE SYS$OUTPUT " SETUP_PIXIE      @d0$PIXIE:SETUP_PIXIE"
$   WRITE SYS$OUTPUT " PXBUILD          Run PXBUILD program"
$   WRITE SYS$OUTPUT "  "
$EXIT:
$   EXIT
