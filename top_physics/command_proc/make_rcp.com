$!========================================================================
$!
$! Name      : MAKE_RCP
$!
$! Purpose   : Make top_mass_rcp form template
$!
$! Arguments : 'p1' low limit of mass
$!             'p2' high limit of mass in integer form
$!             'p3' central top mass value being looked for
$!
$! Created  20-SEP-1993   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   IF p1 .eqs. ""
$   THEN
$   p1 = 100
$   ENDIF
$
$   IF P2 .eqs. ""
$   THEN
$   p2 = 250
$   ENDIF
$
$   IF P3 .eqs. ""
$   THEN
$   p3 = 140
$   ENDIF
$
$  XLO = "''p1'.0"
$  XHI1 = "''p2'.0"
$   m3 = 'P2'+1
$  XHI = "''m3'.0"
$copy top_mass_monte_template.rcp top_mass_monte_'p3'.rcp
$ swap top_mass_monte_'p3'.rcp "'mlo'"  "''xlo'"
$ swap top_mass_monte_'p3'.rcp "'mhi'"  "''xhi'"
$ swap top_mass_monte_'p3'.rcp "'mhi1'"  "''xhi1'"
$ swap top_mass_monte_'p3'.rcp "mlo"  'p1'
$ swap top_mass_monte_'p3'.rcp "mhi1"  'p2'
$ purge top_mass_monte_'p3'.rcp
$ purge swap.log
$EXIT:
$   EXIT
