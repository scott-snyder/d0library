$!========================================================================
$!
$! Name      : MAKE_TREE.COM
$!
$! Purpose   : make a call tree from a .lnk file
$!
$! Arguments : P1   name of the .LNK file
$!                  if P1 is the name of a .MAP file, the link is skipped
$!             P2   any chart2 qualifier (see CHART2.DOC)
$!                  NOLINES turn off line numbers in tree to help comparisons
$!
$! Created 18-OCT-1993   James T. Linnemann 
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$  P1 = f$edit(P1,"UPCASE")
$  file = P1 - ".LNK" - ".MAP"
$   IF (f$locate(".MAP",P1).EQ.f$length(P1))  
$   THEN
$       d0$beta_util:swap 'file'.lnk nomap "map/cross"
$       DELETE SWAP.log;0
$       @'file'.lnk
$   ENDIF
$   lines = ""
$   IF (P2.NES."") THEN lines = "/" + P2
$   CHART2 := $d0$util:CHART2.EXE
$   DEFINE/USER_MODE SYS$OUTPUT 'file'.TREE
$   CHART2/TRIM/NOHANG/TIGHT/NORTL'lines'/SHOWOPTIONS 'file'.MAP
$   IF (f$locate(".MAP",P1).EQ.f$length(P1))  
$   THEN
$       d0$beta_util:swap 'file'.lnk "map/cross" nomap
$       DELETE SWAP.log;0
$   ENDIF
$EXIT:
$   EXIT
